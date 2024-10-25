#' Returns the final TCAC data frame output
#'
#'
#' @description
#' `final_raw` loads and combines all intermediate files with economic, education, and environmental indicators.
#' `final_prepare` creates the high poverty and segregated designation and flags unreliable data.
#' `final_opp` creates the final opportunity scores and designations.  `final_raw` and `final_prepare` are both
#' inputs into `final_opp`.  Only `final_opp` is necessary to run for generating new data.
#'
#' @note
#' 2025 implements a 3-year rolling average of education indicators.  The change is enacted in the final_raw function.
#'
#'
#' @param year designates the map year's filepaths
#' @param geo allows for opportunity to be assessed at the tract level in urban areas and block group in rural areas
#' @param write write the final output
#' @param output `reduced` returns only variables necessary for the map.  If `full`, include all intermediate variables.
#' @param cog logical to write COG referenced shapefile for HCD to be used in housing element update process.
#'
#'
#' @return a data frame
#'
#'
#' @examples
#' final_opp(year = 2024, write = TRUE) # writes the final output
#'
#'
#' @import dplyr
#' @import sf
#' @import rlang
#'
#'
#' @export
final_opp <- function(year = current_year, write = FALSE, reduced = TRUE, cog  = FALSE){

  if(cog==FALSE){
    # load tracts and bgs separately for standard scoring
    urban <- final_prepare(year, geo='tract') %>% dplyr::filter(region != 'Rural Areas')
    rural <- final_prepare(year, geo='bg') %>% dplyr::filter(region == 'Rural Areas')
    final <- dplyr::bind_rows(urban, rural) %>%
      mutate(regionid = ifelse(region == "Rural Areas", county_name, region))
  } else {
    # load only tracts for cog-referenced map
    filepaths()
    final <- final_prepare(year, geo='tract') %>% select(-region)
    cog_region <-  read_zip(county_cog_xwalk, year, col_types = readr::cols())
    final <- final %>% left_join(cog_region, by = "county_name") %>%
      mutate(regionid = ifelse(region == "Rural Areas", county_name, region))

  }

  # rearrange cols
  final <- final %>%
    select(dplyr::contains('fips'), region, regionid, county_name, # geographic information
           pct_above_200_pov, home_value, pct_bachelors_plus, pct_employed, # economic
           math_prof, read_prof, grad_rate, pct_not_frpm, ## educational
           env_site_score, # environmental
           everything()
    )


  # store indicator cols in a vector
  econ_cols <- c('pct_above_200_pov', 'home_value', 'pct_bachelors_plus', 'pct_employed')
  ed_cols <- c('math_prof', 'read_prof', 'grad_rate', 'pct_not_frpm')
  ind_cols <- c(econ_cols, ed_cols)

  # create regional median scores and values
  final <- final %>%
    group_by(regionid) %>%
    # create above/below median scores
    mutate_at(vars(!!!syms(ind_cols)), list(score = function(x) ifelse(x >= median(x, na.rm = TRUE), 1, 0))) %>%
    # save regional median values for charts
    mutate_at(vars(!!!syms(ind_cols)), list(median = ~median(., na.rm = TRUE))) %>%
    ungroup()


  # store score cols in a vector
  score_cols <- c('pct_above_200_pov_score', 'home_value_score', 'pct_bachelors_plus_score',
                  'pct_employed_score', 'math_prof_score', 'read_prof_score',
                  'grad_rate_score', 'pct_not_frpm_score')

  # make indicator scores null where there are less than 2 obs within the reference region
  final <- final %>%
    group_by(regionid) %>%
    mutate_at(vars(!!!syms(score_cols)), function(x) case_when(sum(!is.na(x)) < 2 ~ NA_real_, TRUE ~ x)) %>%
    ungroup()

  # count the number of non-null values for each indicator within the refernce region
  final <- final %>%
    group_by(regionid) %>%
    mutate_at(vars(!!!syms(ind_cols)), list(valid = function(x) sum(!is.na(x)))) %>%
    ungroup()


  # loop through ind cols and invalidate medians with less than 2 valid observations
  for (col in ind_cols) {
    median_col <- paste0(col, "_median")
    valid_col <- paste0(col, "_valid")
    final[[median_col]][which(final[[valid_col]] < 2)] <- NA
  }


  # calculate total non-null indicator scores for each tract/bg
  final <- final %>%
    mutate(total_valid = rowSums(
      transmute_at(., vars(!!!syms(score_cols), env_site_score),
                   function(x) ifelse(!is.na(x), 1, 0))))


  # calculate opportunity score
  final <- final %>%
    mutate(oppscore = rowSums(select(., all_of(score_cols), env_site_score), na.rm = TRUE),
           # invalidate scores with more than 2 missing values
           oppscore = ifelse(total_valid >= 7, oppscore, NA))


  # invalidate scores with density, military, or instituionalized pop flags
  final$oppscore[which(final$instit_flag == 1 | final$military_flag == 1 |
                         final$density_flag == 1)] <- NA

  # load native land flag and apply hps invalidation
  final <- final %>%
    left_join(tribal_overlap(), by = 'fips')
  final$pov_seg_flag[which(final$instit_flag == 1 | final$military_flag == 1 |
                             final$density_flag == 1 | final$nativeland_flag == 1)] <- NA

  # add single exclusion flag
  final <- final %>%
    mutate(exclude_flag =
             case_when(
               density_flag == 1 ~ 1,
               instit_flag == 1 ~ 1,
               military_flag == 1 ~ 1,
               TRUE ~ 0
             ))



  # create opportunity categories
  final <- final %>%
    mutate(oppcat = case_when(
      oppscore >= 8 ~ "Highest Resource",
      oppscore >= 6 ~ "High Resource",
      oppscore >= 4 ~ "Moderate Resource",
      oppscore < 4 ~ "Low Resource",
      TRUE ~ NA_character_))
  # factor
  levels <- c("Highest Resource", "High Resource", "Moderate Resource", "Low Resource")
  final$oppcat <- factor(final$oppcat, levels = levels)


  # if cog is true, write file for cog-referenced map
  if(cog == TRUE){

    final_geo <- final %>% left_join(shape_CA_tract, by = c('fips', 'county_name')) %>% sf::st_as_sf() %>% sf::st_set_crs(4326)

    final_geo <- final_geo %>% select('FIPS' = 'fips', `Opportunity Category` = 'oppcat', `Opportunity Score` = 'oppscore',
                                      `Share > 200% Poverty Score` = 'pct_above_200_pov_score',
                                      `Share with Bachelors+ Score` = 'pct_bachelors_plus_score',
                                      `Employment Rate Score` = 'pct_employed_score',
                                      `Median Home Value Score` = 'home_value_score',
                                      `Share Proficient in Math Score` = 'math_prof_score',
                                      `Share Proficient in Reading Score` = 'read_prof_score',
                                      `High School Grad Rate Score` = 'grad_rate_score',
                                      `Students not in Poverty Score` = 'pct_not_frpm_score',
                                      `Environmental Score` = 'env_site_score',
                                      `High-Poverty & Segregated` = 'pov_seg_flag',
                                      `County Name` = 'county_name',
                                      `Region Code` = 'regionid')

    # remove the gpkg file if it exists, then write
    file_name <- paste0("output/", year, "/final_cog_", year, '.gpkg')
    if(file.exists(file_name)){
      file.remove(file_name)
    }
    sf::st_write(final_geo, file_name)

  # otherwise continue processing for standard map
  } else {

    # add additional variables needed for side panel
    final <- final %>%
      mutate(high_pov_thresh = .3,
             college_flag = if_else(pct_college >= .25, 1, 0))


    # join neighborhood change of non-rural tracts
    change <- read_neighborhood_change(year = year)
    final <- final %>% left_join(change, by = 'fips')

    # save cols for selection
    dem_cols <- c('pct_below_pov', 'pct_asian', 'pct_black',
                  'pct_hispanic', 'pct_poc')
    opp_cols <- c(ind_cols, 'env_site_score', dem_cols)
    change_cols <- colnames(change[, !names(change) %in% c("fips")])
    chart_cols <- c(opp_cols, change_cols)

    # apply exclusion flag to all chart variables
    final <- final %>%
      mutate_at(.vars = chart_cols,
                          # also exclude Alpine, which only has 1 block group
                          funs(ifelse(exclude_flag == 1 | county_name == 'Alpine', NA, .)))



    # attach geometries
    urban <- final %>% filter(is.na(fips_bg))
    rural <- final %>% filter(!is.na(fips_bg))

    urban_geo <- urban %>% left_join(shape_CA_tract, by = c('fips', 'county_name')) %>% sf::st_as_sf() %>% sf::st_set_crs(4326)
    rural_geo <- rural %>% left_join(shape_CA_bg, by = c('fips_bg')) %>% sf::st_as_sf() %>% sf::st_set_crs(4326)
    final_geo <- dplyr::bind_rows(urban_geo, rural_geo)


    # if reduced is false return the full data frame with geos
    if(reduced == FALSE){
      if(write == TRUE){
        # remove the geojson file if it exists
        file_name <- paste0("output/", year, "/final_full_", year, '.geojson')
        if(file.exists(file_name)){
          file.remove(file_name)
        }
        sf::st_write(final_geo, file_name)
      }else{
        return(final_geo)
      }

    # otherwise reduce to vars necessary for mapping interface
    } else {
      final_geo <- final_geo %>%
        select(
          # geo
          starts_with('fips'), region, regionid, county_name,
          # basic population
          total_pop,
          pop_density,
          ends_with('flag'),
          # opp indicators
          ind_cols,
          ends_with('median'),
          contains('score'),
          total_valid,
          # pov and seg indicators
          dem_cols, high_pov_thresh, starts_with('lq_'), ends_with('seg_thresh'),
          # designations
          oppcat, pov_seg_flag,
          # neighborhood change
          all_of(change_cols))

      # limit digits to 4 to minimize file size
      final_geo <- final_geo %>% mutate_if(is.numeric,
                                   round,
                                   digits = 4)

      if(write == TRUE){
        # remove the geojson file if it exists
        file_name <- paste0("output/", year, "/final_", year, '.geojson')
        if(file.exists(file_name)){
          file.remove(file_name)
        }
        sf::st_write(final_geo, file_name)
      }else{
        return(final_geo)
      }
    }
  }
}


# everything below this line is an input to the final_opp function
#' @export
#' @rdname final_opp
final_raw <- function(year = current_year, geo = 'tract'){

  # read intermediate files
  acs_census <- all_census_data(year = year, geo = geo)
  ces <- xwalk_ces(year = year)

  # use 3-year rolling education averages starting in 2025
  if (year >= 2025){
    # load education measures over last 3 years
    ed_inds <- school_distances(year = year, geo = geo)
    ed_inds_yr_minus_1 <- school_distances(year = year-1, geo = geo)
    ed_inds_yr_minus_2 <- school_distances(year = year-2, geo = geo)

    # calculate the 3-year average
    average_read_prof <- rowMeans(cbind(ed_inds$read_prof, ed_inds_yr_minus_1$read_prof, ed_inds_yr_minus_2$read_prof), na.rm = TRUE)
    average_math_prof <- rowMeans(cbind(ed_inds$math_prof, ed_inds_yr_minus_1$math_prof, ed_inds_yr_minus_2$math_prof), na.rm = TRUE)
    average_grad_rate <- rowMeans(cbind(ed_inds$grad_rate, ed_inds_yr_minus_1$grad_rate, ed_inds_yr_minus_2$grad_rate), na.rm = TRUE)
    average_pct_not_frpm <- rowMeans(cbind(ed_inds$pct_not_frpm, ed_inds_yr_minus_1$pct_not_frpm, ed_inds_yr_minus_2$pct_not_frpm), na.rm = TRUE)

    fips_col <- ed_inds[1]

    # create a new dataframe of the 3-year education averages
    education_indicators <- data.frame(
      fips_col,
      read_prof = average_read_prof,
      math_prof = average_math_prof,
      grad_rate = average_grad_rate,
      pct_not_frpm = average_pct_not_frpm
    )
  # otherwise use single year for education measures
  } else {
    education_indicators <- school_distances(year = year, geo = geo)
  }


  #join all data
  suppressMessages({
    rawdata <- dplyr::full_join(education_indicators, acs_census) %>%
      dplyr::left_join(ces)

  })


  #remove year suffixes from ACS variable names
  acs_year <- year-3
  rawdata <- rawdata %>% dplyr::rename_with(~stringr::str_remove(., paste0('_', acs_year)))

  return(rawdata)
}



#' @export
#' @rdname final_opp
final_prepare <- function(year = current_year, geo = 'tract', .data=NULL){

  if(is.null(.data)){
    #adding in the bases for location quotients (tract to county ratios)
    final_prep <- final_raw(year = year, geo=geo)
  } # or insert if modifying scenarios
  else final_prep = .data

  # substitute tract-level poverty and race measures when geo is block group due to very high MOEs in poverty variable
  if(geo == 'bg'){

    pov_seg_vars <- c('pct_below_pov', 'pct_asian', 'pct_black', 'pct_hispanic')

    final_prep <- dplyr::select(final_prep, -dplyr::contains(pov_seg_vars))
    tract_pov <- all_census_data(year = year, geo = 'tract') %>%
      dplyr::select(fips, dplyr::contains(pov_seg_vars)) %>%
      dplyr::rename_at(dplyr::vars(dplyr::contains(pov_seg_vars)),
                       ~gsub('_[0-9]{4}$','',.))

    final_prep <- dplyr::left_join(final_prep, tract_pov,
                                   by = 'fips')
  }
  # calculate location quotient for poverty and segregated overlay
    final_prep <- dplyr::mutate(final_prep,
                                pct_poc = 1-pct_white,
                                pct_poc_cty = 1-pct_white_cty,
                                lq_poc = (pct_poc/pct_poc_cty),
                                lq_asian = (pct_asian/pct_asian_cty),
                                lq_black = (pct_black/pct_black_cty),
                                lq_hispanic = (pct_hispanic/pct_hispanic_cty))


    # LQ threshold of 1.25 for identifying what tracts are racially segregated
    final_prep <- dplyr::mutate(final_prep, asian_conc = 0, black_conc = 0, hispanic_conc = 0,
                                poc_conc = 0)

    final_prep$asian_conc[which(final_prep$lq_asian > 1.25)] <- 1
    final_prep$black_conc[which(final_prep$lq_black > 1.25)] <- 1
    final_prep$hispanic_conc[which(final_prep$lq_hispanic > 1.25)] <- 1
    final_prep$poc_conc[which(final_prep$lq_poc > 1.25)] <- 1

    # create county specific threshold for share of each group that equates to 1.25 LQ
    final_prep <- final_prep %>%
      group_by(county_name) %>%
      mutate(asian_seg_thresh = 1.25*pct_asian_cty,
             black_seg_thresh = 1.25*pct_black_cty,
             hispanic_seg_thresh = 1.25*pct_hispanic_cty,
             poc_seg_thresh = 1.25*pct_poc_cty) %>%
      ungroup()

  # begin applying flags
  # apply coefficient of variation test for ACS variables and suppress unreliable data
  final_prep <- dplyr::mutate(final_prep,
                              cvpov200=((moe_pct_above_200_pov/1.645)/pct_above_200_pov)*100,
                              cvbaplus=((moe_pct_bachelors_plus/1.645)/pct_bachelors_plus)*100,
                              cvemp=((moe_pct_employed/1.645)/pct_employed)*100,
                              cvhomevalue=((moe_home_value/1.645)/home_value)*100,
                              cvpov=((moe_pct_below_pov/1.645)/pct_below_pov)*100,
                              povflag = 0, baplusflag = 0, empflag = 0, homeflag = 0, conpovcv = 0,
                              instit_flag = 0, military_flag = 0, density_flag = 0)


  final_prep$povflag[which(final_prep$pct_above_200_pov == 0 | is.na(final_prep$pct_above_200_pov) |
                             final_prep$cvpov200 > 30)] <- 1
  final_prep$baplusflag[which(final_prep$pct_bachelors_plus == 0 | is.na(final_prep$pct_bachelors_plus) |
                                final_prep$cvbaplus > 30)] <- 1
  final_prep$empflag[which(final_prep$pct_employed == 0 | is.na(final_prep$pct_employed) |
                             final_prep$cvemp > 30)] <- 1
  final_prep$homeflag[which(final_prep$home_value == 0 | is.na(final_prep$home_value) |
                              final_prep$cvhomevalue > 30)] <- 1
  final_prep$conpovcv[which(is.na(final_prep$pct_below_pov) |
                              final_prep$cvpov > 30)] <- 1





  #create flags for institutionalized pop, military, and density
  final_prep$instit_flag[which(final_prep$pct_instit_2020 > 0.75)] <- 1
  final_prep$military_flag[which(final_prep$pct_military >= 0.5)] <- 1
  final_prep$density_flag[which((final_prep$pop_density < 25 &
                                   final_prep$total_pop < 750))] <- 1

  # Invalidate scores with high MOEs
  final_prep$pct_above_200_pov[which(final_prep$povflag == 1)] <- NA
  final_prep$pct_bachelors_plus[which(final_prep$baplusflag == 1)] <- NA
  final_prep$pct_employed[which(final_prep$empflag == 1)] <- NA
  final_prep$home_value[which(final_prep$homeflag == 1)] <- NA
  final_prep$pct_below_pov[which(final_prep$conpovcv == 1)] <- NA


  # calculate race cvs
    final_prep <- dplyr::mutate(final_prep,
                                cv_asian=((moe_pct_asian/1.645)/pct_asian)*100,
                                cv_black=((moe_pct_black/1.645)/pct_black)*100,
                                cv_hispanic=((moe_pct_hispanic/1.645)/pct_hispanic)*100,
                                cv_white=((moe_pct_white/1.645)/pct_white)*100)


    # raise flags
    final_prep <- dplyr::mutate(final_prep, asian_cv_flag = 0, black_cv_flag = 0, hispanic_cv_flag = 0, white_cv_flag = 0)

    final_prep$asian_cv_flag[which(final_prep$pct_asian == 0 | is.na(final_prep$pct_asian) |
                                     final_prep$cv_asian > 30)] <- 1
    final_prep$black_cv_flag[which(final_prep$pct_black == 0 | is.na(final_prep$pct_black) |
                                     final_prep$cv_black > 30)] <- 1
    final_prep$hispanic_cv_flag[which(final_prep$pct_hispanic == 0 | is.na(final_prep$pct_hispanic) |
                                        final_prep$cv_hispanic > 30)] <- 1
    final_prep$white_cv_flag[which(final_prep$pct_white == 0 | is.na(final_prep$pct_white) |
                                     final_prep$cv_white > 30)] <- 1


    # invalidate derived race proportions with high MOEs
    final_prep$pct_asian[which(final_prep$asian_cv_flag == 1)] <- NA
    final_prep$pct_black[which(final_prep$black_cv_flag == 1)] <- NA
    final_prep$pct_hispanic[which(final_prep$hispanic_cv_flag == 1)] <- NA
    final_prep$pct_white[which(final_prep$white_cv_flag == 1)] <- NA
    # use white cv to invalidate poc measure since calculated as 1 - pct_white
    final_prep$pct_poc[which(final_prep$white_cv_flag == 1)] <- NA

    # invalidate concentrated race flags with high MOEs
    final_prep$asian_conc[which(final_prep$asian_cv_flag == 1)] <- NA
    final_prep$black_conc[which(final_prep$black_cv_flag == 1)] <- NA
    final_prep$hispanic_conc[which(final_prep$hispanic_cv_flag == 1)] <- NA
    final_prep$poc_conc[which(final_prep$white_cv_flag == 1)] <- NA

    # create racial seg flag
    final_prep <- dplyr::mutate(final_prep, racial_seg = 0)
    final_prep$racial_seg[which(final_prep$asian_conc == 1 | final_prep$black_conc == 1 |
                                  final_prep$hispanic_conc == 1 | final_prep$poc_conc == 1)] <- 1


    # create poverty and segregation flag
    final_prep$conpovflag <- ifelse(final_prep$pct_below_pov >= 0.3, 1, 0)
    final_prep$pov_seg_flag <- ifelse((final_prep$racial_seg == 1 &
                                    final_prep$conpovflag == 1), 1, 0)
    final_prep$pov_seg_flag[which(is.na(final_prep$conpovflag))] <- NA





  return(final_prep)

}





