#' Returns the final TCAC data frame output
#'
#'
#' @description
#' `final_raw` loads and combines all intermediate files with economic, education, and environmental indicators.
#' `final_prepare` creates the high poverty and segregated designation and flags unreliable data.
#' `final_opp` creates the final opportunity scores and designations.  `final_raw` and `final_prepare` are both
#' inputs into `final_opp`.  Only `final_opp` is necessary to run for generating new data.
#'
#'
#' @param year designates the map year's filepaths
#' @param geo allows for opportunity to be assessed at the tract level in urban areas and block group in rural areas
#' @param write write the final output
#' @param output `reduced` returns only variables necessary for the map.  If `full`, return all intermediate variables.
#' @param as_geo logical to return sf object
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
#'
#'
#' @export
final_opp <- function(year = current_year, write = FALSE, reduced = TRUE, as_geo = FALSE){

  # load and combine urban tracts and rural block groups
  urban <- final_prepare(year, geo='tract') %>% dplyr::filter(region != 'Rural Areas')
  rural <- final_prepare(year, geo='bg') %>% dplyr::filter(region == 'Rural Areas')
  final <- dplyr::bind_rows(urban, rural)

  final <- final %>%
    select(dplyr::contains('fips'), region, county_name, # geographic information
           pct_above_200_pov, home_value, pct_bachelors_plus, pct_employed, # economic
           math_prof, read_prof, grad_rate, pct_not_frpm, ## educational
           env_site_score, # environmental
           everything()
    )


  # assign region id
  final <- final %>%
    mutate(regionid = ifelse(region == "Rural Areas", county_name, region))


  # assign above/below regional median for economic/educational indicators
  final <- final %>%
    group_by(regionid) %>%
    # scores
    mutate_at(vars(pct_above_200_pov:pct_not_frpm), list(score = function(x) ifelse(x >= median(x, na.rm = TRUE), 1, 0))) %>%
    # regional median values for interface charts
    mutate_at(vars(pct_above_200_pov:pct_not_frpm), list(median = median), na.rm = T) %>%
    ungroup()


  # remove scores with less than 2 obs
  final <- final %>%
    group_by(regionid) %>%
    mutate_at(vars(pct_above_200_pov_score:pct_not_frpm_score), function(x) case_when(sum(!is.na(x)) < 2 ~ NA_real_, TRUE ~ x)) %>%
    ungroup()

  # invalidate medians with less than 2 obs
  final <- final %>%
    group_by(regionid) %>%
    mutate_at(vars(pct_above_200_pov:pct_not_frpm), list(valid = function(x) sum(!is.na(x)))) %>%
    ungroup()

  final$pct_above_200_pov_median[which(final$pct_above_200_pov_valid < 2)] <- NA
  final$home_value_median[which(final$home_value_valid < 2)] <- NA
  final$pct_bachelors_plus_median[which(final$pct_bachelors_plus_valid < 2)] <- NA
  final$pct_employed_median[which(final$pct_employed_valid < 2)] <- NA
  final$math_prof_median[which(final$math_prof_valid < 2)] <- NA
  final$read_prof_median[which(final$read_prof_valid < 2)] <- NA
  final$grad_rate_median[which(final$grad_rate_valid < 2)] <- NA
  final$pct_not_frpm_median[which(final$pct_not_frpm_valid < 2)] <- NA


  # calculate total non-null values for each geography
  final <- final %>%
    mutate(total_valid = rowSums(
      transmute_at(., vars(pct_above_200_pov_score:pct_not_frpm_score, env_site_score),
                   function(x) ifelse(!is.na(x), 1, 0))))


  # calculate positively oriented score
  final <- final %>%
    mutate(oppscore = rowSums(select(., pct_above_200_pov_score:pct_not_frpm_score, env_site_score), na.rm = TRUE),
           # invalidate scores with more than 2 missing values
           oppscore = ifelse(total_valid >= 7, oppscore, NA))


  # invalidate scores with density, military, or prisoner flags
  final$oppscore[which(final$prison_flag == 1 | final$military_flag == 1 |
                         final$density_flag == 1)] <- NA

  # load native land flag and apply hps invalidations
  final <- final %>%
    left_join(tribal_overlap(), by = 'fips')
  final$pov_seg_flag[which(final$prison_flag == 1 | final$military_flag == 1 |
                             final$density_flag == 1) | final$nativeland_flag == 1] <- NA

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



  # add poverty and env hazard threshold for interface charts
  final <- final %>%
    mutate(high_pov_thresh = .3, seg_thresh = 1.25)


  # join neighborhood change of non-rural tracts
  change <- read_neighborhood_change(year = year)
  final <- final %>% left_join(change, by = 'fips')

  # return reduced dataframe of map vars by default
  if(reduced == FALSE){
    final <- final
  } else {
    final <- final %>%
      select(
        # geo
        starts_with('fips'), region, regionid, county_name,
        # basic population
        total_pop,
        pop_density,
        density_flag,
        military_flag,
        prison_flag,
        # opp indicators
        pct_above_200_pov:pct_not_frpm, ends_with('median'), contains('score'),
        # pov and seg indicators
        pct_below_pov, high_pov_thresh, starts_with('lq_'), seg_thresh,
        pct_asian, pct_black, pct_hispanic, pct_poc,
        asian_seg_thresh, black_seg_thresh, hispanic_seg_thresh, poc_seg_thresh,
        # score/designations
        oppscore, oppcat, pov_seg_flag,
        # neighborhood change
        baseline_raceinc0021,
        baseline_race1321,
        baseline_income1321,
        part1,
        part2,
        nbrhood_chng,
        trct_raceeth_chng0021,
        trct_raceeth_chng1321,
        trct_inc_chng0021,
        trct_inc_chng1321,
        halfmile_buffer,
        raceeth_half0021,
        raceeth_half1321,
        inc_half0021,
        inc_half1321,
        rent_quarter1321,
        trct_pctchng_medrent1321)

    # limit digits to 4 to minimize file size
    final <- final %>% mutate_if(is.numeric,
                             round,
                             digits = 4)

    # option to write geojson
    if(as_geo == TRUE){
      #final <- final %>% select(-ends_with('_score'))
      urban <- final %>% filter(is.na(fips_bg))
      rural <- final %>% filter(!is.na(fips_bg))

      urban_geo <- urban %>% left_join(shape_CA_tract, by = c('fips', 'county_name')) %>% sf::st_as_sf() %>% sf::st_set_crs(4326)
      rural_geo <- rural %>% left_join(shape_CA_bg, by = c('fips_bg')) %>% sf::st_as_sf() %>% sf::st_set_crs(4326)
      final_geo <- dplyr::bind_rows(urban_geo, rural_geo)

      #return(final_geo)

      if(write == TRUE){
        sf::st_write(final_geo, paste0("output/", year, "/final_", year, '.geojson'))
      }



    }
  }

  #write final file
  if(write)readr::write_csv(final, paste0("output/", year, "/final_", year, '.csv'))
  return(final)


}




# everything below this line is an input to the final_opp function
#' @export
#' @rdname final_opp
final_raw <- function(year = current_year, geo = 'tract', write = FALSE, testing_handle=FALSE){

  # read data from previously generated files
  education_indicators <- school_distances(year = year, geo = geo)
  acs_census <- all_census_data(year = year, geo = geo)
  ces <- xwalk_ces(year = year)


  #join all data
  suppressMessages({
    rawdata <- dplyr::full_join(education_indicators, acs_census) %>%
      dplyr::left_join(ces)

  })

  if(testing_handle==TRUE){
    return(rawdata) #without removing year suffixes
  }

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
                              prison_flag = 0, military_flag = 0, density_flag = 0)


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





  #create flags for prison, military, and density
  final_prep$prison_flag[which(final_prep$pct_prisoner_2020 > 0.75)] <- 1
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
    # use white cv to invalidate poc measure bc calculated as 1 - pct_white # deciding what to do here.  Alt: sum Asian, Black, Hisp?
    #final_prep$pct_poc[which(final_prep$white_cv_flag == 1)] <- NA

    # invalidate concentrated race flags with high MOEs
    final_prep$asian_conc[which(final_prep$asian_cv_flag == 1)] <- NA
    final_prep$black_conc[which(final_prep$black_cv_flag == 1)] <- NA
    final_prep$hispanic_conc[which(final_prep$hispanic_cv_flag == 1)] <- NA
    #final_prep$poc_conc[which(final_prep$white_cv_flag == 1)] <- NA

    # create racial seg flag
    final_prep <- dplyr::mutate(final_prep, racial_seg = 0)
    final_prep$racial_seg[which(final_prep$asian_conc == 1 | final_prep$black_conc == 1 |
                                  final_prep$hispanic_conc == 1 | final_prep$poc_conc == 1)] <- 1


    # create poverty and segregation flag
    final_prep$conpovflag <- ifelse(final_prep$pct_below_pov >= 0.3, 1, 0)
    final_prep$pov_seg_flag <- ifelse((final_prep$racial_seg == 1 &
                                    final_prep$conpovflag == 1), 1, 0)






  return(final_prep)

}





