#'Generates the public summary files
#'
#'
#' @inheritParams final_TCAC
#'
#' @importFrom readr read_csv cols
#'


final_opp_public <- function(year = current_year, write = FALSE, change = FALSE){

  final <- final_opp(year,  reduced = F)
  geo <- final_opp(year, as_geo = T) %>% select(fips, fips_bg, geometry)

  # flip env site score to burden flag
  final$env_burden_flag <- ifelse(final$env_site_score == 1, 0, 1)

  #select columns
  public_columns <- c(
    # geo
    'fips', 'fips_bg', 'county_name', 'region',
    # tract mesurements
    'pct_above_200_pov', 'pct_bachelors_plus', 'pct_employed', 'home_value', 'math_prof', 'read_prof', 'grad_rate', 'pct_not_frpm',
    # regional benchmarks
    'pct_above_200_pov_median', 'pct_bachelors_plus_median', 'pct_employed_median', 'home_value_median',
    'math_prof_median', 'read_prof_median', 'grad_rate_median', 'pct_not_frpm_median',
    # environmental burden flag
    'env_burden_flag',
    # opportunity
    'oppscore','oppcat',
    # pov-seg
    'pct_below_pov', 'pct_asian', 'pct_black', 'pct_hispanic', 'pct_poc',
    'pct_asian_cty', 'pct_black_cty', 'pct_hispanic_cty', 'pct_poc_cty',
    'pov_seg_flag'
    )

  final_public <- dplyr::select(final, public_columns)


  # write file to csv and excel
  if(write == TRUE){

    # excel formatting
    final_public <- dplyr::rename(final_public,
      `Census Tract` = fips,
      `Census Block Group` = fips_bg,
      County = county_name,
      Region = region,

      # tract
      `Share >200% Poverty` = pct_above_200_pov,
      `Share with Bachelors+` = pct_bachelors_plus,
      `Employment Rate` = pct_employed,
      `Median Home Value` = home_value,
      `Share Proficient in Math` = math_prof,
      `Share Proficient in Reading` = read_prof,
      `High School Grad Rate` = grad_rate,
      `Students Not in Poverty` = pct_not_frpm,
      # region
      `Regional Median - Share >200% Poverty` = pct_above_200_pov_median,
      `Regional Median - Share with Bachelors+` = pct_bachelors_plus_median,
      `Regional Median - Employment Rate` = pct_employed_median,
      `Regional Median - Median Home Value` = home_value_median,
      `Regional Median - Share Proficient in Math` = math_prof_median,
      `Regional Median - Share Proficient in Reading` = read_prof_median,
      `Regional Median - High School Grad Rate` = grad_rate_median,
      `Regional Median - Students Not in Poverty` = pct_not_frpm_median,
      # environment
      `Environmental Burden Flag` = env_burden_flag,
      # opportunity
      `Opportunity Score` = oppscore,
      `Opportunity Category` = oppcat,
      # pov-seg
      `Share Below Poverty Level` = pct_below_pov,
      `Share Asian` = pct_asian,
      `Share Black` = pct_black,
      `Share Hispanic` = pct_hispanic,
      `Share All POC` = pct_poc,
      `County Share Asian` = pct_asian_cty,
      `County Share Black` = pct_black_cty,
      `County Share Hispanic` = pct_hispanic_cty,
      `County Share All POC` = pct_poc_cty,
      `High-Poverty & Segregated Flag` = pov_seg_flag,
      )


    # index-based subscripting; needs to be re-written if the order of the columns above is changed
    region_col = which(names(final_public) == 'Region')
    ncols <- length(final_public)

    #create regional worksheets
    sheets <- list(
    'Statewide' = final_public,
    'Bay Area' = final_public[final_public$Region == 'Bay Area Region', -region_col],
    'Capital' = final_public[final_public$Region == "Capital Region", -region_col],
    'Central Coast' = final_public[final_public$Region == "Central Coast Region", -region_col],
    'Central Valley' = final_public[final_public$Region == "Central Valley Region", -region_col],
    'Inland Empire' = final_public[final_public$Region == "Inland Empire Region" , -region_col],
    'Los Angeles' = final_public[final_public$Region == "Los Angeles Region", -region_col],
    'Orange County' = final_public[final_public$Region == "Orange County Region", -region_col],
    'San Diego' = final_public[final_public$Region == "San Diego Region", -region_col],
    'Rural' = final_public[final_public$Region == "Rural Areas", -region_col]
    )

    #create excel style and save

    wb <- openxlsx::createWorkbook(title = paste(year, 'CTCAC/HCD Opportunity Map Summary File'))
    colwidths <- c(16,19,10,10,13,16,16)

    hs <- openxlsx::createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,
                                fgFill = "#969696")
    for(i in 1:length(sheets)){
      openxlsx::addWorksheet(wb, names(sheets)[i])
      openxlsx::writeData(wb, sheet = i, sheets[[i]], keepNA = TRUE)
      openxlsx::addStyle(wb, sheet = i, hs, rows = 1, cols = 1:ncols)
      if(i==1)  openxlsx::setColWidths(wb, sheet = i, cols = 1:ncols, widths = colwidths)
      else openxlsx::setColWidths(wb, sheet = i, cols = 1:(ncols-1), widths = colwidths[-region_col])
    }

    openxlsx::saveWorkbook(wb,
      paste0('output/',year, '/final_', year, '_public.xlsx'),
      overwrite = TRUE)

    # create public shapefile version of data
    #shp_public <- dplyr::inner_join(geo, dplyr::select(final, public_columns))
    shp_public <- final %>% select(public_columns) %>% inner_join(geo, by = c('fips', 'fips_bg'))
    # sf::st_write(shp_public, paste0("output/", year, '/final_', year, '_public.shp'),
    #   delete_dsn=T)
    sf::st_write(shp_public, paste0("output/", year, '/final_', year, '_public.gpkg'), quiet = TRUE, delete_dsn=T)

    if(change == TRUE){
      change_columns <- c('fips',
                          'county_name',
                          'region',
                          'baseline_raceinc0021',
                          'trct_raceeth_chng0021',
                          'raceeth_half0021',
                          'trct_inc_chng0021',
                          'inc_half0021',
                          'part1',
                          'baseline_race1321',
                          'baseline_income1321',
                          'trct_raceeth_chng1321',
                          'raceeth_half1321',
                          'trct_inc_chng1321',
                          'inc_half1321',
                          'rent_quarter1321',
                          'trct_pctchng_medrent1321',
                          'part2',
                          'nbrhood_chng')
      final_change <- final %>% filter(region != 'Rural Areas') %>%
        select(change_columns, -region)

      geo_tract <- geo %>% filter(is.na(fips_bg)) %>% select(-fips_bg)

      final_change <- final_change %>% inner_join(geo_tract, by = 'fips')




    }
}
}
