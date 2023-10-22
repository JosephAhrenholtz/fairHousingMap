#'Generates the public summary files
#'
#'
#' @inheritParams final_TCAC
#'
#' @importFrom readr read_csv cols
#'


final_opp_public <- function(year = current_year, write = FALSE, change = FALSE){

  final <- final_opp(year, as_geo = T)
  geo <- final %>% select(fips, fips_bg, geometry)
  final <- final %>% st_drop_geometry() %>% as.data.frame()


  #select columns
  public_columns <- c('fips', 'fips_bg', 'county_name', 'region', 'pov_seg_flag',
    'oppscore','oppcat')

  final_public <- dplyr::select(final, public_columns)
  # write file to csv and excel
  if(write == TRUE){

    # excel formatting
    final_public <- dplyr::rename(final_public,
      `Census Tract` = fips,
      `Census Block Group` = fips_bg,
      County = county_name,
      Region = region,
      `High-Poverty & Segregated Flag` = pov_seg_flag,
      `Opportunity Score` = oppscore,
      `Opportunity Category` = oppcat)


    # index-based subscripting; needs to be re-written if the order of the columns above is changed
    region_col = which(names(final_public) == 'Region')
    ncols <- length(final_public)

    #create regional worksheets
    sheets <- list(
    'Statewide' = final_public,
    'BayArea' = final_public[final_public$Region == 'Bay Area Region', -region_col],
    'Capital' = final_public[final_public$Region == "Capital Region", -region_col],
    'CentralCoast' = final_public[final_public$Region == "Central Coast Region", -region_col],
    'CentralValley' = final_public[final_public$Region == "Central Valley Region", -region_col],
    'InlandEmpire' = final_public[final_public$Region == "Inland Empire Region" , -region_col],
    'LosAngeles' = final_public[final_public$Region == "Los Angeles Region", -region_col],
    'OrangeCounty' = final_public[final_public$Region == "Orange County Region", -region_col],
    'Rural' = final_public[final_public$Region == "Rural Areas", -region_col],
    'SanDiego' = final_public[final_public$Region == "San Diego Region", -region_col]
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
    sf::st_write(shp_public, paste0("output/", year, '/final_', year, '_public.shp'),
      delete_dsn=T)

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
