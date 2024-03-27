#'Generates public summary files
#'
#'
#' @description
#' Loads final opportunity and neighborhood change data and creates excel workbook summary files, shapefiles, and data dictionaries
#'
#' @param year designates the map year's filepaths
#' @param write whether to write new opportunity summary files
#' @param change whether to write new change summary files
#'
#' @examples
#' final_opp_public(year = 2024, write = TRUE, change = TRUE)
#'
#' @import dplyr
#' @import sf
#' @import readr
#' @import openxlsx
#'


final_opp_public <- function(year = current_year, write = FALSE, change = FALSE){

  final <- final_opp(year,  reduced = F)
  geo <- final_opp(year, as_geo = T) %>% select(fips, fips_bg, geometry)

  # flip env site score to burden flag
  final$env_burden_flag <- ifelse(final$env_site_score == 1, 0, 1)

  #select columns
  public_columns <- c(
    # geo
    "fips", "fips_bg", "county_name", "region",
    # tract mesurements
    "pct_above_200_pov", "pct_bachelors_plus", "pct_employed", "home_value", "math_prof", "read_prof", "grad_rate", "pct_not_frpm",
    # regional benchmarks
    "pct_above_200_pov_median", "pct_bachelors_plus_median", "pct_employed_median", "home_value_median",
    "math_prof_median", "read_prof_median", "grad_rate_median", "pct_not_frpm_median",
    # environmental burden flag
    "env_burden_flag",
    # opportunity
    "oppscore","oppcat",
    # pov-seg
    "pct_below_pov", "pct_asian", "pct_black", "pct_hispanic", "pct_poc",
    "pct_asian_cty", "pct_black_cty", "pct_hispanic_cty", "pct_poc_cty",
    "pov_seg_flag"
    )

  description <- c(
    "Census Tract ID", "Census Block Group ID", "County Name", "Region",
    "Tract/block group percentage of population with income above 200% of federal poverty line ",
    "Tract/block group percentage of adults with a bachelors degree or above",
    "Tract/block group percentage of adults aged 20-64 who are employed in the civilian labor force or in the armed forces",
    "Tract/block group median value of owner-occupied units ",
    "Tract/block group percentage of 4th graders who meet or exceed math proficiency standards",
    "Tract/block group percentage of 4th graders who meet or exceed literacy standards",
    "Tract/block group percentage of high school cohort that graduated on time",
    "Tract/block group percentage of students not receiving free or reduced-price lunch",

    "Regional median percentage of population with income above 200% of federal poverty line ",
    "Regional median percentage of adults with a bachelors degree or above",
    "Regional median percentage of adults aged 20-64 who are employed in the civilian labor force or in the armed forces",
    "Regional median value of owner-occupied units ",
    "Regional median percentage of 4th graders who meet or exceed math proficiency standards",
    "Regional median percentage of 4th graders who meet or exceed literacy standards",
    "Regional median percentage of high school cohort that graduated on time",
    "Regional median percentage of students not receiving free or reduced-price lunch",

    "Binary flag identifying tracts/block groups with highest 5% of regional environmental burden",

    "Final Opportunity Score", "Opportunity Category",

    "Tract percentage of population with income below the federal poverty line",
    "Tract percentage of Asian population",
    "Tract percentage of Black population",
    "Tract percentage of Hispanic Population",
    "Tract percentage of all persons of color (POC)",
    "County percentage of Asian population",
    "County percentage of Black population",
    "County percentage of Hispanic Population",
    "County percentage of all persons of color (POC)",
    "Binary flag identifying tracts that meet standards for both concentrated poverty (defined as 30% of the population below the federal poverty line) and racial segregation (overrepresentation of Black, Hispanic, Asian, or all people of color relative to the county)"

  )
  # dict
  name <- public_columns
  opp_dict <- data.frame(name, description)

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
    region_col = which(names(final_public) == "Region")
    ncols <- length(final_public)

    #create regional worksheets
    sheets <- list(
    "Statewide" = final_public,
    "Bay Area" = final_public[final_public$Region == "Bay Area Region", -region_col],
    "Capital" = final_public[final_public$Region == "Capital Region", -region_col],
    "Central Coast" = final_public[final_public$Region == "Central Coast Region", -region_col],
    "Central Valley" = final_public[final_public$Region == "Central Valley Region", -region_col],
    "Inland Empire" = final_public[final_public$Region == "Inland Empire Region" , -region_col],
    "Los Angeles" = final_public[final_public$Region == "Los Angeles Region", -region_col],
    "Orange County" = final_public[final_public$Region == "Orange County Region", -region_col],
    "San Diego" = final_public[final_public$Region == "San Diego Region", -region_col],
    "Rural" = final_public[final_public$Region == "Rural Areas", -region_col]
    )

    #create excel style and save

    wb <- openxlsx::createWorkbook(title = paste(year, "CTCAC/HCD Opportunity Map Summary File"))
    colwidths <- rep(20, ncols)

    hs <- openxlsx::createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,
                                fgFill = "#969696")
    for(i in 1:length(sheets)){
      openxlsx::addWorksheet(wb, names(sheets)[i])
      openxlsx::writeData(wb, sheet = i, sheets[[i]], keepNA = TRUE)
      openxlsx::addStyle(wb, sheet = i, hs, rows = 1, cols = 1:ncols)
      if(i==1)  openxlsx::setColWidths(wb, sheet = i, cols = 1:ncols, widths = colwidths)
      else openxlsx::setColWidths(wb, sheet = i, cols = 1:(ncols-1), widths = colwidths[-region_col])
    }

    # write opportunity summary files
    openxlsx::saveWorkbook(wb,
      paste0("output/",year, "/final_opp_", year, "_public.xlsx"),
      overwrite = TRUE)
    shp_public <- final %>% select(public_columns) %>% inner_join(geo, by = c("fips", "fips_bg"))
    sf::st_write(shp_public, paste0("output/", year, "/final_opp_", year, "_public.gpkg"), quiet = TRUE, delete_dsn=T)
    readr::write_csv(opp_dict, paste0("output/",year, "/final_opp_", year, "_dictionary.csv"))


    # option to write summary files for neighborhood change
    if(change == TRUE){
      change_columns <- c("fips",
                          "county_name",

                          # pathway 1a
                          "baseline_raceinc0021",
                          "trct_raceeth_chng0021",
                          "raceeth_half0021",
                          "trct_inc_chng0021",
                          "inc_half0021",
                          "path_1a",

                          # pathway 1b
                          "baseline_race1321",
                          "baseline_income1321",
                          "trct_raceeth_chng1321",
                          "raceeth_quarter1321",
                          "trct_inc_chng1321",
                          "inc_quarter1321",
                          "path_1b",

                          # pathway 2
                          "halfmile_buffer",
                          "raceeth_half1321",
                          "inc_half1321",
                          "trct_pctchng_medrent1321",
                          "rent_half1321",
                          "pct_gap",
                          "gap_thresh",
                          "path_2",

                          # nc flag
                          "nbrhood_chng",

                          # exclude flag,
                          "nc_exclude_flag"
                          )

      description <- c(
        "Census Tract ID",
        "County Name",

        # pathway 1a
        "Binary flag if tract was a LMI and BIPOC neighborhood in 2000",
        "Percentage point change in tract's non-Hispanic white population (2000-2021)",
        "Countywide 50% threshold for non-Hispanic white tract-level percentage point increase (2000-2021)",
        "Percentage point change in tract's above-moderate-income households (2000-2021)",
        "Countywide 50% threshold for above-moderate-income households tract-level percentage point increase (2000-2021)",
        "Pathway 1A: Binary flag if a tract meets both racial/ethnic change and economic change between 2000-2021 (50% threshold)",

        # pathway 1b
        "Binary flag if tract was a BIPOC neighborhood in 2013",
        "Binary flag if tract was a LMI neighborhood in 2013",
        "Percentage point change in tract's non-Hispanic white population (2013-2021)",
        "Countywide 75% threshold for non-Hispanic white tract-level percentage point increase (2013-2021)",
        "Percentage point change in tract's above-moderate-income households (2013-2021)",
        "Countywide 75% threshold for above-moderate-income households tract-level percentage point increase (2013-2021)",
        "Pathway 1B: Binary flag if a tract meets both racial/ethnic change and economic change between 2013-2021",

        # pathway 2
        "Binary flag if a tract's population-weighted centroid is within 1/2-mile of the population-weighted centroid of a tract that meets Pathway 1A",
        "Countywide 50% threshold for non-Hispanic white tract-level percentage point increase (2013-2021)",
        "Countywide 50% threshold for above-moderate-income households tract-level percentage point increase (2013-2021)",
        "Percent change in tract's median rent (2013-2021)",
        "Countywide 50% threshold for percent change in median rent (2013-2021)",
        "Difference between the tract's household median income percentile and its median home value percentile (2021)",
        "Flat 25% threshold for home-value/income gap",
        "Pathway 2: Binary flag if a tract that is within 1/2-mile (population-weighted) of a tract that meets Pathway 1A and has rising rents and/or a home-value/income gap, and experienced either racial/ethnic or economic change (2013-2021)",

        # nc flag
        "Binary flag if a tract meets the definition of neighborhood change (Pathway 1A, Pathway 1B, or Pathway 2)",

        # exclude flag
        "Binary flag if tract has unreliable data or meets other exclusion parameters"

      )

      # dict
      name <- change_columns
      change_dict <- data.frame(name, description)

      # create neighborhood change workbook
      change_public <- final %>%
        filter(region != "Rural Areas") %>%
        dplyr::select(change_columns)


      # excel formatting
      change_public <- dplyr::rename(change_public,
                                    `Census Tract` = fips,
                                    County = county_name,

                                    # Pathway 1A
                                    `Binary Flag - Historic POC & LMI neighborhood in 2000 (baseline criteria)` = baseline_raceinc0021,
                                    `2000-2021 Non-Hispanic White Share Change (pp)` = trct_raceeth_chng0021,
                                    `2000-2021 County 50% Threshold for Non-Hispanic White Share Change (pp)` = raceeth_half0021,
                                    `2000-2021 High-Income Share Chage (pp)` = trct_inc_chng0021,
                                    `2000-2021 County 50% Threshold for High-Income Share Chage (pp)` = inc_half0021,
                                    `Binary Flag - Meets Pathway 1A Definition` = path_1a,

                                    # Pathway 1B
                                    `Binary Flag - Binary flag if tract was a BIPOC neighborhood in 2013` = baseline_race1321,
                                    `Binary Flag - Binary flag if tract was a LMI neighborhood in 2013` = baseline_income1321,
                                    `2013-2021 Non-Hispanic White Share Change (pp)` = trct_raceeth_chng1321,
                                    `2013-2021 County 75% Threshold for Non-Hispanic White Share Change (pp)` = raceeth_quarter1321,
                                    `2013-2021 High-Income Share Chage (pp)` = trct_inc_chng1321,
                                    `2013-2021 County 75% Threshold for High-Income Share Chage (pp)` = inc_quarter1321,
                                    `Binary Flag - Meets Pathway 1B Definition` = path_1b,

                                    # Pathway 2
                                    `Binary Flag - Within 1/2-mile of long-term change tract (Part 1)` = halfmile_buffer,
                                    `2013-2021 County 50% Threshold for Non-Hispanic White Share Change (pp)` = raceeth_half1321,
                                    `2013-2021 County 50% Threshold for High-Income Share Chage (pp)` = inc_half1321,
                                    `2013-2021 Median Rent Change (%)` = trct_pctchng_medrent1321,
                                    `2013-2021 County 50% Threshold for Median Rent Change (%)` = rent_half1321,
                                    `2021 Home Value/Income Gap (pp)` = pct_gap,
                                    `Flat 25% threshold for home-value/income gap` = gap_thresh,
                                    `Binary Flag - Meets Pathway 2 Definition` = path_2,

                                    # nc flag
                                    `Binary Flag - Meets Pathway 1A, Pathway 1B, or Pathway Definition` = nbrhood_chng,

                                    # Exclusion flag
                                    `Binary Flag - Tract has unreliable data or meets other exclusion parameters` = nc_exclude_flag
      )

      county_col = which(names(change_public) == "County")
      change_ncols <- length(change_public)

      #create regional worksheets
      change_sheets <- list(
        "Statewide Non-Rural Tracts" = change_public,
        "Alameda County" = change_public[change_public$County == "Alameda", -county_col],
        "Contra Costa County" = change_public[change_public$County == "Contra Costa", -county_col],
        "El Dorado County" = change_public[change_public$County == "El Dorado", -county_col],
        "Fresno County" = change_public[change_public$County == "Fresno", -county_col],
        "Imperial County" = change_public[change_public$County == "Imperial" , -county_col],
        "Kern County" = change_public[change_public$County == "Kern", -county_col],
        "Kings County" = change_public[change_public$County == "Kings", -county_col],
        "Los Angeles County" = change_public[change_public$County == "Los Angeles", -county_col],
        "Madera County" = change_public[change_public$County == "Madera", -county_col],

        "Marin County" = change_public[change_public$County == "Marin", -county_col],
        "Merced County" = change_public[change_public$County == "Merced", -county_col],
        "Monterey County" = change_public[change_public$County == "Monterey", -county_col],
        "Napa County" = change_public[change_public$County == "Napa", -county_col],
        "Orange County" = change_public[change_public$County == "Orange" , -county_col],
        "Placer County" = change_public[change_public$County == "Placer", -county_col],
        "Riverside County" = change_public[change_public$County == "Riverside", -county_col],
        "Sacramento County" = change_public[change_public$County == "Sacramento", -county_col],
        "San Benito County" = change_public[change_public$County == "San Benito", -county_col],

        "San Bernardino County" = change_public[change_public$County == "San Bernardino", -county_col],
        "San Diego County" = change_public[change_public$County == "San Diego", -county_col],
        "San Francisco County" = change_public[change_public$County == "San Francisco", -county_col],
        "San Joaquin County" = change_public[change_public$County == "San Joaquin", -county_col],
        "San Luis Obispo County" = change_public[change_public$County == "San Luis Obispo" , -county_col],
        "San Mateo County" = change_public[change_public$County == "San Mateo", -county_col],
        "Santa Barbara County" = change_public[change_public$County == "Santa Barbara", -county_col],
        "Santa Clara County" = change_public[change_public$County == "Santa Clara", -county_col],
        "Santa Cruz County" = change_public[change_public$County == "Santa Cruz", -county_col],

        "Solano County" = change_public[change_public$County == "Solano", -county_col],
        "Sonoma County" = change_public[change_public$County == "Sonoma", -county_col],
        "Stanislaus County" = change_public[change_public$County == "Stanislaus", -county_col],
        "Tulare County" = change_public[change_public$County == "Tulare", -county_col],
        "Ventura County" = change_public[change_public$County == "Ventura" , -county_col],
        "Yolo County" = change_public[change_public$County == "Yolo", -county_col]
      )

      change_wb <- openxlsx::createWorkbook(title = paste(year, "Neighborhood Change Summary File"))
      colwidths <- rep(20, change_ncols)

      hs <- openxlsx::createStyle(textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize=12,
                                  fgFill = "#969696")
      for(i in 1:length(change_sheets)){
        openxlsx::addWorksheet(change_wb, names(change_sheets)[i])
        openxlsx::writeData(change_wb, sheet = i, change_sheets[[i]], keepNA = TRUE)
        openxlsx::addStyle(change_wb, sheet = i, hs, rows = 1, cols = 1:change_ncols)
        if(i==1)  openxlsx::setColWidths(change_wb, sheet = i, cols = 1:change_ncols, widths = colwidths)
        else openxlsx::setColWidths(change_wb, sheet = i, cols = 1:(change_ncols-1), widths = colwidths[-county_col])
      }

      # write change summary files
      openxlsx::saveWorkbook(change_wb,
                             paste0("output/",year, "/final_change_", year, "_public.xlsx"),
                             overwrite = TRUE)
      change_shp_public <- final %>% filter(region != "Rural Areas") %>%
        select(change_columns, -region)
      geo_tract <- geo %>% filter(is.na(fips_bg)) %>% select(-fips_bg)
      change_shp_public <- change_shp_public %>% inner_join(geo_tract, by = "fips")
      sf::st_write(change_shp_public, paste0("output/", year, "/final_change_", year, "_public.gpkg"), quiet = TRUE, delete_dsn=T)
      readr::write_csv(change_dict, paste0("output/", year, "/final_change_", year, "_dictionary.csv"))

}


}
}
