#' Shortcuts to zipped files in the data-raw directory. See `?read_zip`
#'
#'
#' @param year designates the map year's filepaths
#'
#'
#' @return filepaths to raw data are loaded into environment
#'
#'
#' @examples
#' filepaths(year = 2024)
#'
#' @note
#' Year 2023 filepaths are used to process 2023 OM education data at 2020 geographies, which is required
#' for implementation of 3-year rolling education averages.
#'
#' @export
filepaths <- function(year = current_year) {


if(year >= 2023){
  # census
  assign("census_variables", c(2020, 'census20_variables.csv'), parent.frame())
  assign("acs_variables", c(2019, 'acs19_variables.csv'), parent.frame())
  assign("tract_center", c(2020, "CenPop2020_Mean_TR06.txt"), parent.frame())
  assign("block_group_center", c(2020, "CenPop2020_Mean_BG06.txt"), parent.frame())
  assign("bg_area", c(2020, "bg_area2020.txt"), parent.frame())
  assign("tract_area", c(2020, "tract_area2020.txt"), parent.frame())
  # Education
  assign("ED_english_math", c(2019, "sb_ca2019_1_csv_v4.txt"), parent.frame())
  assign("ED_cohort", c(2021, "acgr21.txt"), parent.frame())
  assign("pubschls", c(2022, "pubschls22.txt"), parent.frame())
  assign("frpm", c(2022, "frpm2122.xlsx"), parent.frame())

}


if(year >= 2024){

  # census
  assign("census_variables", c(2020, 'census20_variables.csv'), parent.frame())
  assign("tract_center_USA", c(2020,"CenPop2020_Mean_TR.txt"), parent.frame())
  assign("tract_center", c(2020, "CenPop2020_Mean_TR06.txt"), parent.frame())
  assign("block_group_center", c(2020, "CenPop2020_Mean_BG06.txt"), parent.frame())
  assign("block_group_center_USA", c(2020, "CenPop2020_Mean_BG.txt"), parent.frame())
  assign("bg_area", c(2020, "bg_area2020.txt"), parent.frame())
  assign("tract_area", c(2020, "tract_area2020.txt"), parent.frame())
  assign("block_center", c(2020, "block_centroids2020_CA.txt"), parent.frame())

  # economic
  assign("acs_variables", c(2021, 'acs21_variables.csv'), parent.frame())
  # education
  assign("ED_english_math", c(2022, "sb_ca2022_1_csv_v1.txt"), parent.frame())
  assign("ED_cohort", c(2022, "cohort2122.txt"), parent.frame())
  assign("pubschls", c(2023, "pubschls23.txt"), parent.frame())
  assign("frpm", c(2023, "frpm2223.xlsx"), parent.frame())
  # environmental
  assign("ces4results", c(2021, "ces4results.xlsx"), parent.frame())


  # region
  assign("USDA_excluded", c('data-raw/shapefiles/2021/SFH_MFH_Ineligible_20211117/SFH_MFH_Ineligible.shp'), parent.frame())
  assign("county_region_xwalk", c(2019, "county-region_xwalk.csv"), parent.frame())
  assign("county_cog_xwalk", c(2019, "county-cog-xwalk.csv"), parent.frame())

  # neighborhood change - get updated code from Matt that includes baseline_race0021.  He sent file but not updated code.
  assign("nc_scores", c("R/neighborhood_change/2024/Results/Neighborhood Change Layer/neighborhood_change_layer_04042024.csv"), parent.frame())

}

if(year >= 2025){

  # economic
  assign("acs_variables", c(2022, 'acs22_variables.csv'), parent.frame())
  # education
  assign("ED_english_math", c(2023, "sb_ca2023_1_csv_v1.txt"), parent.frame())
  assign("ED_cohort", c(2023, "acgr23-v2.txt"), parent.frame())
  assign("pubschls", c(2024, "pubschls24.txt"), parent.frame())
  assign("frpm", c(2024, "frpm2324.xlsx"), parent.frame())

  # region
  assign("USDA_excluded", c('data-raw/shapefiles/2023/SFH_MFH_Ineligible20231206/SFH_MFH_IELG2020_CA.shp'), parent.frame())

  # neighborhood change
  assign("nc_scores", c("R/neighborhood_change/2025/Results/Neighborhood Change Layer/nbrchange_2022end_09162024.csv"), parent.frame())

  # supplementary data - large family LIHTC awards through 2023
  assign("lf_lihtc", c('data-raw/shapefiles/2023/chp_pdb/LF_LIHTC_OBI_09242024.gpkg'), parent.frame())

}


}

