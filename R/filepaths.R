#' Shortcuts to zipped files in the data-raw directory. See `?read_zip`
#'
#' @source
#' \itemize{
#'   \item USDA_excluded: http://www.sc.egov.usda.gov/data/data_files.html, SFH/MFH Ineligibility
#'   \item school data: see education_enviroscreen.R
#' }


filepaths <- function(year = current_year) {
  # syear= year %% 100

if(year >= 2024){


  # Census centroids and area
  assign("tract_center_USA", c(2020,"CenPop2020_Mean_TR.txt"), parent.frame())
  assign("tract_center", c(2020, "CenPop2020_Mean_TR06.txt"), parent.frame())
  assign("block_group_center", c(2020, "CenPop2020_Mean_BG06.txt"), parent.frame())
  assign("block_group_center_USA", c(2020, "CenPop2020_Mean_BG.txt"), parent.frame())
  assign("bg_area", c(2020, "block_group_area_2020.csv"), parent.frame())
  assign("tract_area", c(2020, "tract_area_2020.csv"), parent.frame())
  assign("block_center", c(2020, "block_centroids2020_CA.txt"), parent.frame())
  assign("block_center_AZNVOR", c(2020, "block_centroids2020_AZ_NV_OR.csv"), parent.frame())

  # Economic
  assign("acs_variables", c(2021, 'acs21_variables.csv'), parent.frame())
  # Education
  assign("ED_english_math", c(2022, "sb_ca2022_1_csv_v1.txt"), parent.frame())
  assign("ED_cohort", c(2022, "cohort2122.txt"), parent.frame())
  assign("pubschls", c(2023, "pubschls23.txt"), parent.frame())
  assign("frpm", c(2023, "frpm2223.xlsx"), parent.frame())
  # Environmental
  assign("ces4results", c(2021, "ces4results.xlsx"), parent.frame())


  # region
  assign("USDA_excluded", c('data-raw/shapefiles/2021/SFH_MFH_Ineligible_20211117/SFH_MFH_Ineligible.shp'), parent.frame())
  assign("county_region_xwalk", c(2019, "county-region_xwalk.csv"), parent.frame())
  # assign("county_MPO_xwalk", c(2019, "county-MPO_xwalk.csv"), parent.frame())
  # assign("county_cog_xwalk", c(2019, "county-cog-xwalk.csv"), parent.frame())
}
}

