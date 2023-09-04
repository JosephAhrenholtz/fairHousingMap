#' Shortcuts to zipped files in the data-raw directory. See `?read_zip`
#'
#' @source
#' \itemize{
#'   \item USDA_excluded: http://www.sc.egov.usda.gov/data/data_files.html, SFH/MFH Ineligibility
#'   \item school data: see education_enviroscreen.R
#' }


filepaths <- function(year = current_year) {
  syear= year %% 100

if(year >= 2024){
  # Census
  assign("acs_variables", c(2021, 'acs21_variables.csv'), parent.frame())
  # CES
  assign("ces4results", c(2021, "ces4results.xlsx"), parent.frame())
  # Education
  assign("ED_english_math", c(2022, "sb_ca2022_1_csv_v1.txt"), parent.frame())
  assign("ED_cohort", c(2022, "cohort2122.txt"), parent.frame())
  assign("pubschls", c(2023, "pubschls23.txt"), parent.frame())
  assign("frpm", c(2023, "frpm2223.xlsx"), parent.frame())

  # region
  # assign("county_region_xwalk", c(2019, "county-region_xwalk.csv"), parent.frame())
  # assign("USDA_excluded", c("data-raw/shapefiles/2021/SFH_MFH_Ineligible20180823"), parent.frame())
  # assign("county_MPO_xwalk", c(2019, "county-MPO_xwalk.csv"), parent.frame())
  # assign("county_cog_xwalk", c(2019, "county-cog-xwalk.csv"), parent.frame())
}
}

