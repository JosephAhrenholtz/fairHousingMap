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
#' @export
filepaths <- function(year = current_year) {


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

  # neighborhood change
  assign("nc_scores", c("R/neighborhood_change/2024/Results/Neighborhood Change Layer/neighborhood_change_layer_2024.csv"), parent.frame())

  # supplementary data
  assign("pres_db_locations", c('data-raw/shapefiles/2024/chpc_pdb/OBI_presdb_geocoded_03202024.csv'), parent.frame())
  assign("pres_db_attributes", c('data-raw/shapefiles/2024/chpc_pdb/\\OBI PDB Request_012224.xlsx'), parent.frame())
  assign("segcat", c(2020, "cat_seg_ca_2020.csv"), parent.frame())

}
}

