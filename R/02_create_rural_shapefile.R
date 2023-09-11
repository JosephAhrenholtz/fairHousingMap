#' Create rural designation shapefile
#'
#'
#' @description
#' Imports USDA shapefile for areas ineligible for rural designation, then shrinks to only California data and subtracts them from the
#' complete California shapefile to get a rural areas shapefile.
#'
#'
#' @param year designates the map year's filepaths
#' @param write whether to write the intermediate shapefile
#' @return a shapefile
#'
#'
#' @examples
#' shape_rural(write = T) # computes and writes the rural shapefile to the intermediate directory
#'
#'
#' @import sf
#'
#'
#' @source USDA areas ineligible for rural development housing programs: https://www.sc.egov.usda.gov/data/data_files.html
#' @source Changes in 2019 detailed: https://www.rd.usda.gov/files/CA-SFH-NoticeRuralAreaReview-Final-4.16.18.pdf
#'

#' @export
shape_rural <- function(year = current_year, write = FALSE){
  filepaths(year)
shape_USDA_excluded <- sf::st_read(USDA_excluded, quiet = TRUE)
shape_USDA_excluded <- subset(shape_USDA_excluded, STATENAME == 'California')
# resolve overlapping boundaries and shift to the specified projection
shape_USDA_excluded <- sf::st_buffer(shape_USDA_excluded, dist = 0.1)
shape_USDA_excluded <- sf::st_transform(shape_USDA_excluded, crs = 4326)
# simplify into one polygon
shape_USDA_excluded <- sf::st_geometry(shape_USDA_excluded) %>%
  sf::st_union()
# subtract ineligible areas from shape_CA for eligible areas, throws an
#   unnecessary warning to ensure both shapes are the same CRS
shape_USDA <- suppressWarnings(suppressMessages(
  sf::st_difference(shape_CA_state, shape_USDA_excluded) ))

if(write == TRUE){
#create shapefile directory; must be deleted if it exists
unlink(paste0("data/intermediate/",year, "/rural_shapefile"), recursive = TRUE)
sf::st_write(shape_USDA, dsn = paste0("data/intermediate/",year, "/rural_shapefile"), layer =
                  'rural_shapefile', driver = 'ESRI Shapefile')
}
else shape_USDA
}




