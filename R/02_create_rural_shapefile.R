#' Imports USDA shapefile for areas ineligible for rural designation
#'
#' Reads USDA urban areas, then shrinks to only California data and subtracts them from the
#'   complete California shapefile to get a rural areas shapefile.
#'
#' @param read Reads from file, rather than computing. Saves time in evaluation for other functions.
#'
#' @source USDA areas ineligible for rural development housing programs: https://www.sc.egov.usda.gov/data/data_files.html
#' @source Changes in 2019 detailed
#'   \href{https://www.rd.usda.gov/files/CA-SFH-NoticeRuralAreaReview-Final-4.16.18.pdf}{here}.
#' @export
shape_rural <- function(year = current_year, write = FALSE){
  filepaths(year)
shape_USDA_excluded <- sf::st_read(USDA_excluded, quiet = TRUE)
shape_USDA_excluded <- subset(shape_USDA_excluded, STATENAME == 'California')
# resolve overlapping boundaries and shift to the specified projeciton
shape_USDA_excluded <- sf::st_buffer(shape_USDA_excluded, dist = 0.1)
shape_USDA_excluded <- sf::st_transform(shape_USDA_excluded, crs = 4326)
# Simplify into one polygon
shape_USDA_excluded <- sf::st_geometry(shape_USDA_excluded) %>%
  sf::st_union()
# subtract ineligble areas from shape_CA for eligible areas, throws an
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




