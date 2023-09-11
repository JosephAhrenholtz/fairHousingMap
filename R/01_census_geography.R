#' Reads pop-weighted centroids of Census tracts, block groups, and blocks
#'
#'
#' @param as_shape returns shapefile
#' @param year designates the map year's filepaths
#' @return a data frame
#'
#'
#' @examples
#' read_tract_centers(as_shape = T) # returns tract centroids as shapefile based on the default year
#'
#'
#' @import dplyr
#' @importFrom sp coordinates proj4string
#' @importFrom sf st_as_sf
#' @importFrom magrittr %>%
#'
#'
#' @source tract/bg centroids: https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.html
#' @source block centroids: https://mcdc.missouri.edu/applications/geocorr2022.html
#'

#' @export
read_tract_centers <- function(as_shape = FALSE,
                               year = current_year){
  filepaths(year = year)
  tract_centers <- read_zip(tract_center, year, col_types = readr::cols())
  tract_centers <- tidyr::unite(tract_centers, 'fips', STATEFP, COUNTYFP, TRACTCE, sep = '') %>%
    dplyr::rename(lon = LONGITUDE, lat = LATITUDE)
  #conversion to ShapePointsDataFrame
  if(as_shape == TRUE){
    sp::coordinates(tract_centers) <- ~lon + lat
    sp::proj4string(tract_centers) <- sp::CRS("+init=epsg:4326")
  }
  tract_centers
}

#' @export
read_block_group_centers <- function(as_shape = FALSE,
                               year = current_year){
  filepaths(year = year)
  block_group_centers <- read_zip(block_group_center, year, col_types = readr::cols())
  block_group_centers <- block_group_centers %>%
    tidyr::unite('fips', STATEFP, COUNTYFP, TRACTCE, sep = '', remove = F) %>%
    tidyr::unite('fips_bg', STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, sep = '') %>%
    dplyr::rename(lon = LONGITUDE, lat = LATITUDE)
  #conversion to ShapePointsDataFrame
  if(as_shape == TRUE){
    sp::coordinates(block_group_centers) <- ~lon + lat
    sp::proj4string(block_group_centers) <- sp::CRS("+init=epsg:4326")
  }
  block_group_centers
}

#' @export
read_block_centers <- function(as_shape = FALSE,
                               year = current_year, geo = 'tract'){
  filepaths(year = year)
  bc_cols <- c("county", "tract", "block", "us", "cntyname", "pop20", "lat", "lon", "afact" )
  block_centers <- read_zip(block_center, year, col_names = bc_cols, type = 'tsv',
                            col_types = 'ccccciddd', skip = 2, progress = FALSE)
  # format tract
  block_centers$tract <- gsub('\\.','', block_centers$tract)
  #create tract, bg, and block identifiers
  block_centers <- tidyr::unite(block_centers, 'fips', county, tract, sep = '',
                                remove = FALSE)
  block_centers <- tidyr::unite(block_centers, 'block', county, tract, block, sep = '')
  block_centers$fips_bg <- substr(block_centers$block, 1, 12)
  block_centers <- subset(block_centers, select = -afact)

  if(as_shape == TRUE){
    block_centers <- dplyr::group_by(block_centers, fips)
    if(geo == 'bg') block_centers <- dplyr::group_by(block_centers, fips_bg)
    block_centers <- dplyr::mutate(block_centers, block_pop_pct = pop20/sum(pop20))
    block_centers <- dplyr::ungroup(block_centers)
    block_centers <- sf::st_as_sf(block_centers, coords = c('lon','lat'),
                                  crs = 4326)
  }
  block_centers
}

