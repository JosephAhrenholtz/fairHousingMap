#' Unit tests for creating rural shapefile
#'
#' @details
#' Top-level code should be scoped inside of test_that class
#'
#'
#' @return
#' @export
#'
#'
#' @importFrom testthat test_that expect_equal
#' @references https://r-pkgs.org/testing-design.html
#'
#' @examples


testthat::test_that('interiors of USDA ineligible areas are spatially distinct from interiors of rural shapefile',{
  setwd(here::here())
  filepaths()

  shape_USDA_excluded <- sf::st_read(USDA_excluded, quiet = TRUE)
  shape_USDA_excluded <- subset(shape_USDA_excluded, STATENAME == 'California')
  # resolve overlapping boundaries and shift to the specified projeciton
  shape_USDA_excluded <- sf::st_buffer(shape_USDA_excluded, dist = 0.1)
  shape_USDA_excluded <- sf::st_transform(shape_USDA_excluded, crs = 4326)
  # Simplify into one polygon
  shape_USDA_excluded <- sf::st_geometry(shape_USDA_excluded) %>%
    sf::st_union()

  rural_shp = shape_rural()

  # test length of returned: binary predicate
  # st_touches Identifies if geometries of x and y share a common point but their interiors do not intersect
  testthat::expect_false(lengths(sf::st_touches(shape_USDA_excluded, rural_shp)) > 0)
})



