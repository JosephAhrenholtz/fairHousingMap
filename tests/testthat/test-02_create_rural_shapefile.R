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

  shape_USDA_excluded <- shape_rural(testing_handle=TRUE)
  rural_shp = shape_rural()

  # test length of returned: binary predicate
  # st_touches Identifies if geometries of x and y share a common point but their interiors do not intersect
  testthat::expect_false(lengths(sf::st_touches(shape_USDA_excluded, rural_shp)) > 0)
})



