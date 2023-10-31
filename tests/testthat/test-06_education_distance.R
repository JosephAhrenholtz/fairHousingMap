#' Unit tests for census data functions
#'
#' @details
#' Top-level code should be scoped inside of test_that calls
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

testthat::test_that("Each centroid is assigned three schools",{
  setwd(here::here())
  #tests are incorporated into the main for-loop that joins nearest schools with each centroid
  testthat::expect_warning(school_distances(year=current_year, write=TRUE, testing_handle=TRUE))
})
