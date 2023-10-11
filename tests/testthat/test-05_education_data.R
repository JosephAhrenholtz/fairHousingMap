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

#read_educ_pov
testthat::test_that("There are no fully virtual schools (code: F) in the dataset",{
  educ <- read_educ_pov()
  testthat::expect_equal(sum(educ$Virtual=='F'),0)
})

testthat::test_that("All schools in the dataset are active",{
  educ <- read_educ_pov()
  testthat::expect_equal(sum(educ$StatusType=='Active'),dim(educ)[1])
})

testthat::test_that("All schools in the dataset either serve 3rd or 4th graders, nothing else",{
  educ <- read_educ_pov()
  testthat::expect_equal(sum(educ$SOC),c("65","60","61"))
})

testthat::test_that("Dataset contains only relevant county IDs",{
  educ <- read_educ_pov()
  testthat::expect_equal(sum(educ$countyid>700000),0)
})

#graduation_rates
testthat::test_that("Dataset contains only relevant county IDs",{
  graduation <- graduation_rates()
  testthat::expect_equal(sum(graduation_rates$countyid>7000),0)
  testthat::expect_equal(sum(graduation_rates$countyid>6000),0)
})
