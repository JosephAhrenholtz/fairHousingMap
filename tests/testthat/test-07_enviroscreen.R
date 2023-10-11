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

testthat::test_that("Recheck env_site_mean by taking the mean of relevant columns, and naming them individually",{
  xwalk_ces_output <- xwalk_ces(write=TRUE)
  xwalk_ces_output$mean_recheck = rowMeans(select(xwalk_ces_output,cleanup_sites,hazard_waste,groundwater,solid_waste),na.rm=TRUE)
  testthat::expect_equal(xwalk_ces_output$env_site_mean,xwalk_ces_output$mean_recheck)
})

testthat::test_that("Recheck env_site_pctl by applying percentile to rechecked mean",{
  xwalk_ces_output <- xwalk_ces(write=TRUE)
  xwalk_ces_output$pctl_recheck_approach_one = percent_rank(rowMeans(select(xwalk_ces_output,cleanup_sites,hazard_waste,groundwater,solid_waste),na.rm=TRUE))
  xwalk_ces_output$pctl_recheck_approach_two = percent_rank(xwalk_ces_output$mean_recheck)
  testthat::expect_equal(xwalk_ces_output$env_site_pctl,xwalk_ces_output$pctl_recheck_approach_one)
  testthat::expect_equal(xwalk_ces_output$env_site_pctl,xwalk_ces_output$pctl_recheck_approach_two)
})
