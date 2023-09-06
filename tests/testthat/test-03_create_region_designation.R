#' Unit tests for region designation functions
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


testthat::test_that("All tracts with rural population percentage above 50% are designated rural",{
  setwd(here::here())

  tract_count_region <- create_regions(read = T)
  rural_pop <- tract_count_region %>% filter(rural_pct >= .5)
  testthat::expect_true(all(rural_pop$rural_flag == 1))
})
