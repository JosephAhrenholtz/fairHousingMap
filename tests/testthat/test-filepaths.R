#' Unit tests for census geography functions
#'
#' @details
#' Top-level code should be scoped inside of test_that calls
#'
#'
#' @return
#' @export
#'
#'
#' @importFrom testthat test_that expect_equal expect_that
#' @references https://r-pkgs.org/testing-design.html
#'
#' @examples

testthat::test_that('data-raw exists in root', {
  setwd(here::here())
  testthat::expect_true(dir.exists('data-raw'))
})

testthat::test_that('R exists inside data-raw', {
  setwd(here::here())
  testthat::expect_true(dir.exists('data-raw/R'))
})

testthat::test_that('shapefiles exists inside data-raw', {
  setwd(here::here())

  testthat::expect_true(dir.exists('data-raw/shapefiles'))
})

testthat::test_that('.zip files for yearly data exist', {
  setwd(here::here())

  testthat::expect_true(file.exists('data-raw/2019.zip'))
  testthat::expect_true(file.exists('data-raw/2020.zip'))
  testthat::expect_true(file.exists('data-raw/2021.zip'))
  testthat::expect_true(file.exists('data-raw/2022.zip'))
  testthat::expect_true(file.exists('data-raw/2023.zip'))
  testthat::expect_true(file.exists('data-raw/2024.zip'))
})

testthat::test_that('Output directory, and sub-directory for current year exists in it', {
  setwd(here::here())

  testthat::expect_true(dir.exists('output'))
  testthat::expect_true(dir.exists(paste0('output/',current_year)))
})
