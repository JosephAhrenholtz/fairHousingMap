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

  expect_that(dir.exists('data-raw'),is_true())
})

testthat::test_that('R exists inside data-raw', {
  setwd(here::here())

  expect_that(dir.exists('data-raw/R'),is_true())
})

testthat::test_that('shapefile exists inside data-raw', {
  setwd(here::here())

  expect_that(dir.exists('data-raw/shapefile'),is_true())
})

testthat::test_that('.zip files for yearly data exist', {
  setwd(here::here())

  expect_that(file.exists('data-raw/2019.zip'),is_true())
  expect_that(file.exists('data-raw/2020.zip'),is_true())
  expect_that(file.exists('data-raw/2021.zip'),is_true())
  expect_that(file.exists('data-raw/2022.zip'),is_true())
  expect_that(file.exists('data-raw/2023.zip'),is_true())
  expect_that(file.exists('data-raw/2024.zip'),is_true())
})
