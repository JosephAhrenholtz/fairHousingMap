#' Unit tests for census geography functions
#'
#' @details
#' Top-level code should be scoped inside of test_that calss
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


testthat::test_that('centroids data exists at at current years filepaths',{
  filepaths()
  testthat::expect_true(file.exists(unzip(paste0('data-raw/',tract_center[1],'.zip'), tract_center[2])))
  testthat::expect_true(file.exists(unzip(paste0('data-raw/',block_group_center[1],'.zip'), block_group_center[2])))
  testthat::expect_true(file.exists(unzip(paste0('data-raw/',block_center[1],'.zip'), block_center[2])))
})

testthat::test_that('all formatted tracts are 11 digits', {
  tract_centers <- read_tract_centers()
  tract_char <- range(nchar(tract_centers$fips))

  testthat::expect_equal(tract_char[1], tract_char[2])
  testthat::expect_equal(tract_char[1], 11)
})

testthat::test_that('all formatted block groups are 12 digits', {
  bg_centers <- read_block_group_centers()
  bg_char <- range(nchar(bg_centers$fips_bg))

  testthat::expect_equal(bg_char[1], bg_char[2])
  testthat::expect_equal(bg_char[1], 12)
})

testthat::test_that('all formatted blocks are 15 digits', {
  block_centers <- read_block_centers()
  block_char <- range(nchar(block_centers$block))

  testthat::expect_equal(block_char[1], block_char[2])
  testthat::expect_equal(block_char[1], 15)
})


testthat::test_that('formatted tracts in block file match formatted tracts in tract file',{
  tracts <- read_tract_centers()
  blocks <- read_block_centers()
  not_joined <- anti_join(blocks, tracts, by = 'fips')
  testthat::expect_equal(nrow(not_joined), 0)
})

testthat::test_that('formatted bgs in acs file match formatted bgs in centroid file',{
  acs <- all_census_data(geo = 'bg', read = T) %>% select(fips, fips_bg)
  block_group_centers <- read_block_group_centers()
  not_joined <- anti_join(acs, block_group_centers, by = c('fips', 'fips_bg'))
  testthat::expect_equal(nrow(not_joined), 0)
})

