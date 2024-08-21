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
#' @importFrom testthat test_that expect_equal expect_true
#' @references https://r-pkgs.org/testing-design.html
#'
#' @examples

#read_educ_pov
testthat::test_that("Scores with density, military, or prisoner flags are successfully invalidated",{
  setwd(here::here())
  final_opp <- suppressWarnings(final_opp(write=FALSE,reduced=FALSE))
  flagged_rows <- final_opp[which(final_opp$prison_flag == 1 | final_opp$military_flag == 1 | final_opp$density_flag == 1),]
  testthat::expect_true(sum(is.na(final_opp$oppscore==TRUE))>dim(flagged_rows)[1])
})


testthat::test_that("Any tract/bg with 2+ missing indicators should not have a resource designation",{
  setwd(here::here())
  final_opp <- suppressWarnings(final_opp(write=FALSE,reduced=FALSE))
  final_opp$na_count <- rowSums(is.na(select(final_opp,pct_above_200_pov_score:pct_not_frpm_score)))
  testthat::expect_true(unique(is.na(final_opp$oppscore[rownames(final_opp[final_opp$na_count>2,])]))) #rows with < 2 indicators have NA for oppscore
})

testthat::test_that("Ensure score’s align with resource designations",{
  setwd(here::here())
  final_opp <- suppressWarnings(final_opp(write=FALSE,reduced=FALSE))

  # Assert that oppcat aligns with oppscore
  testthat::expect_true(all(final_opp$oppcat[final_opp$oppscore >= 8] == "Highest Resource", na.rm = TRUE))
  testthat::expect_true(all(final_opp$oppcat[final_opp$oppscore >= 6 & final_opp$oppscore < 8] == "High Resource", na.rm = TRUE))
  testthat::expect_true(all(final_opp$oppcat[final_opp$oppscore >= 4 & final_opp$oppscore < 6] == "Moderate Resource", na.rm = TRUE))
  testthat::expect_true(all(final_opp$oppcat[final_opp$oppscore < 4] == "Low Resource", na.rm = TRUE))
})


#this test also ensures the minimum two observation rule per indicator but the former is closer to the language/angle in the documentation (testing count of indicators instead of geographies)
testthat::test_that("Ensure there’s at least two geographies within a region/rural county to assign a resource designation",{
  setwd(here::here())
  final_opp <- suppressWarnings(final_opp(write=FALSE,reduced=FALSE))

  #create columns containing count of regionid-wise null values
  region_counts <- final_opp %>%
    group_by(regionid) %>%
    summarise(count=n()) %>% ungroup()

  #assert that oppcat is NA when there are < 2 observations for a region
  skip_region <- region_counts[region_counts$count<2,]

  for (location_id in skip_region$regionid) {
    testthat::expect_true(all(is.na(final_opp$oppcat[final_opp$regionid == location_id])))
  }
})
