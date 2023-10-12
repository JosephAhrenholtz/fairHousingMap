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
  final_opp <- final_opp(write=TRUE,reduced=FALSE)
  flagged_rows <- final_opp[which(final_opp$prison_flag == 1 | final_opp$military_flag == 1 | final_opp$density_flag == 1),]
  testthat::expect_equal(sum(is.na(final_opp$oppscore_zero==TRUE)),dim(flagged_rows)[1])
})

testthat::test_that("Data is for the correct year",{
  final_raw <- final_raw(write=FALSE,testing_handle=TRUE)

  #create df where each row contains the last suffix of each column's name
  suffixes <- list()
  for(split_vals in strsplit(names(final_raw),"_")){
    suffix <- (tail(split_vals,n=1))
    suffixes <- append(suffixes,suffix)
  }
  suffixes_df <- as.data.frame(do.call(rbind, suffixes))
  row.names(suffixes_df) <- NULL
  names(suffixes_df)[1] <- "suffix"

  #keep only numeric suffixes
  num_suffixes <- as.data.frame(suffixes_df[grep("[[:digit:]]", suffixes_df$suffix), ])
  names(num_suffixes)[1] <- "suffix"
  test_year = current_year-3
  testthat::expect_equal(sum(num_suffixes$suffix==test_year),dim(num_suffixes)[1]-2) #all cols, save for 2, with a numeric suffix should have current_year-3
})

testthat::test_that("Any tract/bg with 2+ missing indicators should not have a resource designation",{
  final_opp <- final_opp(write=TRUE,reduced=FALSE)
  final_opp$na_count <- rowSums(is.na(select(final_opp,pct_above_200_pov_score:pct_not_frpm_score)))
  testthat::expect_true(unique(is.na(final_opp$oppscore[rownames(final_opp[final_opp$na_count>2,])]))) #rows with < 2 indicators have NA for oppscore
})

testthat::test_that("Ensure score’s align with resource designations",{
  final_opp <- final_opp(write=TRUE,reduced=FALSE)
  testthat::expect_true(all(final_opp[final_opp$oppscore>=8,c("oppcat")]=="Highest Resource",na.rm=TRUE))
  testthat::expect_true(all(final_opp[(final_opp$oppscore>=6 & final_opp$oppscore<8),c("oppcat")]=="High Resource",na.rm=TRUE))
  testthat::expect_true(all(final_opp[(final_opp$oppscore>=4 & final_opp$oppscore<6),c("oppcat")]=="Moderate Resource",na.rm=TRUE))
  testthat:expect_true(all(final_opp[final_opp$oppscore<4,c("oppcat")]=="Low Resource",na.rm=TRUE))
})
