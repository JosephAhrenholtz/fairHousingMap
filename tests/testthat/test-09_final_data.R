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
  final_opp <- final_opp(write=TRUE,reduced=FALSE)
  flagged_rows <- final_opp[which(final_opp$prison_flag == 1 | final_opp$military_flag == 1 | final_opp$density_flag == 1),]
  testthat::expect_true(sum(is.na(final_opp$oppscore_zero==TRUE))>dim(flagged_rows)[1])
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
  setwd(here::here())
  final_opp <- final_opp(write=TRUE,reduced=FALSE)
  final_opp$na_count <- rowSums(is.na(select(final_opp,pct_above_200_pov_score:pct_not_frpm_score)))
  testthat::expect_true(unique(is.na(final_opp$oppscore[rownames(final_opp[final_opp$na_count>2,])]))) #rows with < 2 indicators have NA for oppscore
})

testthat::test_that("Ensure score’s align with resource designations",{
  setwd(here::here())
  final_opp <- final_opp(write=TRUE,reduced=FALSE)
  testthat::expect_true(all(final_opp[final_opp$oppscore>=8,c("oppcat")]=="Highest Resource",na.rm=TRUE))
  testthat::expect_true(all(final_opp[(final_opp$oppscore>=6 & final_opp$oppscore<8),c("oppcat")]=="High Resource",na.rm=TRUE))
  testthat::expect_true(all(final_opp[(final_opp$oppscore>=4 & final_opp$oppscore<6),c("oppcat")]=="Moderate Resource",na.rm=TRUE))
  testthat::expect_true(all(final_opp[final_opp$oppscore<4,c("oppcat")]=="Low Resource",na.rm=TRUE))
})

testthat::test_that("Ensure all neighborhood change indicators brought in using read_neighborhood_change() are null in rural areas",{
  setwd(here::here())
  final_opp <- final_opp(write=TRUE,reduced=FALSE)
  rural_obs <- dim(final_opp[final_opp$region=="Rural Areas",])[1]
  for(var_name in c("baseline_raceinc0021", "baseline_race1321", "part1", "part2", "nbrhood_chng", "trct_raceeth_chng0021",
                    "trct_raceeth_chng1321", "trct_inc_chng0021", "trct_inc_chng1321", "halfmile_buffer", "raceeth_half0021",
                    "raceeth_half1321", "inc_half0021", "inc_half1321", "rent_quarter1321", "trct_pctchng_medrent1321")){
    null_obs_for_that_var <- sum(is.na((final_opp[final_opp$region=="Rural Areas",c(var_name)])))
    #print(null_obs_for_that_var)
    testthat::expect_equal(null_obs_for_that_var,rural_obs)
  }
})

testthat::test_that("Ensure minimum of two observations per indicator to calculate a median for a region/rural county (regionid)",{
  setwd(here::here())
  final_opp <- final_opp(write=TRUE,reduced=FALSE)

  #generate handle for regionid-wise row count
  region_wise_counts <- final_opp %>%
    group_by(regionid) %>%
    summarise(count_of_rows=n()) %>%
    ungroup()

  final_opp <- merge(final_opp,region_wise_counts,by='regionid')

  trimmed_final_opp <- final_opp[final_opp$count_of_rows<2,]
  outcome_list <- names(final_opp)[grepl("_median",names(final_opp))]
  for(outcome in outcome_list){
    testthat::expect_true(is.na(trimmed_final_opp %>% select(all_of(outcome))))
  }
})

#this test also ensures the minimum two observation rule per indicator but the former is closer to the language/angle in the documentation (testing count of indicators instead of geographies)
testthat::test_that("Ensure there’s at least two geographies within a region/rural county to assign a resource designation",{
  setwd(here::here())
  final_opp <- final_opp(write=TRUE,reduced=FALSE)

  #create columns containing count of regionid-wise null values
  region_counts <- final_opp %>%
    group_by(regionid) %>%
    summarise(count=n()) %>% ungroup()

  #assert that oppcat is NA when there are < 2 observations for a region
  skip_region <- region_counts[region_counts$count<2,]

  for(location_id in skip_region$regionid) {
    testthat::expect_true(is.na(final_opp[final_opp$regionid==location_id,c('oppcat')]))
  }
})
