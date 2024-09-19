#' Loads all ACS and decennial Census data into a single data frame.
#'
#'
#' @description
#' Downloads all relevant decennial and ACS data from Census API using tidycensus, then derives percentages and
#' calculates margins of error for derived variables.
#'
#'
#' @param year designates the map year's filepaths
#' @param geo tract or block group
#' @param write write the intermediate file
#' @param read read an existing intermediate file
#'
#'
#' @return a data frame
#'
#'
#' @examples
#' all_census_data(read = F) # downloads decennial and acs data for the current map year at the tract level
#'
#'
#' @source Census API
#' @source tract/bg area: https://mcdc.missouri.edu/applications/geocorr2022.html
#'
#'
#' @importFrom dplyr sym
#' @import tidycensus zip
#'
#'
#' @details To use the Census APIs, sign up for an API key. Add Census API key to .Renviron profile and call it CENSUS_KEY. censusapi will use it by
#'   default. Within R, run: \cr
#'   Add key to .Renviron \cr
#'   `Sys.setenv(CENSUS_KEY= 'YOURKEYHERE')`\cr
#'   Reload .Renviron \cr
#'   `readRenviron("~/.Renviron")` \cr
#'   Check to see that the expected key is output in your R console \cr
#'   `Sys.getenv("CENSUS_KEY")` \cr
#'   `tidycensus::census_api_key('YOURKEYHERE', overwrite = TRUE, install = TRUE)` \cr
#'
#' @note
#' school_distances depends on all_census_data.  The 2023 handle is included in order
#' to run 2023 school_distances at 2020 geos, required to implement 3-year rolling averages
#' of education data in the 2025 map.
#'

#' @export
all_census_data <- function(year = current_year, geo = 'tract', write = FALSE, read = !write){

  filename <- paste0("data/intermediate/", year, "/acs_census_", geo, ".csv.gz")

  # option to read existing or generate new
  if(read == TRUE & file.exists(filename)){
    finalcensus <- readr::read_csv(filename, guess_max = 10000, col_types = readr::cols())
  }
  else{
    filepaths(year = year)
    acs_year <- year - 3

    # load geo area in sq mi
    if(geo=='tract'){
      area <- read_zip(tract_area, year, type = 'tsv',
                       guess_max = 10000, col_types = readr::cols())[-1,]
      area <- dplyr::transmute(area, fips = paste0(county, gsub('\\.','', tract)),
        sqmi = as.numeric(LandSQMI))
    } else{
      area <- read_zip(bg_area, year, type = 'tsv',
                       guess_max = 10000, col_types = readr::cols())[-1,]
      area <- dplyr::transmute(area, fips_bg = paste0(county, gsub('\\.','', tract),blockgroup),
        sqmi = as.numeric(LandSQMI))
    }

    census <- read_census_data(year = year, geo = geo)
    acs <- read_acs_data(year = year, geo = geo)
    regions <- create_regions() %>% dplyr::select(fips, region)

    finalcensus <- suppressMessages(dplyr::full_join(census, acs) %>%
                                      dplyr::left_join(area) %>%
                                      dplyr::left_join(regions) %>%
                                      dplyr::mutate(!!paste0('pop_density_',acs_year) :=
                                                      !!dplyr::sym(paste0("total_pop_", acs_year))/sqmi))

  }
  if(write == TRUE){
    readr::write_csv(finalcensus, filename)
  }
  else finalcensus
}


#' @export
#' @rdname all_census_data
read_census_data <- function(year = current_year, geo = 'tract'){
  filepaths(year = year)

  verify_geo(geo)
  # tidycensus requires the label 'block group'
  if(geo == 'bg') {geo <- 'block group'}

  census_vars <- read_zip(census_variables, year,
                          col_names = FALSE, col_types = readr::cols())[[1]]


  # download decennial census data on institutionalized population
  if(geo == 'tract'){
    raw_census_table <- tidycensus::get_decennial('tract', year = 2020, variables = census_vars,
                                         state = '06', output = 'wide')
  }
  if(geo == 'block group'){
    raw_census_table <- tidycensus::get_decennial('block group', year = 2020, variables = census_vars,
                                                  state = '06', output = 'wide')
  }

  # rename and calculate pct institutionalized pop
  census_table <- dplyr::transmute(raw_census_table, fips = GEOID, countyid = substr(fips,1,5),
                                  total_pop_2020 = P1_001N,
                                  pct_instit_2020 = P5_002N/total_pop_2020
  )

  if(geo == 'block group'){
    census_table <- dplyr::mutate(census_table, fips_bg = fips,
                               fips = substr(fips, 1, 11))
  }
  census_table <- census_table[order(census_table$fips),]
  census_table
}



#' @export
#' @rdname all_census_data
read_acs_data <- function(year = current_year, geo = 'tract', testing_handle = FALSE, test_name=''){

  # 2023 map reverts to 2019 ACS
  if(year == 2023){
    acs_year <- year-4
  } else {
    acs_year <- year-3
  }

  moe_level <- 90
  verify_geo(geo)
  # tidycensus requires the label 'block group'
  if(geo == 'bg') {geo <- 'block group'}
  filepaths(year = year)
  acs_vars <- read_zip(acs_variables, year,
    col_names = FALSE, col_types = readr::cols())[[1]]
  #list of CA counties
  counties <- tidycensus::fips_codes %>% dplyr::filter(state == 'CA')
  counties <- counties[['county_code']]

  if(testing_handle==TRUE & test_name=='load') {
    #return csv containing acs outcome codes and variable names
    var_list <- read_zip(acs_variables, year,
                    col_names = FALSE, col_types = readr::cols())
    all_acs_vars <- as.data.frame(tidycensus::load_variables(acs_year,'acs5'))
    var_list <- merge(var_list,all_acs_vars,by.x='X1',by.y='name',all.x=TRUE)
    var_list <- as.data.frame(var_list %>% rename(code = X1,name_in_code=X2))

    dir.create('data-raw//testing')
    write.csv(var_list, file = paste0('data-raw//testing//',acs_year,'.csv'), row.names = FALSE)
    return('Testing files generated successfully!')
  }

  # download acs data
  if(geo == 'tract'){
    raw_acs_table <- tidycensus::get_acs('tract', year = acs_year, variables = acs_vars,
      state = '06', output = 'wide', moe_level = moe_level)
  }
  if(geo == 'block group'){
    raw_acs_table <- tidycensus::get_acs('block group', year = acs_year, variables = acs_vars,
                                         state = '06', output = 'wide', moe_level = moe_level)
  }


  #rename and create derived variables
  acs_table <- dplyr::mutate(raw_acs_table,
    employment_pop_ = B23024_001E,
    moe_employment_pop_ = B23024_001M,
    employed_ = B23024_005E + B23024_007E + B23024_012E + B23024_014E + B23024_020E + B23024_022E + B23024_027E + B23024_029E,
    pct_employed_ = employed_/employment_pop_
  )
  acs_table <- dplyr::mutate(acs_table,

      #demographic variables
      total_pop_ = B03002_001E,
      white_ = B03002_003E,
      black_ = B03002_004E,
      amer_indian_ = B03002_005E,
      asian_ = B03002_006E,
      hawaiian_ = B03002_007E,
      other_race_ = B03002_008E,
      two_races_ = B03002_009E,
      hispanic_ = B03002_012E,
      moe_total_pop_  = B03002_001M,
      moe_white_  = B03002_003M,
      moe_black_  = B03002_004M,
      moe_amer_indian_  = B03002_005M,
      moe_asian_  = B03002_006M,
      moe_hawaiian_  = B03002_007M,
      moe_other_race_  = B03002_008M,
      moe_two_races_  = B03002_009M,
      moe_hispanic_  = B03002_012M,
      tot_other_race_ = (amer_indian_+hawaiian_ + other_race_ + two_races_),

      # poverty variables
      tot_pop_pov_  = C17002_001E, # total pop for which poverty status determined (poverty universe)
      moe_tot_pop_pov_ = C17002_001M,
      # college students in poverty
      college_pov_ = B14006_009E + B14006_010E,
      college_tot_ = B14007_017E + B14007_018E,
      # pct of college students in the total population
      pct_college_ = college_tot_/total_pop_,
      # above 200 pov
      above_200_pov_  = C17002_008E,
      moe_above_200_pov_  = C17002_008M,
      tot_pop_pov_ = C17002_001E,
      moe_tot_pop_pov_ = C17002_001M,

      # conditionally adjust poverty universe to control for college students in economic indicator
      pct_above_200_pov_  = ifelse(pct_college_ < 0.25,
                                   above_200_pov_/tot_pop_pov_, above_200_pov_/(tot_pop_pov_-college_pov_)),
      # below federal poverty
      prelim_below_pov_ = B17020_002E,
      moe_below_pov_  = B17020_002M,
      # conditionally adjust pop below poverty line to control for college students in high poverty and segregated designation
      below_pov_ = ifelse(pct_college_ < 0.25,
        prelim_below_pov_, prelim_below_pov_ - college_pov_),
      pct_below_pov_  = below_pov_/tot_pop_pov_,

      # household income variables
      med_hhincome_ = B19013_001E,
      moe_med_hhincome_ = B19013_001M,
      # home value variables
      home_value_ = B25077_001E,
      moe_home_value_ = B25077_001M,
      # adult education
      pct_bachelors_plus_  = (B15003_022E + B15003_023E + B15003_024E + B15003_025E)/B15003_001E,
      # county names
      county_name = case_when(acs_year <= 2021 ~ sub('(Block Group \\d+, )?Census Tract \\d+\\.*\\d*, ', '', NAME), # Handles comma separators used prior to ACS 2022
                              acs_year > 2021 ~ sub('(Block Group \\d+; )?Census Tract \\d+\\.*\\d*; ', '', NAME)), # Handles semi-colon separators used as of ACS 2022
      county_name = gsub(' County.*', '', county_name),
      # military population
      pct_military_ = B23025_006E/B23025_001E
    ) %>%

      # keep only relevant variables
      dplyr::select(fips = GEOID, county_name, dplyr::ends_with("_"))

  # include tract and block fips if block groups are returned
  if(geo == 'block group'){
    acs_table <- dplyr::mutate(acs_table, fips_bg = fips,
      fips = substr(fips, 1, 11))
  }

  if(testing_handle==TRUE & test_name=='college') {
    college_test_data <- acs_table %>% select(pct_college_, tot_pop_pov_,above_200_pov_,college_pov_,pct_above_200_pov_)
    return(college_test_data)
  }

  # Creating margins of error that require combining two or more variables
  MOE_list <- lapply(1:nrow(acs_table), function(i){
    raw <- raw_acs_table[i,]
    acs <- acs_table[i,]
    # demographic MOEs
    moe_tot_other_race_ <-
      tidycensus::moe_sum(c(acs$moe_amer_indian_, acs$moe_two_races_,
        acs$moe_hawaiian_, acs$moe_other_race_),
        c(acs$amer_indian_, acs$two_races_,
          acs$hawaiian_, acs$other_race_))


    # poverty MOEs
    moe_pct_above_200_pov_ <-
      tidycensus::moe_prop(acs$above_200_pov_, acs$tot_pop_pov_,
        acs$moe_above_200_pov_, acs$moe_tot_pop_pov_)

    moe_pct_below_pov_ <- tidycensus::moe_prop(acs$below_pov_, acs$tot_pop_pov_,
      acs$moe_below_pov_, acs$moe_tot_pop_pov_)


    # employment MOEs
      moe_employment_pop_ = acs$moe_employment_pop_
      MOE <- c(raw$B23024_005M, raw$B23024_007M, raw$B23024_012M, raw$B23024_014M, raw$B23024_020M, raw$B23024_022M, raw$B23024_027M, raw$B23024_029M)
      EST <- c(raw$B23024_005E, raw$B23024_007E, raw$B23024_012E, raw$B23024_014E, raw$B23024_020E, raw$B23024_022E, raw$B23024_027E, raw$B23024_029E)
    moe_employed_ <- tidycensus::moe_sum(MOE, EST)

    moe_pct_employed_ <-
      tidycensus::moe_prop(acs$employed_, acs$employment_pop_,
        moe_employed_, moe_employment_pop_)


    # education MOEs
    moe_pct_bachelors_plus_ <- # first, MOE for sum of people with Bachelor's
      tidycensus::moe_sum(c(raw$B15003_022M, raw$B15003_023M, raw$B15003_024M, raw$B15003_025M),
        c(raw$B15003_022E, raw$B15003_023E, raw$B15003_024E, raw$B15003_025E))

    moe_pct_bachelors_plus_ <- # second, revised MOE for percentage
      tidycensus::moe_prop(sum(raw$B15003_022E, raw$B15003_023E, raw$B15003_024E, raw$B15003_025E), raw$B15003_001E,
        moe_pct_bachelors_plus_, raw$B15003_001M)


    derived_MOEs <-
      data.frame(moe_tot_other_race_, moe_pct_below_pov_,
        moe_pct_employed_, moe_employed_, moe_pct_bachelors_plus_,
        moe_pct_above_200_pov_)

    derived_MOEs
  })

  # row bind MOE list then bind columns to acs
  MOEs <- do.call(rbind, MOE_list)
  acs_table <- cbind(acs_table, MOEs)



    # add derived ACS race vars for seg overlay
    acs_table <- dplyr::mutate(acs_table,
                               pct_asian_ = asian_/total_pop_,
                               pct_black_ = black_/total_pop_,
                               pct_hispanic_ = hispanic_/total_pop_,
                               pct_white_ = white_/total_pop_,
                               pct_tot_other_race_ = tot_other_race_/total_pop_)

    # moe for derived proportions
    acs_table <- dplyr::mutate(acs_table,
                               moe_pct_asian_ = tidycensus::moe_prop(asian_, total_pop_, moe_asian_, moe_total_pop_),
                               moe_pct_black_ = tidycensus::moe_prop(black_, total_pop_, moe_black_, moe_total_pop_),
                               moe_pct_hispanic_ = tidycensus::moe_prop(hispanic_, total_pop_, moe_hispanic_, moe_total_pop_),
                               moe_pct_white_ = tidycensus::moe_prop(white_, total_pop_, moe_white_, moe_total_pop_),
                               moe_pct_tot_other_race_ = tidycensus::moe_prop(tot_other_race_, total_pop_, moe_tot_other_race_, moe_total_pop_))



    # county variables
    acs_table <- dplyr::group_by(acs_table, county_name)
    acs_table <- dplyr::mutate(acs_table, total_pop_cty_ = sum(total_pop_),
                               hispanic_cty_ = sum(hispanic_),
                               white_cty_ = sum(white_),
                               black_cty_ = sum(black_),
                               asian_cty_ = sum(asian_),
                               amer_indian_cty_ = sum(amer_indian_),
                               other_race_cty_ = sum(other_race_),
                               tot_other_race_cty_ = sum(tot_other_race_),
                               pct_tot_other_race_cty_ = tot_other_race_cty_/total_pop_cty_,
                               pct_hispanic_cty_  = hispanic_cty_/total_pop_cty_,
                               pct_white_cty_  = white_cty_/total_pop_cty_,
                               pct_black_cty_ = black_cty_/total_pop_cty_,
                               pct_asian_cty_  = asian_cty_/total_pop_cty_)
    acs_table <- dplyr::ungroup(acs_table)
    acs_table <- acs_table[order(acs_table$fips),]
    acs_table



  #rename the variables that end with "_" to the correct year
  acs_table <- dplyr::rename_at(acs_table, dplyr::vars(dplyr::ends_with('_')),
    list(~paste0(.,acs_year)))
  acs_table
}
