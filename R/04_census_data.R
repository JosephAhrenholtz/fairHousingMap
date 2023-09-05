#' Loads all ACS data into a single data frame.
#'
#' @param year specify map year
#' @param geo tract or block group
#' @param write logical
#' @param read logical
#'
#' @return a data frame
#' @export
#'
#' @source Census API
#' @source tract/bg area: https://mcdc.missouri.edu/applications/geocorr2022.html
#'
#' @importFrom dplyr sym
#' @importFrom tidycensus get_acs
#'
#' @details To use the Census APIs, sign up for an API key. Add Census API key to .Renviron profile and call it CENSUS_KEY. censusapi will use it by
#'   default. Within R, run: \cr
#'
#'   Add key to .Renviron \cr
#'   `Sys.setenv(CENSUS_KEY= 'YOURKEYHERE')`\cr
#'   Reload .Renviron \cr
#'   `readRenviron("~/.Renviron")` \cr
#'   Check to see that the expected key is output in your R console \cr
#'   `Sys.getenv("CENSUS_KEY")` \cr
#'
#'   `tidycensus::census_api_key('YOURKEYHERE', overwrite = TRUE, install = TRUE)` \cr
#'
#'
all_census_data <- function(year = current_year, geo = 'tract', write = FALSE, read = !write){

  filename <- paste0("data/intermediate/", year, "/acs_census_", geo, ".csv.gz")

  if(read == TRUE & file.exists(filename)){
    finalcensus <- readr::read_csv(filename, guess_max = 10000, col_types = readr::cols())
  }

  else{
    filepaths(year = year)
    acs_year <- year - 3


    if(geo=='tract'){
      area <- read_zip(tract_area, year, type = 'csv',
                       guess_max = 10000, col_types = readr::cols())[-1,]
      area <- dplyr::transmute(area, fips = paste0(county, gsub('\\.','', tract)),
        sqmi = as.numeric(LandSQMI))
    } else{
      area <- read_zip(bg_area, year, type = 'csv',
                       guess_max = 10000, col_types = readr::cols())[-1,]
      area <- dplyr::transmute(area, fips_bg = paste0(county, gsub('\\.','', tract),blockgroup),
        sqmi = as.numeric(LandSQMI))
    }
    #census <- read_census_data(year = year)
    acs <- read_acs_data(year = year, geo = geo)
    regions <- create_regions() %>% dplyr::select(fips, region)

    finalcensus <- suppressMessages(acs) %>%
      dplyr::left_join(area) %>%
        dplyr::left_join(regions) %>%
        dplyr::mutate(!!paste0('pop_density_',acs_year) :=
            !!dplyr::sym(paste0("total_pop_", acs_year))/sqmi) %>%
        dplyr::filter(!is.na(county_name))  #removes one census tract that has changed.
  }
  if(write == TRUE){
    readr::write_csv(finalcensus, filename)
  }
  else finalcensus
}



#' @rdname all_census_data
read_acs_data <- function(year = current_year, geo = 'tract'){

  acs_year <- year-3

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



  if(geo == 'tract'){
    raw_acs_table <- tidycensus::get_acs('tract', year = acs_year, variables = acs_vars,
      state = '06', output = 'wide', moe_level = moe_level)
  }

  if(geo == 'block group'){
  #run for variables beginning in each letter, then collapse into one dataframe
  raw_acs_list <- list()
  for(i in seq(length(counties))){
    counter = 0
    print(paste0("Download ", round(100*i/length(counties)), "% complete"))
    timer <- Sys.time()
    #variables from different tables must be called separately
    # The large amount of data being downloaded can cause errors. Repeat
    #   until succssful.
    repeat{
      table1 <- try(suppressMessages(
        tidycensus::get_acs('block group', year = acs_year, variables = acs_vars,
          state = '06', county = counties[[i]], output = 'wide', moe_level = moe_level)
      ), silent = TRUE)
      if(class(table1)[[1]] != 'try-error'){
        counter = counter + 1
        break
      }
      if(Sys.time() - timer > 240) stop("Census download timed out")
    }

    raw_acs_list[[i]] <- table1
  }
  }

  #rename and create derived variables

  if(geo == 'block group') raw_acs_table <- dplyr::bind_rows(raw_acs_list)
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
      # adding college students in poverty
      college_pov_ = B14006_009E + B14006_010E,
      college_tot_ = B14007_017E + B14007_018E,
      # calculate the total pct of college students in the total pouplation
      pct_college_ = college_tot_/total_pop_,
      #poverty variables
      tot_pop_pov_  =C17002_001E,
      moe_tot_pop_pov_  = C17002_001M,
      # use census variable for poverty, unless college students are more
      #   than 25% of the population, in which case subtract college students
      #   who are below the poverty level from the poverty figures.
      prelim_below_pov_ = B17020_002E,
      below_pov_ = ifelse(pct_college_ < 0.25,
        prelim_below_pov_, prelim_below_pov_ - college_pov_),
      moe_below_pov_  = B17020_002M, ### re-calculate this
      above_200_pov_  = C17002_008E,
      moe_above_200_pov_  = C17002_008M,
      pct_below_pov_  = below_pov_/tot_pop_pov_,
      pct_above_200_pov_  = above_200_pov_/tot_pop_pov_, ### Change to above POV later
      # Household income variables
      med_hhincome_ = B19013_001E,
      moe_med_hhincome_ = B19013_001M,
      #home value variables
      home_value_ = B25077_001E,
      moe_home_value_ = B25077_001M,
      #education
      pct_bachelors_plus_  = (B15003_022E + B15003_023E + B15003_024E + B15003_025E)/B15003_001E,
      # county names
      county_name = sub('(Block Group \\d+, )?Census Tract \\d+\\.*\\d*, ', '', NAME),
      county_name = gsub(' County.*', '', county_name),
      # military population filter
      pct_military_ = B23025_006E/B23025_001E
    ) %>%
      # keep only relevant variables
      dplyr::select(fips = GEOID, county_name, dplyr::ends_with("_"))

  # include tract and block fips if block groups are returned
  if(geo == 'block group'){
    acs_table <- dplyr::mutate(acs_table, fips_bg = fips,
      fips = substr(fips, 1, 11))
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


    moe_pct_above_200_pov_ <-
      tidycensus::moe_prop(acs$above_200_pov_, acs$tot_pop_pov_,
        acs$moe_above_200_pov_, acs$moe_tot_pop_pov_)

    moe_pct_below_pov_ <- tidycensus::moe_prop(acs$below_pov_, acs$tot_pop_pov_,
      acs$moe_below_pov_, acs$moe_tot_pop_pov_)



    # employment MOEs
     # employment age is 20-64
      moe_employment_pop_ = acs$moe_employment_pop_
      MOE <- c(raw$B23024_005M, raw$B23024_007M, raw$B23024_012M, raw$B23024_014M, raw$B23024_020M, raw$B23024_022M, raw$B23024_027M, raw$B23024_029M)
      EST <- c(raw$B23024_005E, raw$B23024_007E, raw$B23024_012E, raw$B23024_014E, raw$B23024_020E, raw$B23024_022E, raw$B23024_027E, raw$B23024_029E)
    moe_employed_ <- tidycensus::moe_sum(MOE, EST)

    moe_pct_employed_ <-
      tidycensus::moe_prop(acs$employed_, acs$employment_pop_,
        moe_employed_, moe_employment_pop_)


    # Education MOE; note the first is not included in the final dataframe
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

  MOEs <- do.call(rbind, MOE_list)

  #remove MOEs that were replaced in the lapply call
  # acs_table <- dplyr::select(acs_table, -moe_below_pov_, -moe_pct_below_pov_)
  # merge table with calculated MOEs
  acs_table <- cbind(acs_table, MOEs)





  # add derived ACS race vars for 2023 to be used in the seg filter
    # race percentages
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
