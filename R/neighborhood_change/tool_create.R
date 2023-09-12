# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Aug. 4, 2023                
# Last revised: Aug. 28, 2023                 
# Project: AFFH    
# Subproject: 2023 Neighborhood Change Workplan Research
# Re: Create neighborhood change tool/overlay
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script uses the universal database (2020 tract boundaries) to create
# the neighborhood change preferred approach overlay.

# Inputs:
# 2020 universal database (tracts)
# Misc. ACS/census data for reliability checks

# Outputs:
# Preferred approach CSV
# Preferred approach GPKG

# Update log: 
# 8/28/23 - in part 1 of the definition, explicitly flag for both BIPOC and LMI
# in the base year (e.g., 2000)
# 8/28/23 - replace the Census-designated place 10-mile buffer filter with the 
# rural areas filter

# Setup -------------------------------------------------------------------

# Packages: 
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)

# Directories: 
homedir <- here::here()
workdir <- "/Data/"
savedir <- "/Results/Neighborhood Change Layer/"
setwd(homedir)

# Import data: 

## Universal dataset (2020 boundaries)
tract2020_df <-
  read_csv(paste0(homedir, "/Results/tract2020.csv"))

## 2021 Race/Ethnicity estimates (reliability check)
raceeth21_df <-
  read_csv(
    paste0(homedir, workdir, "ACS/Tract/Race/ACSDT5Y2021.B03002-Data.csv"),
    skip = 1
  ) %>% 
  rename_with(
    ~ str_replace_all(., "\\!\\!", "_") %>% 
      str_replace_all(" ", "_") %>% 
      str_remove_all("\\:") %>% 
      str_remove_all("\\,") %>% 
      str_to_lower()
  ) %>% 
  select(geography, estimate_total, margin_of_error_total) %>% 
  mutate(geography = str_remove(geography, "^1400000US"))

## 2021 income estimates (reliability check)
income21_df <-
  read_csv(
    paste0(homedir, workdir, "ACS/Tract/HH Income/ACSDT5Y2021.B19001-Data.csv"),
    skip = 1
  ) %>% 
  rename_with(
    ~ str_replace_all(., "\\!\\!", "_") %>% 
      str_replace_all(" ", "_") %>% 
      str_remove_all("\\:") %>% 
      str_remove_all("\\,") %>% 
      str_to_lower()
  ) %>% 
  select(geography, estimate_total, margin_of_error_total) %>% 
  mutate(geography = str_remove(geography, "^1400000US"))

## Prisoner population - 2020 (exclusion)
prisoner_df <-
  read_csv(
    paste0(
      homedir, workdir, "ACS/Tract/Group Quarters/DECENNIALPL2020.P5-Data.csv"
    ),
    skip = 1
  )

## Total population - 2020 (exclusion)
totalpop_df <-
  read_csv(
    paste0(homedir, workdir, "Decennial/Tract/Race/DECENNIALPL2020.P2-Data.csv"),
    skip = 1
  ) %>% 
  transmute(
    tract = str_remove(Geography, "^1400000US"),
    total_pop = `!!Total:`
  )

# Rural areas
ruralareas_df <- read_csv(paste0(homedir, workdir, "tract_county_region.csv"))

# ## Principal cities buffer (created separately)
# # NOTE: use until rural areas classification is available
# principal_cities_buffer <-
#   read_sf(paste0(homedir, workdir, "/buffered_principal_cities.gpkg"))

## Population centers of census tracts
tract_popcenter <-
  read_sf(
    paste0(
      homedir, workdir, "/NHGIS Crosswalks/", 
      "nhgis0009_shapefile_cenpop2020_us_tract_cenpop_2020/", 
      "US_tract_cenpop_2020.shp"
    )
  ) %>% 
  st_as_sf() %>% 
  select(tract = GEOID)

# Parameters
#todays_date <- format(Sys.Date(), "%m%d%Y") %>% as.character()

# Main Script -------------------------------------------------------------

## Prepare spatial data ---------------------------------------------------

ca_counties <- 
  counties(state = "CA") %>% 
  select(county = COUNTYFP, county_name = NAME) %>% 
  st_drop_geometry()

ca_tracts <- 
  tracts(state = "CA", year = 2021) %>% 
  select(tract2020 = GEOID) %>% 
  # join with rural classification flags (produced separately)
  left_join(ruralareas_df %>% select(tract2020 = fips, rural_flag))

# ## Identify tracts within 10 mi of a principal city (replace w/ rural areas
# ## designation when available)
# tracts_intersect <- ca_tracts %>% st_intersection(principal_cities_buffer)

# ca_tracts <-
#   ca_tracts %>% 
#   mutate(
#     within_10mi = if_else(tract2020 %in% tracts_intersect$tract2020, 1, 0)
#   ) %>% 
#   select(tract2020, within_10mi, geometry) %>% 
#   st_as_sf()

# Use population-weighted tract centroids (NHGIS)
buffer_tract_popcenter <-
  tract_popcenter %>% 
  filter(str_detect(tract, "^06")) %>% 
  st_transform(crs = st_crs(3488)) %>% 
  # create buffer (804.672 meters = 1/2-mile)
  st_buffer(804.672) %>% 
  st_transform(crs = st_crs(ca_tracts))

# add geographic data to the universal dataset
tract2020_df <- 
  tract2020_df %>% 
  left_join(ca_counties) %>% 
  left_join(ca_tracts) %>% 
  # remove empty tract data (does not contain actual data)
  filter(!is.na(tract2020))

rm(ca_counties)

## Data reliability checks --------------------------------------------

# Exclude unreliable tracts, missing data tracts, or otherwise excluded tracts 
# (e.g., prisoner population) based on existing Opportunity Map methodology:

## In addition to instances where estimates were not reported or were missing
## at the tract level, the research partners also identified “unreliable” data 
## points due to sample size limitations in the American Community Survey (ACS). 
## An ACS-derived indicator was deemed unreliable if its coefficient of 
## variation (the ratio of the standard error to the estimate) was greater than 
## 30 percent. In those instances, the estimates were suppressed. 
## Standard errors are calculated using 90% margins of error from the ACS data 
## tables.

## Test CV on 2021 estimates (since pre-2020 estimates are xwalked)

# Race CVs
race_cv <-
  raceeth21_df %>% 
  mutate(
    se = margin_of_error_total/1.645,
    cv = (se / estimate_total) * 100,
    # flag tracts with unreliable race/ethnicity data
    unreliable_pop = if_else(cv > 30, 1, 0)
  ) %>% 
  select(tract = geography, unreliable_pop)

# Income CVs
inc_cv <-
  income21_df %>% 
  mutate(
    se = margin_of_error_total/1.645,
    cv = (se / estimate_total) * 100,
    # flag tracts with unreliable income data
    unreliable_hh = if_else(cv > 30, 1, 0)
  ) %>% 
  select(tract = geography, unreliable_hh)

## Areas where prisoners make up at least 75% of the population; 
## Prisoner population taken from 2020 Census table P5 

prisoner_df <-
  prisoner_df %>% 
  rename_with(
    ~ str_replace_all(., "\\!\\!", "_") %>% 
      str_replace_all(" ", "_") %>% 
      str_remove_all("\\:") %>% 
      str_remove_all("\\,") %>% 
      str_to_lower()
  ) %>% 
  transmute(
    tract = str_remove(geography, "^1400000US"),
    prisoner_pop = 
      `_total_institutionalized_population_correctional_facilities_for_adults`
  ) %>% 
  left_join(totalpop_df) %>% 
  mutate(
    prisoner_pct = prisoner_pop / total_pop,
    prisoner_flag = if_else(prisoner_pct >= .75, 1, 0)
  ) %>% 
  select(tract, prisoner_flag)

## Areas with population density below 15 people per square mile and total 
## population below 500; and

pop_density_df <-
  tract2020_df %>% 
  select(tract2020, total_pop = trct_pop_total_2021) %>% 
  # join with tract shapefile to estimate density
  left_join(ca_tracts %>% select(-rural_flag)) %>% 
  st_as_sf() %>% 
  # 1 sq mile = 2589988.1103 sq meters
  mutate(
    trct_area = as.double(st_area(geometry)) / 2589988.1103,
    pop_density = total_pop / trct_area,
    pop_density_flag = 
      if_else(total_pop < 500 & pop_density < 15, 1, 0)
  ) %>% 
  st_drop_geometry() %>% 
  select(tract = tract2020, pop_density_flag)

## Areas where at least half of the age 16+ population is employed by the armed 
## forces, in order to exclude military base areas where it is not possible to 
## develop non-military affordable housing.
### armed forces data from ACS table B23025

army_df <-
  # read in 2021 army data from census API
  get_acs(
    geography = "tract",
    variables = c("B23025_001", "B23025_006"),
    state = "CA",
    year = 2021,
    cache_table = TRUE
  ) %>% 
  select(-c(moe, NAME)) %>% 
  # rename variables
  mutate(
    variable = if_else(variable == "B23025_001", "total_pop", "army_pop")
  ) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(
    army_pct = army_pop / total_pop,
    army_flag = if_else(army_pct >= .5, 1, 0)
  ) %>% 
  select(tract = GEOID, army_flag)

## Join all exclusions together
exclusions_df <-
  race_cv %>% 
  left_join(inc_cv) %>% 
  left_join(prisoner_df) %>% 
  left_join(pop_density_df) %>% 
  left_join(army_df) %>% 
  # create single final exclusion flag
  rowwise(tract) %>% 
  mutate(exclusion_flag = sum(c_across(unreliable_pop:army_flag))) %>% 
  ungroup() %>% 
  mutate(exclusion_flag = if_else(exclusion_flag > 0, 1, exclusion_flag)) %>% 
  select(tract, exclusion_flag)

rm(army_df, inc_cv, race_cv, raceeth21_df, income21_df, pop_density_df, 
   prisoner_df, totalpop_df)
## Criteria 1 (race/ethnicity) --------------------------------------------- 

## 1.	In 2000, tracts where the % of the population that is NH white is below 
## the county’s % of the population that is NH white, and  
## where the median income is below 120% of the county median income, and 
## is in a non-rural area; and

## 2.	Between 2000 and 2021, tracts that experienced a percentage point increase
## in the NH white population within the top half (50%) of countywide increases

### NOTE: repeat above conditions for 2013-2021 time period to help filter the
### Criteria 3 requirements below.

raceeth_df <-
  tract2020_df %>%
  # select needed variables
  select(
    tract2020, region, county, county_name, rural_flag, contains("pct_nhwhite"),
    trct_medinc_2000, trct_medinc_2013, cnty_medinc_2000, cnty_medinc_2013
  ) %>% 
  distinct() %>% 
  # find percentage point change by tract and county (2000-2021 and 2013-2021)
  mutate(
    trct_raceeth_chng0021 = trct_pct_nhwhite_2021 - trct_pct_nhwhite_2000,
    cnty_raceeth_chng0021 = cnty_pct_nhwhite_2021 - cnty_pct_nhwhite_2000,
    trct_raceeth_chng1321 = trct_pct_nhwhite_2021 - trct_pct_nhwhite_2013,
    cnty_raceeth_chng1321 = cnty_pct_nhwhite_2021 - cnty_pct_nhwhite_2013
  ) %>% 
  # determine distribution of racial/ethnic change by county
  group_by(county) %>% 
  mutate(
    # distribution only for positive increase in NH white (2000-2021)
    temp = if_else(trct_raceeth_chng0021 < 0, NA_real_, trct_raceeth_chng0021),
    # determine 50% threshold by county (set to 0 for counties w/ NA values)
    raceeth_half0021 = quantile(temp, probs = .5, na.rm = TRUE),
    raceeth_half0021 = replace_na(raceeth_half0021, 0),
    
    # repeat above for 2013-2021 period
    temp = if_else(trct_raceeth_chng1321 < 0, NA_real_, trct_raceeth_chng1321),
    raceeth_half1321 = quantile(temp, probs = .5, na.rm = TRUE),
    raceeth_half1321 = replace_na(raceeth_half1321, 0)
  ) %>% 
  ungroup() %>% 
  select(-temp) %>% 
  # generate flags
  mutate(
    # 2000-2021
    # ## Condition 1
    # baseline_race0021 = 
    #   if_else(
    #     trct_pct_nhwhite_2000 < cnty_pct_nhwhite_2000 & rural_flag == 0, 1, 0
    #   ),
    # ## if NA, assume condition not met
    # baseline_race0021 = replace_na(baseline_race0021, 0),
    # 
    # ## Condition 2
    # change_race0021 = 
    #   if_else(
    #     baseline_race0021 == 1 & trct_raceeth_chng0021 >= raceeth_half0021,
    #     1, 0
    #   ),
  
    ## Expanded baseline definition
    # condition 1
    baseline_raceinc0021 =
      if_else(
        (trct_pct_nhwhite_2000 < cnty_pct_nhwhite_2000) &
          (trct_medinc_2000 <= (cnty_medinc_2000 * 1.2)) &
          rural_flag == 0, 1, 0
      ),
    # if NA, assume condition not met
    baseline_raceinc0021 = replace_na(baseline_raceinc0021, 0),

    # condition 2
    change_race0021 =
      if_else(
        baseline_raceinc0021 == 1 &
          trct_raceeth_chng0021 >= raceeth_half0021,
        1, 0
      ),
  
    # 2013-2021
    ## condition 1
    baseline_race1321 = 
      if_else(
        trct_pct_nhwhite_2013 < cnty_pct_nhwhite_2013 & rural_flag == 0, 1, 0
      ),
    ## if NA, assume condition not met
    baseline_race1321 = replace_na(baseline_race1321, 0),
    
    ## condition 2
    change_race1321 = 
      if_else(
        baseline_race1321 == 1 & trct_raceeth_chng1321 >= raceeth_half1321,
        1, 0
      )
  ) %>% 
  select(
    # descriptive data
    tract2020, region, county, county_name, 
    # tract race/ethnicity
    trct_pct_nhwhite_2000, trct_pct_nhwhite_2013, trct_pct_nhwhite_2021, 
    trct_raceeth_chng0021, trct_raceeth_chng1321, 
    # countywide thresholds
    raceeth_half0021, raceeth_half1321,
    # county race/ethnicity
    cnty_pct_nhwhite_2000, cnty_pct_nhwhite_2013, cnty_pct_nhwhite_2021, 
    # baseline flags (2000, 2013)
    starts_with("baseline"),
    # change flags (2000, 2013)
    starts_with("change")
  ) %>% 
  # join with exclusion flags
  left_join(exclusions_df %>% rename(tract2020 = tract)) %>% 
  # Exclude tracts (set to NA)
  mutate(
    across(
      .cols = c(contains("baseline"), contains("change")),
      ~if_else(exclusion_flag == 1, NA_real_, .)
    )
  ) 

## Criteria 2 (income) -------------------------------------------------------

## 1.	In 2000, tracts where the median income is below 120% of the county median 
## income, and 
## where the % of the population that is NH white is below the county’s % of the
## population that is NH white, and
## is in a non-rural area; and

## 2.	Between 2000 and 2021, tracts that experienced a percentage point 
## increase in high-income households (>120% of AMI) within the top half (50%) 
## of county-wide increases 

### NOTE: repeat above conditions for 2013-2021 time period to help filter the
### Criteria 3 requirements below.

income_df <-
  tract2020_df %>%
  # select needed variables
  select(
    tract2020, county, county_name, region, rural_flag, contains("abovemod"), 
    contains("totalhh"), contains("medinc"), trct_pct_nhwhite_2000, 
    trct_pct_nhwhite_2013, cnty_pct_nhwhite_2000, cnty_pct_nhwhite_2013
  ) %>% 
  distinct() %>% 
  # find percentage point change in high-income households
  mutate(
    # tract level change
    trct_pct_abovemod_2000 = trct_abovemod_2000 / trct_totalhh_2000,
    trct_pct_abovemod_2013 = trct_abovemod_2013 / trct_totalhh_2013,
    trct_pct_abovemod_2021 = trct_abovemod_2021 / trct_totalhh_2021,
    trct_inc_chng0021 = trct_pct_abovemod_2021 - trct_pct_abovemod_2000,
    trct_inc_chng1321 = trct_pct_abovemod_2021 - trct_pct_abovemod_2013,
    
    # county level change
    cnty_pct_abovemod_2000 = cnty_abovemod_2000 / cnty_totalhh_2000,
    cnty_pct_abovemod_2013 = cnty_abovemod_2013 / cnty_totalhh_2013,
    cnty_pct_abovemod_2021 = cnty_abovemod_2021 / cnty_totalhh_2021,
    cnty_inc_chng0021 = cnty_pct_abovemod_2021 - cnty_pct_abovemod_2000,
    cnty_inc_chng1321 = cnty_pct_abovemod_2021 - cnty_pct_abovemod_2013
  ) %>% 
  # determine distribution of income change by county
  group_by(county) %>% 
  mutate(
    # 2000-2021
    temp = if_else(trct_inc_chng0021 < 0, NA_real_, trct_inc_chng0021),
    inc_half0021 = quantile(temp, probs = .5, na.rm = TRUE),
    inc_half0021 = replace_na(inc_half0021, 0),
    
    # 2013-2021
    temp = if_else(trct_inc_chng1321 < 0, NA_real_, trct_inc_chng1321),
    inc_half1321 = quantile(temp, probs = .5, na.rm = TRUE),
    inc_half1321 = replace_na(inc_half1321, 0)
  ) %>% 
  ungroup() %>% 
  select(-temp) %>% 
  mutate(
    # 2000-2021
    # ## condition 1
    baseline_income0021 =
      if_else(
        trct_medinc_2000 <= (cnty_medinc_2000 * 1.2) & rural_flag == 0, 1, 0
      ),
    ## if NA, assume condition not met
    baseline_income0021 = replace_na(baseline_income0021, 0),
     
    # ## condition 2
    # change_income0021 = 
    #   if_else(
    #     baseline_income0021 == 1 & trct_inc_chng0021 >= inc_half0021, 1, 0
    #   ),
    
    ## Expanded baseline definition
    ## condition 1
    baseline_raceinc0021 =
      if_else(
        (trct_pct_nhwhite_2000 < cnty_pct_nhwhite_2000) &
          (trct_medinc_2000 <= (cnty_medinc_2000 * 1.2)) &
          rural_flag == 0, 1, 0
      ),
    ## if NA, assume condition not met
    baseline_raceinc0021 = replace_na(baseline_raceinc0021, 0),

    ## condition 2
    change_income0021 =
      if_else(
        baseline_raceinc0021 == 1 & trct_inc_chng0021 >= inc_half0021,
        1, 0
      ),
    
    # 2013-2021
    ## condition 1
    baseline_income1321 = 
      if_else(
        trct_medinc_2013 <= (cnty_medinc_2013 * 1.2) & rural_flag == 0, 1, 0
      ),
    ## if NA, assume condition not met
    baseline_income1321 = replace_na(baseline_income1321, 0),
    
    ## condition 2
    change_income1321 = 
      if_else(
        baseline_income1321 == 1 & trct_inc_chng1321 >= inc_half1321, 1, 0
      ),
    
    ## Expanded baseline definition
    # ## condition 1
    # baseline_raceinc1321 = 
    #   if_else(
    #     (trct_pct_nhwhite_2013 < cnty_pct_nhwhite_2013) & 
    #       (trct_medinc_2013 <= (cnty_medinc_2013 * 1.2)) &
    #       within_10mi == 1, 1, 0
    #   ),
    # ## if NA, assume condition not met
    # baseline_raceinc1321 = replace_na(baseline_raceinc1321, 0),
    # 
    # ## condition 2
    # change_income1321 = 
    #   if_else(
    #     baseline_raceinc1321 == 1 & trct_inc_chng1321 >= inc_half1321,
    #     1, 0
    #   )
  ) %>% 
  select(
    # descriptive data
    tract2020, county, county_name, region, 
    # tract income level distribution/median incomes
    trct_pct_abovemod_2000, trct_pct_abovemod_2013, trct_pct_abovemod_2021, 
    trct_medinc_2000, trct_medinc_2013, trct_medinc_2021,
    trct_inc_chng0021, trct_inc_chng1321,
    # county median incomes
    cnty_medinc_2000, cnty_medinc_2013, cnty_medinc_2021, cnty_inc_chng0021,
    cnty_inc_chng1321,
    # countywide thresholds
    inc_half0021, inc_half1321,
    # baseline flags (2000, 2013)
    starts_with("baseline"),
    # change flags (2000, 2013)
    starts_with("change")
  ) %>% 
  # join with flagged exclusion tracts
  left_join(exclusions_df %>% rename(tract2020 = tract)) %>% 
  # Exclude certain tracts (set to NA value)
  mutate(
    across(
      .cols = c(contains("baseline"), contains("change")),
      ~if_else(exclusion_flag == 1, NA_real_, .)
    )
  ) 

## Criteria 3 (rapidly rising rents) ---------------------------------------
rent_df <-
  tract2020_df %>%
  select(
    tract2020, county, county_name, region, rural_flag, contains("medrent")
  ) %>% 
  distinct() %>% 
  # find % increase in median rent
  mutate(
    # 2013-2021
    trct_pctchng_medrent1321 = 
      (trct_medrent_2021 - trct_medrent_2013) / trct_medrent_2013,
    cnty_pctchng_medrent1321 = 
      (cnty_medrent_2021 - cnty_medrent_2013) / cnty_medrent_2013,
  ) %>% 
  # create median rent increase threshold by county (top 75%)
  group_by(county) %>% 
  mutate(
    # 2013-2021 (75%)
    temp = 
      if_else(
        trct_pctchng_medrent1321 < 0, NA_real_, trct_pctchng_medrent1321
      ),
    rent_quarter1321 = quantile(temp, probs = .75, na.rm = TRUE),
    rent_quarter1321 = replace_na(rent_quarter1321, 0)
  ) %>% 
  ungroup() %>% 
  select(-temp) %>% 
  # join with income data to get median income data
  left_join(
    income_df %>% 
      select(tract2020, trct_medinc_2000, cnty_medinc_2000, baseline_income0021)
  ) %>% 
  mutate(
    # flag if meets Criteria 3
    medrent_disp1321 = 
      if_else(
        # Condition 1 (baseline)
        trct_medinc_2000 <= (cnty_medinc_2000 * 1.2) & rural_flag == 0 &
          # Condition 2 (change)
          trct_pctchng_medrent1321 >= rent_quarter1321,
        1, 0
      )
  ) %>% 
  select(
    # descriptive data
    tract2020, county, county_name, region, 
    # tract level rent data
    trct_medrent_2013, trct_medrent_2021, trct_pctchng_medrent1321, 
    # county level rent data
    cnty_medrent_2013, cnty_medrent_2021, cnty_pctchng_medrent1321,
    # countywide threshold
    rent_quarter1321,
    # criteria 3 flag
    medrent_disp1321 
  ) %>% 
  # join with tract exclusion data
  left_join(exclusions_df %>% rename(tract2020 = tract)) %>% 
  # Exclude certain tracts (set value to NA)
  mutate(
    medrent_disp1321 = if_else(exclusion_flag == 1, NA_real_, medrent_disp1321)
  ) 

## Part 1 (Criteria 1 and 2, 2000-2021) ------------------------------------

## A census tract that meets both Criteria 1 (racial/ethnic change) and 
## Criteria 2 (economic change) between 2000-2021

nbrhood_chng_df <-
  # join all dataframes together
  raceeth_df %>% 
  left_join(income_df) %>% 
  left_join(rent_df) %>% 
  # flag if a tract meets part 1 (meets Criteria 1 & meets Criteria 2)
  mutate(part1 = if_else(change_race0021 == 1 & change_income0021 == 1, 1, 0)) 

## Part 2 (Proximity and Criteria 3, Criteria 1 or 2) -----------------------

## 2.	A census tract that is within ½ mile (population-weighted) of a tract that
## meets Part 1 and meets Criteria 3 (rapidly rising rents), and Criteria 1 or 2
## between 2013-2021  

# Create 1/2-mile buffer (use pop-weighted)
#sf_use_s2(FALSE)
halfmile_buffer <-
  nbrhood_chng_df %>% 
  select(tract = tract2020, part1) %>% 
  # filter to tracts that meet Part 1
  filter(part1 == 1) %>% 
  # join with population centers
  left_join(buffer_tract_popcenter) %>% 
  st_as_sf() %>% 
  st_transform(crs = 3488) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  mutate(halfmile_buffer = 1)

nbrhood_chng_df <-
  nbrhood_chng_df %>% 
  # join with spatial boundaries & rural flag
  left_join(ca_tracts) %>% 
  st_as_sf() %>% 
  st_transform(crs = 3488) %>% 
  # overlap with buffer to identify tracts within buffer
  st_join(halfmile_buffer) %>% 
  mutate(
    # create flag if tract is within buffer
    halfmile_buffer = replace_na(halfmile_buffer, 0),
    
    # flag tracts that meet Part 2
    part2 =
      if_else(
        # within 1/2 mile & meets Criteria 3 AND 1 or 2 (2013-2021) 
        (halfmile_buffer == 1 & 
           (medrent_disp1321 == 1 & 
              (change_race1321 == 1 | change_income1321 == 1)
           )
        ) & rural_flag == 0,
        1, 0
      ),
    # Exclude certain tracts (set value to NA)
    part2 = if_else(exclusion_flag == 1, NA_real_, part2),
    
    # create neighborhood change definition flag
    nbrhood_chng = if_else(part1 == 1 | part2 == 1, 1, 0)
  ) %>% 
  # join back with other useful demographic data
  left_join(
    tract2020_df %>% 
      # create moderate income counts
      mutate(
        trct_mod_2000 = trct_lowmod_2000 - trct_lowinc_2000,
        trct_mod_2013 = trct_lowmod_2013 - trct_lowinc_2013,
        trct_mod_2021 = trct_lowmod_2021 - trct_lowinc_2021
      ) %>% 
      select(
        tract2020, region, trct_totalhh_2021, trct_pop_total_2021, 
        cnty_totalhh_2021, trct_pop_total_2021, trct_medrent_2021, 
        cnty_medrent_2021, trct_medrent_2013, cnty_medrent_2013, matches("trct_pct"),
        matches("trct_pop_\\w+_2000"), matches("trct_pop_\\w+_2013"),
        matches("trct_pop_\\w+_2021"), matches("trct_pct_\\w+_2000"),
        matches("trct_pct_\\w+_2013"), matches("trct_pop_\\w+_2021"),
        trct_lowinc_2000, trct_lowinc_2013, trct_lowinc_2021, trct_mod_2000,
        trct_mod_2013, trct_mod_2021, trct_totalhh_2000, trct_totalhh_2013
      )
  ) 

# Test: Visualize results 
# nbrhood_chng_df %>% filter(nbrhood_chng == 1) %>% mapview::mapview()

# Save Results ------------------------------------------------------------
nbrhood_chng_df %>% 
  st_drop_geometry() %>% 
  write_csv(
    paste0(homedir, savedir, "neighborhood_change_layer_2023.csv")
  )

nbrhood_chng_df %>% 
  write_sf(
    paste0(homedir, savedir, "neighborhood_change_layer_2023.gpkg"),
    overwrite = TRUE
  )
