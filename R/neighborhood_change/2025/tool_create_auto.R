# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen
# Date created: July 8, 2024
# Last revised: Sep 16, 2024
# Project: AFFH
# Subproject: 2023 Neighborhood Change Workplan Research
# Re: Create neighborhood change tool/overlay (automated)
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script uses the universal database (2020 tract boundaries) to create
# the neighborhood change preferred approach overlay. This version of the
# script automates the process for easy annual updating

# Inputs:
# 2020 universal database (tracts)
# Misc. ACS/census data for reliability checks

# Outputs:
# Preferred approach CSV
# Preferred approach GPKG

# Update log:
# 7/9/24: added 1/4 mile buffer
# 8/12/24: switch from county to region, high-income HHs to median income
# 9/16/24: address Quinn's review comments

# Setup -------------------------------------------------------------------

# Packages:
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)

# Directories:
homedir <- paste0(here::here(),'/R/neighborhood_change/2025')
workdir <- "/Data/"
savedir <- "/Results/Neighborhood Change Layer/"
setwd(homedir)

# Parameters
todays_date <- format(Sys.Date(), "%m%d%Y") %>% as.character()
year_var <- 2022 ## Change end year here

# Import data:

## Universal dataset (2020 boundaries)
tract2020_df <- read_csv(paste0(homedir, "/Results/tract2020_09162024.csv"))

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

## Rural areas
ruralareas_df <- read_csv(paste0(homedir, workdir, "tract_county_region.csv"))

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
  select(tract = GEOID) %>%
  filter(str_detect(tract, "^06")) %>%
  st_transform(crs = st_crs(3488))

# Main Script -------------------------------------------------------------

## Prepare spatial data (manual) ------------------------------------------

ca_counties <-
  counties(state = "CA") %>%
  select(county = COUNTYFP, county_name = NAME) %>%
  st_drop_geometry()

ca_tracts <-
  tracts(state = "CA", year = 2021) %>%
  select(tract2020 = GEOID) %>%
  # join with rural classification flags (produced separately)
  left_join(ruralareas_df %>% select(tract2020 = fips, rural_flag))

# Use population-weighted tract centroids (NHGIS)
buffer_tract_popcenter <-
  tract_popcenter %>%
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

## Create tool (automated) --------------------------------------------
# NOTE: Automated below

### Data reliability checks --------------------------------------------

### Read in data
## End year Race/Ethnicity estimates (reliability check)
raceethend_df <-
  get_acs(
    geography = "tract",
    state = "CA",
    table = "B03002",
    year = year_var,
    cache_table = TRUE
  ) %>%
  filter(variable == "B03002_001") %>%
  select(geography = GEOID, estimate_total = estimate,
         margin_of_error_total = moe)

## End year income estimates (reliability check)
incomeend_df <-
  get_acs(
    geography = "tract",
    state = "CA",
    table = "B19001",
    year = year_var,
    cache_table = TRUE
  ) %>%
  filter(variable == "B19001_001") %>%
  select(geography = GEOID, estimate_total = estimate,
         margin_of_error_total = moe)

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

## Remove college/grad students (at least 25% of tract)

## Test CV on end year estimates (since pre-2020 estimates are xwalked)

# Race CVs
race_cv <-
  raceethend_df %>%
  mutate(
    se = margin_of_error_total/1.645,
    cv = (se / estimate_total) * 100,
    # flag tracts with unreliable race/ethnicity data
    unreliable_pop = if_else(cv > 30, 1, 0)
  ) %>%
  select(tract = geography, unreliable_pop)

# Income CVs
inc_cv <-
  incomeend_df %>%
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

pop_var <- paste0("trct_pop_total_", year_var)

pop_density_df <-
  tract2020_df %>%
  select(tract2020, total_pop = all_of(pop_var)) %>%
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
  # read in end year army data from census API
  get_acs(
    geography = "tract",
    variables = c("B23025_001", "B23025_006"),
    state = "CA",
    year = year_var,
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

## Areas where the college/grad student population is at least 25% of the total
## population in end year
### Student population data from ACS table S1401
college_df <-
  get_acs(
    geography = "tract",
    variables = "S1401_C01_010",
    state = "CA",
    year = year_var
  ) %>%
  select(tract2020 = GEOID, collegepop = estimate) %>%
  left_join(tract2020_df %>% select(tract2020, total_pop = all_of(pop_var))) %>%
  transmute(
    tract = tract2020,
    college_flag = if_else(collegepop / total_pop >= .25, 1, 0)
  ) %>%
  distinct()

## Join all exclusions together
exclusions_df <-
  race_cv %>%
  left_join(inc_cv) %>%
  left_join(prisoner_df) %>%
  left_join(pop_density_df) %>%
  left_join(army_df) %>%
  left_join(college_df) %>%
  # create single final exclusion flag
  rowwise(tract) %>%
  mutate(exclusion_flag = sum(c_across(unreliable_pop:college_flag))) %>%
  ungroup() %>%
  mutate(exclusion_flag = if_else(exclusion_flag > 0, 1, exclusion_flag)) %>%
  select(tract, exclusion_flag)

### Criteria 1 (race/ethnicity) ---------------------------------------------

## 1.	In 2000, tracts where the % of the population that is NH white is below
## the region’s % of the population that is NH white, and
## where the median income is below 120% of the region median income, and
## is in a non-rural area; and

## 2.	Between 2000 and end year, tracts that experienced a percentage point increase
## in the NH white population within the top half (50%) of regionwide increases

### NOTE: repeat above conditions for 2013-end year time period to help filter the
### Criteria 3 requirements below.

raceeth_df <-
  tract2020_df %>%
  # select needed variables
  select(
    tract2020, region, county, county_name, rural_flag, trct_pct_nhwhite_2000,
    region_pct_nhwhite_2000, trct_pct_nhwhite_2013, region_pct_nhwhite_2013,
    trct_pct_nhwhite_end = paste0("trct_pct_nhwhite_", year_var),
    region_pct_nhwhite_end = paste0("region_pct_nhwhite_", year_var),
    trct_medinc_2000, trct_medinc_2013, region_medinc_2000, region_medinc_2013
  ) %>%
  distinct() %>%
  # find percentage point change by tract (2000-end and 2013-end)
  mutate(
    trct_raceeth_chng00end = trct_pct_nhwhite_end - trct_pct_nhwhite_2000,
    trct_raceeth_chng13end = trct_pct_nhwhite_end - trct_pct_nhwhite_2013
  ) %>%
  # determine distribution of racial/ethnic change by region
  group_by(region) %>%
  mutate(
    # distribution only for positive increase in NH white (2000-end)
    temp = if_else(trct_raceeth_chng00end < 0, NA_real_, trct_raceeth_chng00end),
    # determine 50% threshold by region (set to 0 for regions w/ NA values)
    raceeth_half00end = quantile(temp, probs = .5, na.rm = TRUE),
    raceeth_half00end = replace_na(raceeth_half00end, 0),

    # repeat above for 2013-end period (50th and 75th pct)
    temp = if_else(trct_raceeth_chng13end < 0, NA_real_, trct_raceeth_chng13end),
    raceeth_half13end = quantile(temp, probs = .5, na.rm = TRUE),
    raceeth_half13end = replace_na(raceeth_half13end, 0),
    raceeth_quarter13end = quantile(temp, probs = .75, na.rm = TRUE),
    raceeth_quarter13end = replace_na(raceeth_quarter13end, 0)
  ) %>%
  ungroup() %>%
  select(-temp) %>%
  # generate flags
  mutate(
    # 2000-end
    ## Expanded baseline definition
    # condition 1
    baseline_raceinc00end =
      if_else(
        (trct_pct_nhwhite_2000 < region_pct_nhwhite_2000) &
          (trct_medinc_2000 <= (region_medinc_2000 * 1.2)) &
          rural_flag == 0, 1, 0
      ),
    # if NA, assume condition not met
    baseline_raceinc00end = replace_na(baseline_raceinc00end, 0),

    # create baseline race for future use
    baseline_race00end =
      if_else(
        trct_pct_nhwhite_2000 < region_pct_nhwhite_2000 & rural_flag == 0, 1, 0
      ),

    # condition 2
    change_race00end =
      if_else(
        baseline_raceinc00end == 1 &
          trct_raceeth_chng00end >= raceeth_half00end,
        1, 0
      ),

    # 2013-end
    ## condition 1
    baseline_race13end =
      if_else(
        trct_pct_nhwhite_2013 < region_pct_nhwhite_2013 & rural_flag == 0, 1, 0
      ),
    ## if NA, assume condition not met
    baseline_race13end = replace_na(baseline_race13end, 0),

    ## condition 2a
    change_race13end =
      if_else(
        baseline_race13end == 1 & trct_raceeth_chng13end >= raceeth_half13end,
        1, 0
      ),

    ## condition 2b
    change_race13end_75pct =
      if_else(
        baseline_race13end == 1 & trct_raceeth_chng13end >= raceeth_quarter13end,
        1, 0
      )
  ) %>%
  select(
    # descriptive data
    tract2020, region, county, county_name, rural_flag,
    # tract race/ethnicity
    trct_pct_nhwhite_2000, trct_pct_nhwhite_2013, trct_pct_nhwhite_end,
    trct_raceeth_chng00end, trct_raceeth_chng13end,
    # regionwide thresholds
    raceeth_half00end, raceeth_half13end, raceeth_quarter13end,
    # region race/ethnicity
    region_pct_nhwhite_2000, region_pct_nhwhite_2013, region_pct_nhwhite_end,
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
  ) %>%
  distinct()

### Criteria 2 (income) -------------------------------------------------------

## 1.	In 2000, tracts where the median income is below 120% of the region median
## income, and
## where the % of the population that is NH white is below the region’s % of the
## population that is NH white, and
## is in a non-rural area; and

## 2.	Between 2000 and end year, tracts that experienced a percent increase
## in median household income within the top half (50%) of regionwide increases

### NOTE: repeat above conditions for 2013-end time period to help filter the
### Criteria 3 requirements below.

income_df <-
  tract2020_df %>%
  # select needed variables
  select(
    tract2020, county, county_name, region, rural_flag,
    contains("totalhh"), contains("medinc"), trct_pct_nhwhite_2000,
    trct_pct_nhwhite_2013, region_pct_nhwhite_2000, region_pct_nhwhite_2013) %>%
  rename(
    trct_totalhh_end = paste0("trct_totalhh_", year_var),
    trct_medinc_end = paste0("trct_medinc_", year_var),
    region_medinc_end = paste0("region_medinc_", year_var)
  ) %>%
  distinct() %>%
  # find % change in median income
  mutate(
    # tract level change
    trct_medinc_pctchng_00end =
      (trct_medinc_end - trct_medinc_2000) / trct_medinc_2000,
    trct_medinc_pctchng_13end =
      (trct_medinc_end - trct_medinc_2013) / trct_medinc_2013,
  ) %>%
  # determine distribution of income change by region
  group_by(region) %>%
  mutate(
    # 2000-end
    temp =
      if_else(trct_medinc_pctchng_00end < 0, NA_real_, trct_medinc_pctchng_00end),
    medinc_half00end = quantile(temp, probs = .5, na.rm = TRUE),
    medinc_half00end = replace_na(medinc_half00end, 0),

    # 2013-end (50th and 75th pct)
    temp =
      if_else(trct_medinc_pctchng_13end < 0, NA_real_, trct_medinc_pctchng_13end),
    medinc_half13end = quantile(temp, probs = .5, na.rm = TRUE),
    medinc_half13end = replace_na(medinc_half13end, 0),
    medinc_quarter13end = quantile(temp, probs = .75, na.rm = TRUE),
    medinc_quarter13end = replace_na(medinc_quarter13end, 0)
  ) %>%
  ungroup() %>%
  select(-temp) %>%
  mutate(
    # 2000-end
    # ## condition 1
    baseline_income00end =
      if_else(
        trct_medinc_2000 <= (region_medinc_2000 * 1.2) & rural_flag == 0, 1, 0
      ),
    ## if NA, assume condition not met
    baseline_income00end = replace_na(baseline_income00end, 0),

    ## Expanded baseline definition
    ## condition 1
    baseline_raceinc00end =
      if_else(
        (trct_pct_nhwhite_2000 < region_pct_nhwhite_2000) &
          (trct_medinc_2000 <= (region_medinc_2000 * 1.2)) &
          rural_flag == 0, 1, 0
      ),
    ## if NA, assume condition not met
    baseline_raceinc00end = replace_na(baseline_raceinc00end, 0),

    ## condition 2
    change_income00end =
      if_else(
        baseline_raceinc00end == 1 &
          trct_medinc_pctchng_00end >= medinc_half00end,
        1, 0
      ),

    # 2013-end
    ## condition 1
    baseline_income13end =
      if_else(
        trct_medinc_2013 <= (region_medinc_2013 * 1.2) & rural_flag == 0, 1, 0
      ),
    ## if NA, assume condition not met
    baseline_income13end = replace_na(baseline_income13end, 0),

    ## condition 2a
    change_income13end =
      if_else(
        baseline_income13end == 1 &
          trct_medinc_pctchng_13end >= medinc_half13end,
        1, 0
      ),

    ## condition 2b
    change_income13end_75pct =
      if_else(
        baseline_income13end == 1 &
          trct_medinc_pctchng_13end >= medinc_quarter13end,
        1, 0
      )
  ) %>%
  select(
    # descriptive data
    tract2020, county, county_name, region, rural_flag,
    # tract income level distribution/median incomes
    trct_medinc_2000, trct_medinc_2013, trct_medinc_end,
    trct_medinc_pctchng_00end, trct_medinc_pctchng_13end,
    # region median incomes
    region_medinc_2000, region_medinc_2013, region_medinc_end,
    # regionwide thresholds
    medinc_half00end, medinc_half13end, medinc_quarter13end,
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
  ) %>%
  distinct()

### Criteria 3 (rapidly rising rents & home value gap) ------------------------
rent_df <-
  tract2020_df %>%
  select(
    tract2020, county, county_name, region, rural_flag, contains("medrent")
  ) %>%
  distinct() %>%
  rename(
    trct_medrent_end = paste0("trct_medrent_", year_var),
    region_medrent_end = paste0("region_medrent_", year_var)
  ) %>%
  # find % increase in median rent
  mutate(
    # 2013-end
    trct_pctchng_medrent13end =
      (trct_medrent_end - trct_medrent_2013) / trct_medrent_2013,
    region_pctchng_medrent13end =
      (region_medrent_end - region_medrent_2013) / region_medrent_2013,
  ) %>%
  # create median rent increase threshold by region (top 75%)
  group_by(region) %>%
  mutate(
    # 2013-end (75%)
    temp =
      if_else(
        trct_pctchng_medrent13end < 0, NA_real_, trct_pctchng_medrent13end
      ),
    rent_quarter13end = quantile(temp, probs = .75, na.rm = TRUE),
    rent_quarter13end = replace_na(rent_quarter13end, 0),

    # 2013-end (50%)
    rent_half13end = quantile(temp, probs = .50, na.rm = TRUE),
    rent_half13end = replace_na(rent_half13end, 0)
  ) %>%
  ungroup() %>%
  select(-temp) %>%
  # join with income data to get median income data
  left_join(
    income_df %>%
      select(tract2020, trct_medinc_2013, region_medinc_2013, baseline_income13end)
  ) %>%
  mutate(
    # flag if meets Criteria 3
    medrent_disp13end =
      if_else(
        # Condition 1 (baseline)
        trct_medinc_2013 <= (region_medinc_2013 * 1.2) & rural_flag == 0 &
          # Condition 2 (change)
          trct_pctchng_medrent13end >= rent_half13end,
        1, 0
      )
  ) %>%
  select(
    # descriptive data
    tract2020, county, county_name, region,
    # tract level rent data
    trct_medrent_2013, trct_medrent_end, trct_pctchng_medrent13end,
    # region level rent data
    region_medrent_2013, region_medrent_end, region_pctchng_medrent13end,
    # regionwide threshold
    rent_half13end,
    # criteria 3 flag
    medrent_disp13end
  ) %>%
  distinct() %>%
  # join with tract exclusion data
  left_join(exclusions_df %>% rename(tract2020 = tract)) %>%
  # Exclude certain tracts (set value to NA)
  mutate(
    medrent_disp13end = if_else(exclusion_flag == 1, NA_real_, medrent_disp13end)
  ) %>%
  distinct()

# Recreate modified version of home value-income gap
## Percentile rank of tract median income (compared to region)
inc_pct <-
  tract2020_df %>%
  distinct() %>%
  filter(!is.na(county)) %>%
  select(
    tract2020, region, contains("trct_medinc"), contains("trct_pop_total")
  ) %>%
  distinct() %>%
  pivot_longer(
    contains("trct_medinc"), names_to = "year", values_to = "medinc"
  ) %>%
  pivot_longer(
    contains("trct_pop_total"), names_to = "year_copy", values_to = "poptotal"
  ) %>%
  mutate(
    year = str_extract(year, "\\d{4}$"),
    year_copy = str_extract(year_copy, "\\d{4}$")
  ) %>%
  filter(year == year_copy) %>%
  select(-year_copy) %>%
  distinct() %>%
  group_by(region, year) %>%
  arrange(region, year, medinc) %>%
  mutate(
    income_percentile =
      if_else(
        !is.na(medinc),
        lag(cumsum(poptotal), default = 0)/(sum(poptotal) - 1),
        NA_real_
      )
  ) %>%
  ungroup() %>%
  distinct()

## Percentile rank of tract median house value
hval_pct <-
  tract2020_df %>%
  filter(!is.na(county)) %>%
  select(
    tract2020, region, contains("trct_medhval"), contains("trct_ownhu")
  ) %>%
  pivot_longer(
    contains("trct_medhval"), names_to = "year", values_to = "medhval"
  ) %>%
  pivot_longer(
    contains("trct_ownhu"), names_to = "year_copy", values_to = "ownhu"
  ) %>%
  mutate(
    year = str_extract(year, "\\d{4}$"),
    year_copy = str_extract(year_copy, "\\d{4}$")
  ) %>%
  filter(year == year_copy) %>%
  select(-year_copy) %>%
  distinct() %>%
  group_by(region, year) %>%
  arrange(region, year, medhval) %>%
  mutate(
    hval_percentile =
      if_else(
        !is.na(medhval),
        lag(cumsum(ownhu), default = 0)/(sum(ownhu) - 1),
        NA_real_
      )
  ) %>%
  ungroup() %>%
  distinct()

hval_gap <-
  inc_pct %>%
  left_join(hval_pct) %>%
  mutate(year = as.double(year)) %>%
  # filter to end year
  filter(year == year_var) %>%
  mutate(pct_gap = hval_percentile - income_percentile) %>%
  select(tract2020, income_percentile, hval_percentile, pct_gap) %>%
  mutate(hvalinc_gap = if_else(pct_gap >= .25, 1, 0))

### Pathway 1a (Criteria 1 and 2, 2000-end) ------------------------------------

## A census tract that meets both Criteria 1 (racial/ethnic change) and
## Criteria 2 (economic change) between 2000-end

nbrhood_chng_df <-
  # join all dataframes together
  raceeth_df %>%
  left_join(income_df) %>%
  left_join(rent_df) %>%
  left_join(hval_gap) %>%
  # flag if a tract meets part 1 (meets Criteria 1 & meets Criteria 2)
  mutate(
    pathway1a =
      if_else(
        trct_raceeth_chng00end >= raceeth_half00end &
          trct_medinc_pctchng_00end >= medinc_half00end &
          (trct_pct_nhwhite_2000 < region_pct_nhwhite_2000) &
          (trct_medinc_2000 <= (region_medinc_2000 * 1.2)) &
          rural_flag == 0,
        1, 0
      ),
    # Exclude certain tracts (set value to NA)
    pathway1a = if_else(exclusion_flag == 1, NA_real_, pathway1a)
  )

### Pathway 2 (Proximity and Criteria 3, Criteria 1 or 2) -----------------------

## 2.	A census tract that is within ½ mile (population-weighted) of a tract that
## meets Pathway 1a and meets Criteria 3 (rapidly rising rents or home value/income gap),
## and Criteria 1 or 2 between 2013-2021

# Create 1/2-mile buffer (use pop-weighted)
#sf_use_s2(FALSE)
halfmile_buffer <-
  nbrhood_chng_df %>%
  st_drop_geometry() %>%
  select(tract = tract2020, pathway1a) %>%
  # filter to tracts that meet pathway 1a
  filter(pathway1a == 1) %>%
  # join with population centers
  left_join(buffer_tract_popcenter) %>%
  st_as_sf() %>%
  st_transform(crs = 3488) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(halfmile_buffer = 1)

nbrhood_chng_df <-
  nbrhood_chng_df %>%
  st_drop_geometry() %>%
  left_join(tract_popcenter, by = c("tract2020" = "tract")) %>%
  st_as_sf() %>%
  st_transform(crs = st_crs(halfmile_buffer)) %>%
  # overlap with buffer to identify tracts within buffer
  st_join(halfmile_buffer) %>%
  st_drop_geometry() %>%
  mutate(
    # create flag if tract is within buffer
    halfmile_buffer = replace_na(halfmile_buffer, 0),

    # flag tracts that meet Pathway 2
    pathway2 =
      if_else(
        # within 1/2 mile & meets Criteria 3 AND 1 or 2 (2013-end)
        (halfmile_buffer == 1 &
           ((medrent_disp13end == 1 | hvalinc_gap == 1) &
              ((trct_raceeth_chng13end >= raceeth_half13end &
                  baseline_race13end == 1) |
                 (trct_medinc_pctchng_13end >= medinc_half13end &
                    baseline_income13end == 1))
           )
        ) & rural_flag == 0,
        1, 0
      ),
    # Exclude certain tracts (set value to NA)
    pathway2 = if_else(exclusion_flag == 1, NA_real_, pathway2)
  )

### Pathway 1b (Criteria 1 and 2, 2013-end) -----------------------------------
nbrhood_chng_df <-
  nbrhood_chng_df %>%
  mutate(
    pathway1b =
      if_else(
        (baseline_race13end == 1 & baseline_income13end == 1) &
          (trct_raceeth_chng13end >= raceeth_quarter13end &
             trct_medinc_pctchng_13end >= medinc_quarter13end) &
          rural_flag == 0,
        1, 0
      ),
    pathway1b = if_else(exclusion_flag == 1, NA_real_, pathway1b),

    # create neighborhood change definition flag
    nbrhood_chng =
      if_else(pathway1a == 1 | pathway1b == 1 | pathway2 == 1, 1, 0)
  ) %>%
  # join back with other useful demographic data
  left_join(
    tract2020_df %>%
      select(
        tract2020, region, trct_totalhh_end = paste0("trct_totalhh_", year_var),
        trct_pop_total_end = paste0("trct_pop_total_", year_var),
        trct_pop_total_end = paste0("trct_pop_total_", year_var),
        trct_medrent_end = paste0("trct_medrent_", year_var),
        region_medrent_end = paste0("region_medrent_", year_var),
        trct_medrent_2013, region_medrent_2013,
        trct_medinc_end = paste0("trct_medinc_", year_var),
        region_medinc_end = paste0("region_medinc_", year_var),
        trct_medinc_2000, trct_medinc_2013,
        matches("trct_pct"),
        matches("trct_pop_\\w+_2000"), matches("trct_pop_\\w+_2013"),
        matches(paste0("trct_pop_\\w+_", year_var)),
        matches("trct_pct_\\w+_2000"), matches("trct_pct_\\w+_2013"),
        matches("trct_pct_\\w+_end"),
        matches("region_pop_\\w+_2000"), matches("region_pop_\\w+_2013"),
        matches("region_pop_\\w+_end"),
        matches("region_pct_\\w+_2000"), matches("region_pct_\\w+_2013"),
        matches("region_pct_\\w+_end"),
        trct_totalhh_2000, trct_totalhh_2013
      ) %>%
      select(-paste0("trct_pct_nhwhite_", year_var))
  ) %>%
  rename_with(~str_replace(., "_end$", paste0("_", year_var))) %>%
  rename_with(
    ~str_replace(., "end$", str_sub(year_var, start = 3, end = 4))
  ) %>%
  st_drop_geometry() %>%
  distinct() %>%
  left_join(ca_tracts %>% select(tract2020)) %>%
  st_as_sf()

# Save Results ------------------------------------------------------------
nbrhood_chng_df %>%
  st_drop_geometry() %>%
  write_csv(
    paste0(
      homedir, savedir, "nbrchange_", year_var, "end_", todays_date, ".csv"
    )
  )

nbrhood_chng_df %>%
  write_sf(
    paste0(
      homedir, savedir, "nbrchange_", year_var, "end_", todays_date, ".gpkg"
    ),
    overwrite = TRUE
  )
