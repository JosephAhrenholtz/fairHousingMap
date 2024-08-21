# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen
# Date created: Sept. 1, 2023
# Last revised: Aug. 12, 2024
# Project: AFFH
# Subproject: 2023 Neighborhood Change Workplan Research
# Re: Clean and prepare raw data for neighborhood change tool (streamlined version)
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script cleans and prepares raw data from ACS and other sources for use
# in the neighborhood change tool. Crosswalks 2020 census geographies to 2010
# borders and vice versa. It is a streamlined version of the original code, pared
# down to only include necessary demographic data.

# Inputs:
# ACS data
# NHGIS crosswalk
# TCAC/HCD Opportunity Map, 2023

# Outputs:
# Cleaned dataset w/ 2010 boundaries
# Cleaned dataset w/ 2020 boundaries

# Update log:
## 8/12/2024: updated to address new NCM methodology

# Setup -------------------------------------------------------------------

# Packages:
library(tidyverse)
library(glue)
library(readxl)
library(data.table)
library(sf)

# Directories:
homedir <- paste0(here::here(),'/R/neighborhood_change/2025')
workdir <- "/Data/"
savedir <- "/Results/"
setwd(homedir)

# Import data:
## NHGIS Crosswalks
xwalk_20to10 <-
  read_csv(
    paste0(homedir, workdir, "NHGIS Crosswalks/nhgis_bg2020_bg2010_06.csv")
  )
xwalk_10to20 <-
  read_csv(
    paste0(homedir, workdir, "NHGIS Crosswalks/nhgis_bg2010_bg2020_06.csv")
  )
xwalk_00to10 <-
  read_csv(
    paste0(homedir, workdir, "NHGIS Crosswalks/nhgis_bgp2000_bg2010_06.csv")
  )

# TCAC/HCD Opportunity map (most recent, for region)
tcac_df <-
  read_xlsx(
    paste0(homedir, workdir, "TCAC/draft-2024-statewide-summary-table.xlsx"),
    guess_max = 11000
  )

# Parameters
todays_date <- format(Sys.Date(), "%m%d%Y") %>% as.character()

# Main Script -------------------------------------------------------------

# Pre-processing ---------------------------------------------------------

# Prepare TCAC OppMap (for region data)
tcac_opp_df <-
  tcac_df %>%
  select(tract = `Census Tract`, bg = `Census Block Group`,
         county_name = County, region = Region) %>%
  # create a county FIPS column
  mutate(county = str_extract(tract, "^\\d{0,5}"))

rm(tcac_df)

# Reader helper functions
## Note: Table S1501 only provides % in 2011, other years has # estimates.

## ACS/decennial cleaner (works on files downloaded directly from data.census.gov)
acs_cleaner <- function(path) {
  # read in Census data
  read_csv(
    paste0(homedir, workdir, "ACS/", path),
    skip = 1,
    guess_max = 10000
  ) %>%
    # remove annotations
    select(-contains("Annotation")) %>%
    # # remove margin of error/annotations
    # select(-c(contains("Margin"), contains("Annotation"))) %>%
    # remove totally empty columns
    janitor::remove_empty(which = "cols") %>%
    # clean up column names
    rename_with(
      ~str_replace_all(., " ", "_") %>%
        str_replace_all("\\!\\!", "_") %>%
        str_remove_all("\\'") %>%
        str_remove_all("\\,") %>%
        str_remove_all("\\(") %>%
        str_remove_all("\\)") %>%
        str_remove_all("\\:") %>%
        str_replace_all("\\-", "_") %>%
        str_to_lower()
    ) %>%
    # clean up geography (tract, BG, etc.) data
    mutate(geography = str_remove(geography, "^.+US"))
}

file_reader <- function(geography, subject, ignore_years = "None") {
  # Read in and append all years of Census data

  # set years to ignore (useful if decennial data and ACS data are available,
  # tells the system not to override the decennial dataframe)
  ignore_years <- paste(ignore_years, collapse = "|")

  # clean up file geography (BG)
  object_geo <-
    if_else(geography == "Block Group", "bg", str_to_lower(geography))
  # specify file subject
  object_subj <-
    case_when(
      subject == "HH Gross Rent" ~ "rent",
      subject == "Median HH Gross Rent" ~ "medrent",
      subject == "HH Income" ~ "inc",
      subject == "Median HH Income" ~ "medinc",
      subject == "Race" ~ "race",
      subject == "Home Value" ~ "hval",
      subject == "Median Home Value" ~ "medhval"
    )
  # create full name for final dataframe
  object_name <- paste0(object_subj, "_", object_geo, "_")

  files <-
    # dynamically specify file path for desired data
    paste0(
      glue("{geography}/{subject}/"),
      list.files(
        path = glue(homedir, workdir, "ACS/{geography}/{subject}/"),
        pattern = "*-Data.csv"
      )
    )

  if (ignore_years != "None") {
    # if ignoring a year(s), remove that year(s) from the file list
    files <- files %>% str_subset(pattern = ignore_years, negate = TRUE)
  } else {
    files <- files
  }

  # read in desired dataframe and create an object in the R enviro
  list2env(
    lapply(
      setNames(
        # set the object name based on file path
        files,
        make.names(
          paste0(object_name, str_extract(files, "(?<=ACS\\w{4,5}20)\\d{2}"))
        )
      ),
      # use ACS cleaner specifications to bring in the data
      acs_cleaner
    ),
    # read to Global Environment
    envir = .GlobalEnv
  )

}

# Calculation helper functions

## Median of grouped data
### Function separately validated against medians provided by the ACS
grouped_median <-
  function(df, geography, lower_bound = lower_bound,
           upper_bound = upper_bound, observations) {
    # Estimates median values from provided groups/bands (e.g., income bands)
    # Especially useful after crosswalking

    # Prep function specifications for use
    geography <- enquo(geography)
    lower_bound <- enquo(lower_bound)
    upper_bound <- enquo(upper_bound)
    observations <- enquo(observations)

    # Generate internal median function
    CalculateMedian <- function(df, lower_bound, upper_bound, observations) {

      # Prep function specifications for use
      lower_bound <- enquo(lower_bound)
      upper_bound <- enquo(upper_bound)
      observations <- enquo(observations)

      # Calculating cumulative number of observations and n
      dataset <-
        df %>%
        select(
          lower = !!lower_bound, upper = !!upper_bound, obs = !!observations
        ) %>%
        data.table()

      dataset <- dataset[,cumobs := cumsum(obs)]
      n <- dataset[,max(cumobs)]

      if (n == 0) {
        # if no observations, set median to NA
        df <- df %>% mutate(grpmed = NA_real_)
      } else {

        # if >0 observations, find the median

        # Median of Grouped Data = L + W[(N/2 – C) / F]
        #
        # where:
        #
        # L: Lower limit of median class
        # W: Width of median class
        # N: Total Frequency
        # C: Cumulative frequency up to median class
        # F: Frequency of median class

        grpmed <- dataset[
          (cumobs - obs) <= (max(cumobs)/2) &
            cumobs >= (max(cumobs)/2),

          lower + ((upper - lower)/obs) * ((n/2) - (cumobs - obs))
        ]

        # if duplicated median for geo, take the maximum and round it
        grpmed <- max(grpmed, na.rm = TRUE) %>% round()

        # add the grouped median to the final dataset
        df <- df %>% mutate(grpmed = grpmed)

      }
      return(df)

    }

    df %>%
      # data needs to be in correct format (grouped by geo)
      group_by(!!geography) %>%
      # apply the median function to each geo
      group_map(
        ~ CalculateMedian(.x, !!lower_bound, !!upper_bound, !!observations),
        .keep = TRUE
      ) %>%
      bind_rows() %>%
      return()
  }

# County -------------------------------------------------------------------

# This section reads, cleans, and joins all data at the county level. County
# geos are the same in 2000, 2010, and 2020.

### Renter Costs
rent_county_00 <-
  acs_cleaner(
    "../Decennial/County/HH Gross Rent/DECENNIALSF32000.H062-Data.csv"
  )
file_reader("County", "HH Gross Rent")

medrent_county_00 <-
  acs_cleaner(
    "../Decennial/County/Median HH Gross Rent/DECENNIALSF32000.H063-Data.csv"
  )
file_reader("County", "Median HH Gross Rent")

### Household Income
inc_county_00 <-
  acs_cleaner("../Decennial/County/HH Income/DECENNIALSF32000.P052-Data.csv")
file_reader("County", "HH Income")

medinc_county_00 <-
  acs_cleaner(
    "../Decennial/County/Median HH Income/DECENNIALSF32000.P053-Data.csv"
  )
file_reader("County", "Median HH Income")

### Race/ethnicity (population)
race_county_00 <-
  acs_cleaner("../Decennial/County/Race/DECENNIALSF12000.P004-Data.csv")
race_county_10 <-
  acs_cleaner("../Decennial/County/Race/DECENNIALPL2010.P2-Data.csv")
race_county_20 <-
  acs_cleaner("../Decennial/County/Race/DECENNIALPL2020.P2-Data.csv")
# ignore decennial years for race/ethnicity
file_reader("County", "Race", ignore_years = c("2010", "2020"))

### Home Value
hval_county_00 <-
  acs_cleaner("../Decennial/County/Home Value/DECENNIALSF32000.H074-Data.csv")
file_reader("County", "Home Value")

medhval_county_00 <-
  acs_cleaner(
    "../Decennial/County/Median Home Value/DECENNIALDPSF32000.DP4-Data.csv"
  )
file_reader("County", "Median Home Value")

# Tract ----------------------------------------------------------------

# This section reads, cleans, and joins all data at the tract level. Tract
# geos are different in 2000, 2010, and 2020.

# ## Renter Costs
rent_tract_00 <-
  acs_cleaner("../Decennial/Tract/HH Gross Rent/DECENNIALSF32000.H062-Data.csv")
file_reader("Tract", "HH Gross Rent")

medrent_tract_00 <-
  acs_cleaner("../Decennial/Tract/Median HH Gross Rent/DECENNIALSF32000.H063-Data.csv")
file_reader("Tract", "Median HH Gross Rent")

## HH Income
inc_tract_00 <-
  acs_cleaner("../Decennial/Tract/HH Income/DECENNIALSF32000.P052-Data.csv")
file_reader("Tract", "HH Income")

medinc_tract_00 <-
  acs_cleaner("../Decennial/Tract/Median HH Income/DECENNIALSF32000.P053-Data.csv")
file_reader("Tract", "Median HH Income")

## Race/ethnicity (population)
race_tract_00 <-
  acs_cleaner("../Decennial/Tract/Race/DECENNIALSF12000.P004-Data.csv")
race_tract_10 <-
  acs_cleaner("../Decennial/Tract/Race/DECENNIALPL2010.P2-Data.csv")
race_tract_20 <-
  acs_cleaner("../Decennial/Tract/Race/DECENNIALPL2020.P2-Data.csv")
file_reader("Tract", "Race", ignore_years = c("2010", "2020"))

## Home Value
hval_tract_00 <-
  acs_cleaner("../Decennial/Tract/Home Value/DECENNIALSF32000.H074-Data.csv")
file_reader("Tract", "Home Value")

medhval_tract_00 <-
  acs_cleaner(
    "../Decennial/Tract/Median Home Value/DECENNIALDPSF32000.DP4-Data.csv"
  )
file_reader("Tract", "Median Home Value")

# Block Groups -------------------------------------------------------------

# This section reads in block group data. Block groups have different geography
# in 2010 and 2020. Block group data is used to construct tract-level
# crosswalks between decennial years.

## Renter Costs
file_reader("Block Group", "HH Gross Rent")

file_reader("Block Group", "Median HH Gross Rent")

## HH Income
file_reader("Block Group", "HH Income")

file_reader("Block Group", "Median HH Income")

## Race/ethnicity (population)
race_bg_10 <-
  acs_cleaner("../Decennial/Block Group/Race/DECENNIALPL2010.P2-Data.csv")
race_bg_20 <-
  acs_cleaner("../Decennial/Block Group/Race/DECENNIALPL2020.P2-Data.csv")
file_reader("Block Group", "Race", ignore_years = c("2010", "2020"))

## Home Value
file_reader("Block Group", "Home Value")

file_reader("Block Group", "Median Home Value")

## Block Group Parts to Block Groups -----------------------------------------

# This section reads in data on block group parts from 2000 to crosswalk 2000
# data at the tract-level between 2000, 2010, and 2020. BGP data is collected
# from the NHGIS dataset (not the Census directly).

# Note: immediately crosswalk 2000 data to 2010 block group geos

### Renter Costs (manual)
rent_bg_00 <-
  read_csv(
    paste0(
      homedir, workdir,
      "NHGIS 2000/HH Gross Rent/nhgis0003_ds152_2000_blck_grp_090.csv"
    ),
    skip = 1
  ) %>%
  # clean column names
  rename_with(
    ~str_replace_all(., " ", "_") %>%
      str_replace_all("\\!\\!", "_") %>%
      str_remove_all("\\'") %>%
      str_remove_all("\\,") %>%
      str_remove_all("\\(") %>%
      str_remove_all("\\)") %>%
      str_remove_all("\\:") %>%
      str_replace_all("\\-", "_") %>%
      str_to_lower()
  ) %>%
  # select only needed columns (geos, rent bands)
  select(bgp2000gj = gis_join_match_code, contains("$")) %>%
  # join with NHGIS crosswalk to 2010 BG
  left_join(xwalk_00to10, by = "bgp2000gj", multiple = "all") %>%
  # weigh 2000 BGP data by NHGIS weights
  mutate(across(.cols = contains("$"), ~. * wt_hh)) %>%
  # drop unneeded columns
  select(-c(contains("wt_"), ends_with("gj"))) %>%
  # summarize data at the 2010 BG level
  group_by(bg2010ge) %>%
  summarise(across(.cols = everything(), sum)) %>%
  # rename and rearrange data
  rename(bg_2010 = bg2010ge) %>%
  rename_with(.cols = 2:ncol(.), ~ paste0("rent_", .)) %>%
  select(bg_2010, everything())

### Household Income (manual)
inc_bg_00 <-
  read_csv(
    paste0(
      homedir, workdir,
      "NHGIS 2000/HH Income/nhgis0005_ds152_2000_blck_grp_090.csv"
    ),
    skip = 1
  ) %>%
  # clean column names
  rename_with(
    ~str_replace_all(., " ", "_") %>%
      str_replace_all("\\!\\!", "_") %>%
      str_remove_all("\\'") %>%
      str_remove_all("\\,") %>%
      str_remove_all("\\(") %>%
      str_remove_all("\\)") %>%
      str_remove_all("\\:") %>%
      str_replace_all("\\-", "_") %>%
      str_to_lower()
  ) %>%
  # select needed columns (geo, income bands)
  select(bgp2000gj = gis_join_match_code, contains("$")) %>%
  # join with NHGIS crosswalk to 2010 BG
  left_join(xwalk_00to10, by = "bgp2000gj", multiple = "all") %>%
  # weigh 2000 BGP data by NHGIS weights
  mutate(across(.cols = contains("$"), ~. * wt_hh)) %>%
  # remove unneeded columns
  select(-c(contains("wt_"), ends_with("gj"))) %>%
  # summarize data at the 2010 BG level
  group_by(bg2010ge) %>%
  summarise(across(.cols = everything(), sum)) %>%
  # rename and rearrange data
  rename(bg_2010 = bg2010ge) %>%
  rename_with(.cols = 2:ncol(.), ~ paste0("inc_", .)) %>%
  select(bg_2010, everything())

### Race/ethnicity (population) (manual)
race_bg_00 <-
  read_csv(
    paste0(
      homedir, workdir,
      "NHGIS 2000/Race/nhgis0008_ds152_2000_blck_grp_090.csv"
    ),
    skip = 1
  ) %>%
  # clean column names
  rename_with(
    ~str_replace_all(., " ", "_") %>%
      str_replace_all("\\!\\!", "_") %>%
      str_remove_all("\\'") %>%
      str_remove_all("\\,") %>%
      str_remove_all("\\(") %>%
      str_remove_all("\\)") %>%
      str_remove_all("\\:") %>%
      str_remove_all("\\>\\>_") %>%
      str_replace_all("\\-", "_") %>%
      str_to_lower()
  ) %>%
  # calculate the total & latino population
  mutate(
    total_pop =
      rowSums(across(.cols = not_hispanic_or_latino_white_alone:ncol(.))),
    latino = rowSums(across(.cols = starts_with("hispanic_or_latino")))
  ) %>%
  # select needed columns (geo, race/ethnicity, total)
  select(
    gis_join_match_code, contains("not_hispanic"), latino, total_pop
  ) %>%
  # clean column names
  rename_with(
    ~str_replace(., "not_hispanic_or_latino_", "nh") %>%
      str_remove("_alone") %>%
      str_replace("black_or_african_american", "black") %>%
      str_replace("american_indian_and_alaska_native", "aian") %>%
      str_replace("native_hawaiian_and_other_pacific_islander", "pac") %>%
      str_replace("gis_join_match_code", "bgp2000gj")
  ) %>%
  # create simplified "other" category
  mutate(nhother = nhsome_other_race + nhtwo_or_more_races) %>%
  select(-c(nhsome_other_race, nhtwo_or_more_races)) %>%
  # join with NHGIS crosswalk to 2010 BG
  left_join(xwalk_00to10, by = "bgp2000gj", multiple = "all") %>%
  select(names(xwalk_00to10), everything()) %>%
  # weigh 2000 BGP data by NHGIS weights
  mutate(across(.cols = nhwhite:ncol(.), ~. * wt_pop)) %>%
  select(bg_2010 = bg2010ge, starts_with("nh"), latino, total_pop) %>%
  # summarize data at the 2010 BG level
  group_by(bg_2010) %>%
  summarise(across(.cols = everything(), sum))

## Home value (manual)
hval_bg_00 <-
  read_csv(
    paste0(
      homedir, workdir,
      "NHGIS 2000/Home Value/nhgis0013_ds152_2000_blck_grp_090.csv"
    )
  ) %>%
  # clean column names
  rename_with(
    ~str_replace_all(., " ", "_") %>%
      str_replace_all("\\!\\!", "_") %>%
      str_remove_all("\\'") %>%
      str_remove_all("\\,") %>%
      str_remove_all("\\(") %>%
      str_remove_all("\\)") %>%
      str_remove_all("\\:") %>%
      str_replace_all("\\-", "_") %>%
      str_to_lower()
  ) %>%
  # select needed columns (geo, price bands)
  select(bgp2000gj = gisjoin, contains("g8t")) %>%
  # join with NHGIS crosswalk to 2010 BG
  left_join(xwalk_00to10, by = "bgp2000gj", multiple = "all") %>%
  # weigh 2000 BGP data by NHGIS weights (units)
  mutate(across(.cols = contains("g8t"), ~. * wt_hu)) %>%
  # remove unneeded columns
  select(-c(contains("wt_"), ends_with("gj"))) %>%
  # summarize data at the 2010 BG level
  group_by(bg2010ge) %>%
  summarise(across(.cols = everything(), sum))  %>%
  # rename and rearrange data
  rename(bg_2010 = bg2010ge) %>%
  rename_with(
    .cols = 2:ncol(.),
    ~ paste0("hval_",
             case_when(
               . == "g8t001" ~ "less_than_$10000",
               . == "g8t002" ~ "$10000_to_$14999",
               . == "g8t003" ~ "$15000_to_$19999",
               . == "g8t004" ~ "$20000_to_$24999",
               . == "g8t005" ~ "$25000_to_$29999",
               . == "g8t006" ~ "$30000_to_$34999",
               . == "g8t007" ~ "$35000_to_$39999",
               . == "g8t008" ~ "$40000_to_$49999",
               . == "g8t009" ~ "$50000_to_$59999",
               . == "g8t010" ~ "$60000_to_$69999",
               . == "g8t011" ~ "$70000_to_$79999",
               . == "g8t012" ~ "$80000_to_$89999",
               . == "g8t013" ~ "$90000_to_$99999",
               . == "g8t014" ~ "$100000_to_$124999",
               . == "g8t015" ~ "$125000_to_$149999",
               . == "g8t016" ~ "$150000_to_$174999",
               . == "g8t017" ~ "$175000_to_$199999",
               . == "g8t018" ~ "$200000_to_$249999",
               . == "g8t019" ~ "$250000_to_$299999",
               . == "g8t020" ~ "$300000_to_$399999",
               . == "g8t021" ~ "$400000_to_$499999",
               . == "g8t022" ~ "$500000_to_$749999",
               . == "g8t023" ~ "$750000_to_$999999",
               . == "g8t024" ~ "$1000000_or_more",
               TRUE ~ .
             )
    )
  ) %>%
  select(bg_2010, everything())

## Join all 2000 BGs together, sum up to tracts
### 2010 boundaries
tract00_10 <-
  rent_bg_00 %>%
  left_join(inc_bg_00) %>%
  left_join(race_bg_00) %>%
  left_join(hval_bg_00) %>%
  mutate(tract_2010 = str_extract(bg_2010, "^\\d{11}")) %>%
  select(-bg_2010) %>%
  # derive 2010 tract values
  group_by(tract_2010) %>%
  # round crosswalked values to nearest whole number
  summarise(across(.cols = everything(), ~sum(.) %>% round()))

## Derive median income separately (2000 to 2010)
tract00_medinc <-
  tract00_10 %>%
  select(tract_2010, starts_with("inc_")) %>%
  # derive medians for income from grouped data
  pivot_longer(
    cols = starts_with("inc_"),
    names_to = "inc_range",
    values_to = "n_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(inc_range, "\\d{5,6}$"),
    # adjust income bands to match what the census provides
    upper_bound =
      case_when(
        # less than $10,000 = $9,999
        upper_bound == "10000" ~ "9999",
        # no upper bound set to $500,000
        is.na(upper_bound) ~ "500000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(inc_range, "(?<=^inc_\\$)\\d{5,6}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  # find the grouped median income
  grouped_median(geography = tract_2010, observations = n_hh) %>%
  rename(medinc = grpmed) %>%
  # derive total HH
  group_by(tract_2010, medinc) %>%
  summarise(total_hh = sum(n_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medinc = if_else(medinc >= 200000, 200000, medinc),
    medinc = if_else(total_hh <= 20, NA_real_, medinc)
  )

tract00_medrent <-
  # repeat above process but for rent
  tract00_10 %>%
  select(tract_2010, starts_with("rent_")) %>%
  pivot_longer(
    cols = starts_with("rent_"),
    names_to = "rent_range",
    values_to = "renter_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(rent_range, "\\d{3,4}$"),
    upper_bound =
      case_when(
        upper_bound == "100" ~ "99",
        is.na(upper_bound) ~ "5000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(rent_range, "(?<=^rent_\\$)\\d{3,4}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = tract_2010, observations = renter_hh) %>%
  rename(medrent = grpmed) %>%
  group_by(tract_2010, medrent) %>%
  summarise(renter_hh = sum(renter_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medrent = if_else(medrent >= 2000, 2000, medrent),
    medrent = if_else(renter_hh <= 20, NA_real_, medrent)
  )

tract00_medhval <-
  # repeat above process but for home value
  tract00_10 %>%
  select(tract_2010, starts_with("hval_")) %>%
  pivot_longer(
    cols = starts_with("hval_"),
    names_to = "hval_range",
    values_to = "hval_hu"
  ) %>%
  mutate(
    upper_bound =
      str_remove_all(hval_range, "\\,+") %>% str_extract("\\d{5,8}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        # $1M+ is upper bound, set to $2M
        is.na(upper_bound) ~ "2000000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_remove_all(hval_range, "\\,+") %>%
      str_extract("(?<=^hval_\\$)\\d{5,8}") %>%
      as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = tract_2010, observations = hval_hu) %>%
  rename(medhval = grpmed) %>%
  group_by(tract_2010, medhval) %>%
  summarise(hval_hu = sum(hval_hu)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medhval = if_else(medhval >= 1000000, 1000000, medhval),
    medhval = if_else(hval_hu <= 20, NA_real_, medhval)
  )

## Join all 2000 data together (2010 boundaries)
tract00_10 <-
  tract00_10 %>%
  left_join(tract00_medinc) %>%
  left_join(tract00_medrent) %>%
  left_join(tract00_medhval) %>%
  select(tract_2010, total_pop, renter_hh, total_hh, hval_hu, everything()) %>%
  rename_with(.cols = 2:ncol(.), ~ paste0(., "_00"))

## 2020 boundaries
tract00_20 <-
  # xwalk 2010 boundaries to 2020 tracts (build up from BG)
  rent_bg_00 %>%
  left_join(inc_bg_00) %>%
  left_join(race_bg_00) %>%
  left_join(hval_bg_00) %>%
  left_join(
    xwalk_10to20 %>% select(ends_with("ge"), starts_with("wt")),
    by = c("bg_2010" = "bg2010ge"),
    multiple = "all"
  ) %>%
  # apply total population weight to race/ethnicity
  mutate(
    across(.cols = c(starts_with("nh"), "total_pop", "latino"), ~. * wt_pop)
  ) %>%
  # apply household weight to rent and income
  mutate(across(.cols = c(contains("inc"), contains("rent")), ~. * wt_hh)) %>%
  # apply housing unit weight to home value
  mutate(across(.cols = contains("hval_"), ~ . * wt_hu)) %>%
  select(-c(bg_2010, contains("wt_"))) %>%
  # summarize at 2020 BG boundaries
  group_by(bg2020ge) %>%
  summarise(across(.cols = everything(), sum)) %>%
  ungroup() %>%
  mutate(tract_2020 = str_extract(bg2020ge, "^\\d{11}")) %>%
  select(-bg2020ge) %>%
  # summarize at 2020 tract boundaries (round to nearest whole number)
  group_by(tract_2020) %>%
  summarise(across(.cols = everything(), ~sum(.) %>% round()))

# derive medians for income and rent from grouped data (repeat same process as
# above)
tract00_medinc <-
  tract00_20 %>%
  select(tract_2020, starts_with("inc_")) %>%
  # derive medians for income and rent from grouped data
  pivot_longer(
    cols = starts_with("inc_"),
    names_to = "inc_range",
    values_to = "n_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(inc_range, "\\d{5,6}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        is.na(upper_bound) ~ "500000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(inc_range, "(?<=^inc_\\$)\\d{5,6}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = tract_2020, observations = n_hh) %>%
  rename(medinc = grpmed) %>%
  group_by(tract_2020, medinc) %>%
  summarise(total_hh = sum(n_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medinc = if_else(medinc >= 200000, 200000, medinc),
    medinc = if_else(total_hh <= 20, NA_real_, medinc)
  )

tract00_medrent <-
  tract00_20 %>%
  select(tract_2020, starts_with("rent_")) %>%
  pivot_longer(
    cols = starts_with("rent_"),
    names_to = "rent_range",
    values_to = "renter_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(rent_range, "\\d{3,4}$"),
    upper_bound =
      case_when(
        upper_bound == "100" ~ "99",
        is.na(upper_bound) ~ "5000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(rent_range, "(?<=^rent_\\$)\\d{3,4}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = tract_2020, observations = renter_hh) %>%
  rename(medrent = grpmed) %>%
  group_by(tract_2020, medrent) %>%
  summarise(renter_hh = sum(renter_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medrent = if_else(medrent >= 2000, 2000, medrent),
    medrent = if_else(renter_hh <= 20, NA_real_, medrent)
  )

tract00_medhval <-
  # repeat above process but for home value
  tract00_20 %>%
  select(tract_2020, starts_with("hval_")) %>%
  pivot_longer(
    cols = starts_with("hval_"),
    names_to = "hval_range",
    values_to = "hval_hu"
  ) %>%
  mutate(
    upper_bound =
      str_remove_all(hval_range, "\\,+") %>% str_extract("\\d{5,8}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        # $1M+ is upper bound, set to $2M
        is.na(upper_bound) ~ "2000000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_remove_all(hval_range, "\\,+") %>%
      str_extract("(?<=^hval_\\$)\\d{5,8}") %>%
      as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = tract_2020, observations = hval_hu) %>%
  rename(medhval = grpmed) %>%
  group_by(tract_2020, medhval) %>%
  summarise(hval_hu = sum(hval_hu)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medhval = if_else(medhval >= 1000000, 1000000, medhval),
    medhval = if_else(hval_hu <= 20, NA_real_, medhval)
  )

## Join all 2000 data by 2020 tract boundaries
tract00_20 <-
  tract00_20 %>%
  left_join(tract00_medinc) %>%
  left_join(tract00_medrent) %>%
  left_join(tract00_medhval) %>%
  select(tract_2020, total_pop, renter_hh, total_hh, hval_hu, everything()) %>%
  rename_with(.cols = 2:ncol(.), ~ paste0(., "_00"))

# Helper functions -----------------------------------------------------

## This section creates helper functions to automate cleaning and preparation
## of various ACS/census datasets. Different functions apply to different
## vintages, as data can be shaped differently.

# Rent helpers
medrent_cleaner <- function(df, year) {
  df %>%
    rename_with(
      .cols = everything(),
      ~case_when(
        str_detect(., "margin") ~ "moe_medrent",
        str_detect(., "median_gross_rent") ~ "medrent",
        TRUE ~ .
      )
    ) %>%
    bind_rows(tibble(moe_medrent = NA)) %>%
    transmute(
      geography = geography,
      year = year,
      medrent =
        str_replace(medrent, "0\\-", "00") %>%
        str_remove("\\+") %>%
        str_remove("\\-") %>%
        str_remove("\\,") %>%
        str_remove("null") %>%
        as.double(),
      moe_medrent =
        str_replace(moe_medrent, "0\\-", "00") %>%
        str_remove("\\+") %>%
        str_remove("\\-") %>%
        str_remove("\\,") %>%
        str_remove("null") %>%
        str_replace("\\*{3,}", "0") %>%
        str_remove("\\*{1,2}") %>%
        as.double()
    ) %>%
    filter(!is.na(geography))
}

medrent_join <- function(df, geo) {

  df %>%
    pivot_wider(
      names_from = year,
      values_from = c(medrent, moe_medrent),
      names_glue = "{.value}_{year}"
    ) %>%
    rename_with(.cols = contains("medrent_"), ~paste0(geo, "_", .))
}

# Income helpers

medinc_cleaner <- function(df, year) {
  df %>%
    rename(moe_medinc = contains("margin")) %>%
    rename(medinc = contains("median_household_income_")) %>%
    bind_rows(tibble(moe_medinc = NA)) %>%
    transmute(
      year = year,
      geography = geography,
      medinc =
        str_replace(medinc, "0\\-", "00") %>%
        str_remove("\\+") %>%
        str_remove("\\-") %>%
        str_remove("\\,") %>%
        str_remove("null") %>%
        as.double(),
      moe_medinc =
        str_replace(moe_medinc, "0\\-", "00") %>%
        str_remove("\\+") %>%
        str_remove("\\-") %>%
        str_remove("\\,") %>%
        str_remove("null") %>%
        str_replace("\\*{3,}", "0") %>%
        str_remove("\\*{1,2}") %>%
        as.double()
    ) %>%
    filter(!is.na(geography))
}

medinc_join <- function(df, geo) {
  df %>%
    pivot_wider(
      names_from = year,
      values_from = c(medinc, moe_medinc),
      names_glue = "{.value}_{year}"
    ) %>%
    rename_with(.cols = contains("medinc_"), ~paste0(geo, "_", .))
}

inc_cleaner <- function(df, year) {

  df %>%
    select(
      -c(estimate_total, contains("geographic_area_name"),
         contains("label_for_geo_id"))
    ) %>%
    pivot_longer(
      cols = 2:ncol(.), names_to = "income_level", values_to = "n_hh"
    ) %>%
    mutate(
      year = year,
      income_level =
        str_remove_all(income_level, "^estimate_total_") %>%
        str_remove_all("^inc_") %>%
        str_replace_all("_", " ") %>%
        str_to_title(),
      income_max =
        str_extract(income_level, "\\$\\d+$") %>%
        str_remove("\\$") %>%
        as.double(),
      income_max =
        case_when(
          income_max == 10000 ~ 9999,
          income_level == "$200000 Or More" ~ 599999,
          TRUE ~ income_max
        ),
      county = str_extract(geography, "\\d{0,5}")
    ) %>%
    left_join(medinc_long) %>%
    # create income level
    mutate(
      inc_cat =
        case_when(
          income_max <= medinc * .3 ~ "ELI",
          income_max <= medinc * .5 & income_max > medinc * .3 ~ "VLI",
          income_max <= medinc * .8 & income_max > medinc * .5 ~ "LI",
          income_max <= medinc * 1.2 & income_max > medinc * .8 ~ "mod",
          income_max > medinc * 1.2 ~ "abovemod"
        )
    ) %>%
    group_by(geography, inc_cat) %>%
    summarise(n_hh = sum(n_hh, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = inc_cat, values_from = n_hh) %>%
    mutate(across(.cols = 2:ncol(.), ~replace_na(., 0))) %>%
    mutate(
      year = year,
      lowinc = ELI + VLI + LI,
      lowmod = lowinc + mod,
      totalhh = lowinc + mod + abovemod
    ) %>%
    left_join(
      df %>%
        select(
          -c(estimate_total, contains("geographic_area_name"),
             contains("label_for_geo_id"))
        ) %>%
        rename_with(
          .cols = starts_with("estimate"),
          ~str_replace_all(., "^estimate_total_", "inc_")
        )
    )
}

inc_join <- function(df, geo) {

  df %>%
    select(year, geography, lowinc, lowmod, abovemod, totalhh) %>%
    pivot_wider(
      names_from = year,
      values_from = c(lowinc, lowmod, abovemod, totalhh),
      names_glue = "{.value}_{year}"
    ) %>%
    rename_with(
      .cols =
        c(contains("lowinc"), contains("lowmod"), contains("abovemod"),
          contains("totalhh")),
      ~paste0(geo, "_", .)
    )
}

# Race/ethnicity
raceeth_cleaner <- function(df, year) {
  df %>%
    bind_rows(
      tibble(margin_of_error_total = NA,
             margin_of_error_total_not_hispanic_or_latino_white_alone = NA)
    ) %>%
    select(
      geography,
      pop_total = estimate_total,
      moe_pop_total = margin_of_error_total,
      pop_nhwhite = estimate_total_not_hispanic_or_latino_white_alone,
      moe_pop_nhwhite = margin_of_error_total_not_hispanic_or_latino_white_alone,
      pop_nhblack =
        estimate_total_not_hispanic_or_latino_black_or_african_american_alone,
      pop_nhaian =
        estimate_total_not_hispanic_or_latino_american_indian_and_alaska_native_alone,
      pop_nhasian = estimate_total_not_hispanic_or_latino_asian_alone,
      pop_latino = estimate_total_hispanic_or_latino,
      pop_nhpac =
        estimate_total_not_hispanic_or_latino_native_hawaiian_and_other_pacific_islander_alone
    ) %>%
    mutate(
      across(
        .cols = 2:ncol(.),
        ~str_remove_all(., "\\$") %>%
          str_remove_all("\\-") %>%
          str_remove_all("\\+") %>%
          str_remove_all("\\,") %>%
          str_remove("null") %>%
          str_remove("\\(r\\d+\\)") %>%
          str_replace_all("\\*{5}", "0") %>%
          as.double()
      )
    ) %>%
    mutate(
      pct_nhwhite = pop_nhwhite / pop_total,
      pct_nhblack = pop_nhblack / pop_total,
      pct_nhaian = pop_nhaian / pop_total,
      pct_nhasian = pop_nhasian / pop_total,
      pct_latino = pop_latino / pop_total,
      pop_nhother =
        pop_total - pop_nhwhite - pop_nhblack - pop_nhaian - pop_nhasian -
        pop_latino - pop_nhpac,
      pct_nhother = pop_nhother / pop_total,
      year = year
    ) %>%
    filter(!is.na(geography))
}

raceeth_join <- function(df, geo) {

  df %>%
    select(geography, year, everything()) %>%
    pivot_wider(
      names_from = year,
      values_from = pop_total:ncol(.),
      names_glue = "{.value}_{year}"
    ) %>%
    rename_with(.cols = 2:ncol(.), ~paste0(geo, "_", .))
}

## Home value
medhval_cleaner <- function(df, year) {
  df %>%
    rename_with(
      .cols = everything(),
      ~case_when(
        str_detect(., "estimate_median_value_dollars") ~ "medhval",
        str_detect(., "margin_of_error_median_value_dollars") ~ "moe_medhval",
        TRUE ~ .
      )
    ) %>%
    bind_rows(tibble(moe_hval_hu = NA)) %>%
    transmute(
      geography = geography,
      year = year,
      medhval =
        str_remove_all(medhval, "\\+") %>%
        str_remove_all("\\-") %>%
        str_remove_all("\\,") %>%
        str_remove_all("null") %>%
        as.double(),
      moe_medhval =
        str_remove_all(medhval, "\\+") %>%
        str_remove_all("\\-") %>%
        str_remove_all("\\,") %>%
        str_remove_all("null") %>%
        str_replace_all("\\*{3,}", "0") %>%
        as.double(),
      hval_hu = hval_hu,
      moe_hval_hu = moe_hval_hu
    ) %>%
    filter(!is.na(geography))
}

medhval_join <- function(df, geo) {

  df %>%
    pivot_wider(
      names_from = year,
      values_from = c(medhval, hval_hu, moe_medhval, moe_hval_hu),
      names_glue = "{.value}_{year}"
    ) %>%
    rename_with(
      .cols = c(contains("medhval_"), contains("hval_hu")), ~paste0(geo, "_", .)
    )
}

### Regional functions
regional_medinc <- function(df, year) {
  ### Median Income
  df %>%
    inc_cleaner(year) %>%
    left_join(
      tcac_opp_df %>%
        select(county, region) %>%
        filter(!str_detect(region, "Rural")) %>%
        distinct(),
      by = c("geography" = "county")
    ) %>%
    select(region, contains("inc_")) %>%
    mutate(region = if_else(is.na(region), "Rural Counties", region)) %>%
    group_by(region) %>%
    summarise(across(.cols = where(is.double), ~sum(., na.rm = TRUE))) %>%
    # derive medians for income from grouped data
    pivot_longer(
      cols = starts_with("inc_"),
      names_to = "inc_range",
      values_to = "n_hh"
    ) %>%
    mutate(
      upper_bound = str_extract(inc_range, "\\d{5,6}$"),
      upper_bound =
        case_when(
          upper_bound == "10000" ~ "9999",
          is.na(upper_bound) ~ "500000",
          TRUE ~ upper_bound
        ) %>% as.double(),
      lower_bound =
        str_extract(inc_range, "(?<=^inc_\\$)\\d{5,6}") %>% as.double(),
      lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
    ) %>%
    grouped_median(geography = region, observations = n_hh) %>%
    rename(medinc = grpmed) %>%
    group_by(region, medinc) %>%
    summarise(total_hh = sum(n_hh)) %>%
    ungroup() %>%
    mutate(
      # adjust interpolated medians to better align with given census data
      medinc = if_else(medinc >= 200000, 200000, medinc),
      medinc = if_else(total_hh <= 20, NA_real_, medinc),
      year = year
    ) %>%
    select(region, year, region_medinc = medinc, region_total_hh = total_hh)
}

regional_medrent <- function(df, year) {
  ### Median Rent
  df %>%
    select(
      -c(estimate_total, estimate_total_with_cash_rent,
         estimate_total_no_cash_rent)
    ) %>%
    left_join(
      tcac_opp_df %>%
        select(county, region) %>%
        filter(!str_detect(region, "Rural")) %>%
        distinct(),
      by = c("geography" = "county")
    ) %>%
    mutate(region = if_else(is.na(region), "Rural Counties", region)) %>%
    rename_with(
      .cols = starts_with("estimate_"), ~str_remove(., "estimate_total_with_cash_")
    ) %>%
    select(region, starts_with("rent_")) %>%
    pivot_longer(
      cols = starts_with("rent_"),
      names_to = "rent_range",
      values_to = "renter_hh"
    ) %>%
    mutate(
      upper_bound = str_extract(rent_range, "\\d{3,4}$"),
      upper_bound =
        case_when(
          upper_bound == "100" ~ "99",
          is.na(upper_bound) ~ "5000",
          TRUE ~ upper_bound
        ) %>% as.double(),
      lower_bound =
        str_extract(rent_range, "(?<=^rent_\\$)\\d{3,4}") %>% as.double(),
      lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
    ) %>%
    grouped_median(geography = region, observations = renter_hh) %>%
    rename(medrent = grpmed) %>%
    group_by(region, medrent) %>%
    summarise(renter_hh = sum(renter_hh)) %>%
    ungroup() %>%
    mutate(
      # adjust interpolated medians to better align with given census data
      medrent = if_else(medrent >= 2000, 2000, medrent),
      medrent = if_else(renter_hh <= 20, NA_real_, medrent),
      year = year
    ) %>%
    select(region, year, region_medrent = medrent, region_renter_hh = renter_hh)
}

# County join ----------------------------------------------------------

# This sections joins all county data together into a single dataframe. It
# uses helper functions where possible, or manually cleans a given year if
# needed.

## Rent
medrent_county_df <-
  medrent_cleaner(medrent_county_00, year = 2000) %>%
  bind_rows(
    medrent_cleaner(medrent_county_10, 2010),
    medrent_cleaner(medrent_county_11, 2011),
    medrent_cleaner(medrent_county_12, 2012),
    medrent_cleaner(medrent_county_13, 2013),
    medrent_cleaner(medrent_county_14, 2014),
    medrent_cleaner(medrent_county_15, 2015),
    medrent_cleaner(medrent_county_16, 2016),
    medrent_cleaner(medrent_county_17, 2017),
    medrent_cleaner(medrent_county_18, 2018),
    medrent_cleaner(medrent_county_19, 2019),
    medrent_cleaner(medrent_county_20, 2020),
    medrent_cleaner(medrent_county_21, 2021),
    medrent_cleaner(medrent_county_22, 2022)
  ) %>%
  medrent_join("cnty")

## Income
medinc_county_df <-
  medinc_cleaner(medinc_county_00, 2000) %>%
  bind_rows(
    medinc_cleaner(medinc_county_10, 2010),
    medinc_cleaner(medinc_county_11, 2011),
    medinc_cleaner(medinc_county_12, 2012),
    medinc_cleaner(medinc_county_13, 2013),
    medinc_cleaner(medinc_county_14, 2014),
    medinc_cleaner(medinc_county_15, 2015),
    medinc_cleaner(medinc_county_16, 2016),
    medinc_cleaner(medinc_county_17, 2017),
    medinc_cleaner(medinc_county_18, 2018),
    medinc_cleaner(medinc_county_19, 2019),
    medinc_cleaner(medinc_county_20, 2020),
    medinc_cleaner(medinc_county_21, 2021),
    medinc_cleaner(medinc_county_22, 2022)
  ) %>%
  medinc_join("cnty")

# reshape the data for later use
medinc_long <-
  medinc_county_df %>%
  select(-contains("moe_")) %>%
  pivot_longer(
    cols = 2:ncol(.),
    names_to = "year",
    values_to = "medinc"
  ) %>%
  mutate(year = str_remove(year, "\\w+_medinc_") %>% as.double()) %>%
  rename(county = geography)

## Race
race_county_df <-
  raceeth_cleaner(
    race_county_00 %>%
      rename_with(
        ~str_replace(., "total", "estimate_total") %>%
          str_remove("population_of_one_race_")
      ),
    2000
  ) %>%
  bind_rows(
    raceeth_cleaner(
      race_county_10 %>%
        rename_with(
          ~str_replace(., "total", "estimate_total") %>%
            str_remove("population_of_one_race_")
        ),
      2010
    ),
    raceeth_cleaner(race_county_11, 2011),
    raceeth_cleaner(race_county_12, 2012),
    raceeth_cleaner(race_county_13, 2013),
    raceeth_cleaner(race_county_14, 2014),
    raceeth_cleaner(race_county_15, 2015),
    raceeth_cleaner(race_county_16, 2016),
    raceeth_cleaner(race_county_17, 2017),
    raceeth_cleaner(race_county_18, 2018),
    raceeth_cleaner(race_county_19, 2019),
    raceeth_cleaner(
      race_county_20 %>%
        rename_with(
          ~str_replace(., "_total", "estimate_total") %>%
            str_remove("population_of_one_race_")
        ),
      2020
    ),
    raceeth_cleaner(race_county_21, 2021),
    raceeth_cleaner(race_county_22, 2022)
  ) %>%
  raceeth_join("cnty")

## Home value

medhval_county_df <-
  medhval_cleaner(
    medhval_county_00 %>%
      select(
        geography,
        medhval = number_specified_owner_occupied_units_value_median_dollars,
        hval_hu = number_specified_owner_occupied_units
      ),
    year = 2000
  ) %>%
  bind_rows(
    medhval_cleaner(
      medhval_county_10 %>%
        left_join(
          hval_county_10 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2010
    ),
    medhval_cleaner(
      medhval_county_11 %>%
        left_join(
          hval_county_11 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2011
    ),
    medhval_cleaner(
      medhval_county_12 %>%
        left_join(
          hval_county_12 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2012
    ),
    medhval_cleaner(
      medhval_county_13 %>%
        left_join(
          hval_county_13 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2013
    ),
    medhval_cleaner(
      medhval_county_14 %>%
        left_join(
          hval_county_14 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2014
    ),
    medhval_cleaner(
      medhval_county_15 %>%
        left_join(
          hval_county_15 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2015
    ),
    medhval_cleaner(
      medhval_county_16 %>%
        left_join(
          hval_county_16 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2016
    ),
    medhval_cleaner(
      medhval_county_17 %>%
        left_join(
          hval_county_17 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2017
    ),
    medhval_cleaner(
      medhval_county_18 %>%
        left_join(
          hval_county_18 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2018
    ),
    medhval_cleaner(
      medhval_county_19 %>%
        left_join(
          hval_county_19 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2019
    ),
    medhval_cleaner(
      medhval_county_20 %>%
        left_join(
          hval_county_20 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2020
    ),
    medhval_cleaner(
      medhval_county_21 %>%
        left_join(
          hval_county_21 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2021
    ),
    medhval_cleaner(
      medhval_county_22 %>%
        left_join(
          hval_county_22 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ), 2022
    )
  ) %>%
  medhval_join("cnty")

## All
county_df <-
  medrent_county_df %>%
  left_join(medinc_county_df) %>%
  left_join(race_county_df) %>%
  left_join(medhval_county_df) %>%
  mutate(geography = str_remove(geography, "^06"))

# Regional join --------------------------------------------------------

# This sections joins all regional data together into a single dataframe. It
# uses helper functions where possible, or manually cleans a given year if
# needed.

## Median income
medinc_region_df <-
  inc_county_00 %>%
  rename_with(.cols = total:ncol(.), ~paste0("estimate_", .)) %>%
  regional_medinc(2000) %>%
  bind_rows(
    regional_medinc(inc_county_10, 2010),
    regional_medinc(inc_county_11, 2011),
    regional_medinc(inc_county_12, 2012),
    regional_medinc(inc_county_13, 2013),
    regional_medinc(inc_county_14, 2014),
    regional_medinc(inc_county_15, 2015),
    regional_medinc(inc_county_16, 2016),
    regional_medinc(inc_county_17, 2017),
    regional_medinc(inc_county_18, 2018),
    regional_medinc(inc_county_19, 2019),
    regional_medinc(inc_county_20, 2020),
    regional_medinc(inc_county_21, 2021),
    regional_medinc(inc_county_22, 2022)
  ) %>%
  pivot_wider(
    names_from = year, values_from = c(region_medinc, region_total_hh)
  ) %>%
  # replace single county region estimates w/ known values
  mutate(
    geography =
      case_when(
        region == "Los Angeles Region" ~ "06037",
        region == "San Diego Region" ~ "06073",
        region == "Orange County Region" ~ "06059",
        TRUE ~ NA_character_
      )
  ) %>%
  left_join(medinc_county_df) %>%
  # replace extrapolated medians with known medians in single-county regions
  mutate(
    region_medinc_2000 =
      if_else(is.na(cnty_medinc_2000), region_medinc_2000, cnty_medinc_2000),
    region_medinc_2010 =
      if_else(is.na(cnty_medinc_2010), region_medinc_2010, cnty_medinc_2010),
    region_medinc_2011 =
      if_else(is.na(cnty_medinc_2011), region_medinc_2011, cnty_medinc_2011),
    region_medinc_2012 =
      if_else(is.na(cnty_medinc_2012), region_medinc_2012, cnty_medinc_2012),
    region_medinc_2013 =
      if_else(is.na(cnty_medinc_2013), region_medinc_2013, cnty_medinc_2013),
    region_medinc_2014 =
      if_else(is.na(cnty_medinc_2014), region_medinc_2014, cnty_medinc_2014),
    region_medinc_2015 =
      if_else(is.na(cnty_medinc_2015), region_medinc_2015, cnty_medinc_2015),
    region_medinc_2016 =
      if_else(is.na(cnty_medinc_2016), region_medinc_2016, cnty_medinc_2016),
    region_medinc_2017 =
      if_else(is.na(cnty_medinc_2017), region_medinc_2017, cnty_medinc_2017),
    region_medinc_2018 =
      if_else(is.na(cnty_medinc_2018), region_medinc_2018, cnty_medinc_2018),
    region_medinc_2019 =
      if_else(is.na(cnty_medinc_2019), region_medinc_2019, cnty_medinc_2019),
    region_medinc_2020 =
      if_else(is.na(cnty_medinc_2020), region_medinc_2020, cnty_medinc_2020),
    region_medinc_2021 =
      if_else(is.na(cnty_medinc_2021), region_medinc_2021, cnty_medinc_2021),
    region_medinc_2022 =
      if_else(is.na(cnty_medinc_2022), region_medinc_2022, cnty_medinc_2022)
  ) %>%
  select(-c(geography:ncol(.)))

## Median rent
medrent_region_df <-
  rent_county_00 %>%
  rename_with(.cols = total:ncol(.), ~paste0("estimate_", .)) %>%
  regional_medrent(2000) %>%
  bind_rows(
    regional_medrent(rent_county_10, 2010),
    regional_medrent(rent_county_11, 2011),
    regional_medrent(rent_county_12, 2012),
    regional_medrent(rent_county_13, 2013),
    regional_medrent(rent_county_14, 2014),
    regional_medrent(rent_county_15, 2015),
    regional_medrent(rent_county_16, 2016),
    regional_medrent(rent_county_17, 2017),
    regional_medrent(rent_county_18, 2018),
    regional_medrent(rent_county_19, 2019),
    regional_medrent(rent_county_20, 2020),
    regional_medrent(rent_county_21, 2021),
    regional_medrent(rent_county_22, 2022)
  ) %>%
  pivot_wider(
    names_from = year, values_from = c(region_medrent, region_renter_hh)
  ) %>%
  # replace single county region estimates
  mutate(
    geography =
      case_when(
        region == "Los Angeles Region" ~ "06037",
        region == "San Diego Region" ~ "06073",
        region == "Orange County Region" ~ "06059",
        TRUE ~ NA_character_
      )
  ) %>%
  left_join(medrent_county_df) %>%
  # replace extrapolated medians with known medians in single-county regions
  mutate(
    region_medrent_2000 =
      if_else(is.na(cnty_medrent_2000), region_medrent_2000, cnty_medrent_2000),
    region_medrent_2010 =
      if_else(is.na(cnty_medrent_2010), region_medrent_2010, cnty_medrent_2010),
    region_medrent_2011 =
      if_else(is.na(cnty_medrent_2011), region_medrent_2011, cnty_medrent_2011),
    region_medrent_2012 =
      if_else(is.na(cnty_medrent_2012), region_medrent_2012, cnty_medrent_2012),
    region_medrent_2013 =
      if_else(is.na(cnty_medrent_2013), region_medrent_2013, cnty_medrent_2013),
    region_medrent_2014 =
      if_else(is.na(cnty_medrent_2014), region_medrent_2014, cnty_medrent_2014),
    region_medrent_2015 =
      if_else(is.na(cnty_medrent_2015), region_medrent_2015, cnty_medrent_2015),
    region_medrent_2016 =
      if_else(is.na(cnty_medrent_2016), region_medrent_2016, cnty_medrent_2016),
    region_medrent_2017 =
      if_else(is.na(cnty_medrent_2017), region_medrent_2017, cnty_medrent_2017),
    region_medrent_2018 =
      if_else(is.na(cnty_medrent_2018), region_medrent_2018, cnty_medrent_2018),
    region_medrent_2019 =
      if_else(is.na(cnty_medrent_2019), region_medrent_2019, cnty_medrent_2019),
    region_medrent_2020 =
      if_else(is.na(cnty_medrent_2020), region_medrent_2020, cnty_medrent_2020),
    region_medrent_2021 =
      if_else(is.na(cnty_medrent_2021), region_medrent_2021, cnty_medrent_2021),
    region_medrent_2022 =
      if_else(is.na(cnty_medrent_2022), region_medrent_2022, cnty_medrent_2022)
  ) %>%
  select(-c(geography:ncol(.)))

## Race/ethnicity
race_region_df <-
  county_df %>%
  select(geography, contains("cnty_pop")) %>%
  left_join(
    tcac_opp_df %>%
      select(county, region) %>%
      mutate(county = str_remove(county, "^06")) %>%
      filter(!str_detect(region, "Rural")) %>%
      distinct(),
    by = c("geography" = "county")
  ) %>%
  select(region, contains("cnty_pop")) %>%
  mutate(region = if_else(is.na(region), "Rural Counties", region)) %>%
  group_by(region) %>%
  summarise(across(.cols = where(is.double), ~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  rename_with(.cols = 2:ncol(.), ~str_replace(., "cnty", "region")) %>%
  # derive pct nh race by year
  pivot_longer(
    cols = contains("total"), names_to = "years", values_to = "total"
  ) %>%
  pivot_longer(
    cols = c(contains("pop_nh"), contains("pop_latino")),
    names_to = "years_2", values_to = "nhrace"
  ) %>%
  mutate(
    race = str_remove(years_2, "region_pop_") %>% str_remove("_\\d{0,4}$"),
    years = str_remove(years, "region_pop_total_"),
    years_2 = str_remove(years_2, "region_pop_\\w+_")
  ) %>%
  filter(years == years_2) %>%
  select(-years_2) %>%
  pivot_wider(names_from = race, values_from = nhrace) %>%
  mutate(
    across(.cols = nhwhite:ncol(.), ~ . / total, .names = "region_pct_{.col}")
  ) %>%
  rename_with(.cols = total:latino, ~ paste0("region_pop_", .)) %>%
  pivot_wider(names_from = years, values_from = region_pop_total:ncol(.))

## All
region_df <-
  medrent_region_df %>%
  left_join(medinc_region_df) %>%
  left_join(race_region_df)

rm(list = ls(pattern = "_county_"))

# Tract join -----------------------------------------------------------

# This section creates final joined datasets, both for 2010 boundaries and
# 2020 boundaries. Data for each decade's boundaries are conjoined in separate
# subsections below.

## 2010 boundaries -----------------------------------------------------

## 2020-2010 BG Crosswalk function
xwalker_20to10 <- function(df, wt) {

  # select weight type in the function (population, housing, etc.)
  wt <- enquo(wt)

  df <-
    df %>%
    left_join(
      xwalk_20to10 %>% select(bg2020ge, bg2010ge, !!wt),
      by = c("geography" = "bg2020ge"),
      multiple = "all"
    ) %>%
    select(-c(geography, geographic_area_name)) %>%
    mutate(across(.cols = contains("estimate"), ~ . * !!wt)) %>%
    mutate(geography = str_extract(bg2010ge, "\\d{11}")) %>%
    select(geography, everything()) %>%
    select(-c(!!wt, bg2010ge)) %>%
    group_by(geography) %>%
    summarise(across(.cols = everything(), ~sum(., na.rm = TRUE) %>% round()))
}

## Rent
### Crosswalk 2020+ data to 2010 boundaries
rent_xwalktract_22 <-
  rent_bg_22 %>%
  xwalker_20to10(wt = wt_hh) %>%
  rename_with(~str_remove(., "estimate_total_with_cash_")) %>%
  select(geography, starts_with("rent_"))

medrent_xwalktract_22 <-
  rent_xwalktract_22 %>%
  pivot_longer(
    cols = starts_with("rent_"),
    names_to = "rent_range",
    values_to = "renter_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(rent_range, "\\d{3,4}$"),
    upper_bound =
      case_when(
        upper_bound == "100" ~ "99",
        is.na(upper_bound) ~ "5000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(rent_range, "(?<=^rent_\\$)\\d{3,4}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = renter_hh) %>%
  rename(medrent = grpmed)  %>%
  group_by(geography, medrent) %>%
  summarise(renter_hh = sum(renter_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medrent = if_else(medrent >= 3500, 3500, medrent),
    medrent = if_else(renter_hh <= 20, NA_real_, medrent)
  )

rent_xwalktract_21 <-
  rent_bg_21 %>%
  xwalker_20to10(wt = wt_hh) %>%
  rename_with(~str_remove(., "estimate_total_with_cash_")) %>%
  select(geography, starts_with("rent_"))

medrent_xwalktract_21 <-
  rent_xwalktract_21 %>%
  pivot_longer(
    cols = starts_with("rent_"),
    names_to = "rent_range",
    values_to = "renter_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(rent_range, "\\d{3,4}$"),
    upper_bound =
      case_when(
        upper_bound == "100" ~ "99",
        is.na(upper_bound) ~ "5000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(rent_range, "(?<=^rent_\\$)\\d{3,4}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = renter_hh) %>%
  rename(medrent = grpmed)  %>%
  group_by(geography, medrent) %>%
  summarise(renter_hh = sum(renter_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medrent = if_else(medrent >= 3500, 3500, medrent),
    medrent = if_else(renter_hh <= 20, NA_real_, medrent)
  )

rent_xwalktract_20 <-
  rent_bg_20 %>%
  xwalker_20to10(wt = wt_hh) %>%
  rename_with(~str_remove(., "estimate_total_with_cash_")) %>%
  select(geography, starts_with("rent_"))

medrent_xwalktract_20 <-
  rent_xwalktract_20 %>%
  pivot_longer(
    cols = starts_with("rent_"),
    names_to = "rent_range",
    values_to = "renter_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(rent_range, "\\d{3,4}$"),
    upper_bound =
      case_when(
        upper_bound == "100" ~ "99",
        is.na(upper_bound) ~ "5000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(rent_range, "(?<=^rent_\\$)\\d{3,4}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = renter_hh) %>%
  rename(medrent = grpmed)  %>%
  group_by(geography, medrent) %>%
  summarise(renter_hh = sum(renter_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medrent = if_else(medrent >= 3500, 3500, medrent),
    medrent = if_else(renter_hh <= 20, NA_real_, medrent)
  )

# Note: median derived from rent distribution data for xwalked data
medrent_tract2010_df <-
  tract00_10 %>%
  select(geography = tract_2010, medrent = medrent_00) %>%
  medrent_cleaner(2000) %>%
  bind_rows(
    medrent_cleaner(medrent_tract_10, 2010),
    medrent_cleaner(medrent_tract_11, 2011),
    medrent_cleaner(medrent_tract_12, 2012),
    medrent_cleaner(medrent_tract_13, 2013),
    medrent_cleaner(medrent_tract_14, 2014),
    medrent_cleaner(medrent_tract_15, 2015),
    medrent_cleaner(medrent_tract_16, 2016),
    medrent_cleaner(medrent_tract_17, 2017),
    medrent_cleaner(medrent_tract_18, 2018),
    medrent_cleaner(medrent_tract_19, 2019),
    medrent_cleaner(medrent_xwalktract_20, 2020),
    medrent_cleaner(medrent_xwalktract_21, 2021),
    medrent_cleaner(medrent_xwalktract_22, 2022)
  ) %>%
  medrent_join("trct")

## Income
### Crosswalk 2020+ data to 2010 boundaries
inc_xwalktract_22 <- inc_bg_22 %>% xwalker_20to10(wt = wt_hh)
inc_xwalktract_21 <- inc_bg_21 %>% xwalker_20to10(wt = wt_hh)
inc_xwalktract_20 <- inc_bg_20 %>% xwalker_20to10(wt = wt_hh)

medinc_xwalktract_22 <-
  inc_xwalktract_22 %>%
  rename_with(~str_replace(., "estimate_total_", "inc_")) %>%
  select(geography, starts_with("inc_")) %>%
  # derive medians for income and rent from grouped data
  pivot_longer(
    cols = starts_with("inc_"),
    names_to = "inc_range",
    values_to = "n_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(inc_range, "\\d{5,6}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        is.na(upper_bound) ~ "500000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(inc_range, "(?<=^inc_\\$)\\d{5,6}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = n_hh) %>%
  rename(medinc = grpmed) %>%
  group_by(geography, medinc) %>%
  summarise(total_hh = sum(n_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medinc = if_else(total_hh <= 20, NA_real_, medinc)
  )

medinc_xwalktract_21 <-
  inc_xwalktract_21 %>%
  rename_with(~str_replace(., "estimate_total_", "inc_")) %>%
  select(geography, starts_with("inc_")) %>%
  # derive medians for income and rent from grouped data
  pivot_longer(
    cols = starts_with("inc_"),
    names_to = "inc_range",
    values_to = "n_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(inc_range, "\\d{5,6}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        is.na(upper_bound) ~ "500000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(inc_range, "(?<=^inc_\\$)\\d{5,6}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = n_hh) %>%
  rename(medinc = grpmed) %>%
  group_by(geography, medinc) %>%
  summarise(total_hh = sum(n_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medinc = if_else(total_hh <= 20, NA_real_, medinc)
  )

medinc_xwalktract_20 <-
  inc_xwalktract_20 %>%
  rename_with(~str_replace(., "estimate_total_", "inc_")) %>%
  select(geography, starts_with("inc_")) %>%
  # derive medians for income and rent from grouped data
  pivot_longer(
    cols = starts_with("inc_"),
    names_to = "inc_range",
    values_to = "n_hh"
  ) %>%
  mutate(
    upper_bound = str_extract(inc_range, "\\d{5,6}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        is.na(upper_bound) ~ "500000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_extract(inc_range, "(?<=^inc_\\$)\\d{5,6}") %>% as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = n_hh) %>%
  rename(medinc = grpmed) %>%
  group_by(geography, medinc) %>%
  summarise(total_hh = sum(n_hh)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medinc = if_else(total_hh <= 20, NA_real_, medinc)
  )

# Note: median derived from income distribution data for xwalked data
medinc_tract2010_df <-
  tract00_10 %>%
  select(geography = tract_2010, medinc = medinc_00) %>%
  medinc_cleaner(2000) %>%
  bind_rows(
    medinc_cleaner(medinc_tract_10, 2010),
    medinc_cleaner(medinc_tract_11, 2011),
    medinc_cleaner(medinc_tract_12, 2012),
    medinc_cleaner(medinc_tract_13, 2013),
    medinc_cleaner(medinc_tract_14, 2014),
    medinc_cleaner(medinc_tract_15, 2015),
    medinc_cleaner(medinc_tract_16, 2016),
    medinc_cleaner(medinc_tract_17, 2017),
    medinc_cleaner(medinc_tract_18, 2018),
    medinc_cleaner(medinc_tract_19, 2019),
    medinc_cleaner(medinc_xwalktract_20, 2020),
    medinc_cleaner(medinc_xwalktract_21, 2021),
    medinc_cleaner(medinc_xwalktract_22, 2022)
  ) %>%
  medinc_join("trct")

inc_tract2010_df <-
  tract00_10 %>%
  select(geography = tract_2010, starts_with("inc_")) %>%
  rename_with(~str_remove(., "_00$")) %>%
  mutate(estimate_total = "") %>%
  inc_cleaner(2000) %>%
  bind_rows(
    inc_cleaner(inc_tract_10, 2010),
    inc_cleaner(inc_tract_11, 2011),
    inc_cleaner(inc_tract_12, 2012),
    inc_cleaner(inc_tract_13, 2013),
    inc_cleaner(inc_tract_14, 2014),
    inc_cleaner(inc_tract_15, 2015),
    inc_cleaner(inc_tract_16, 2016),
    inc_cleaner(inc_tract_17, 2017),
    inc_cleaner(inc_tract_18, 2018),
    inc_cleaner(inc_tract_19, 2019),
    inc_cleaner(inc_xwalktract_20, 2020),
    inc_cleaner(inc_xwalktract_21, 2021),
    inc_cleaner(inc_xwalktract_22, 2022)
  ) %>%
  inc_join("trct")

## Race
race_xwalktract_22 <- race_bg_22 %>% xwalker_20to10(wt = wt_pop)
race_xwalktract_21 <- race_bg_21 %>% xwalker_20to10(wt = wt_pop)
race_xwalktract_20 <- race_bg_20 %>% xwalker_20to10(wt = wt_pop)

race_tract2010_df <-
  tract00_10 %>%
  select(geography = tract_2010, starts_with("nh"), latino_00) %>%
  rename_with(~str_remove(., "_00$")) %>%
  transmute(
    geography = geography,
    pop_total = nhwhite + nhblack + nhaian + nhasian + nhpac + nhother + latino,
    pop_nhwhite = nhwhite,
    pct_nhwhite = pop_nhwhite / pop_total,
    pop_nhblack = nhblack,
    pct_nhblack = pop_nhblack / pop_total,
    pop_nhaian = nhaian,
    pct_nhaian = pop_nhaian / pop_total,
    pop_nhasian = nhasian,
    pct_nhasian = pop_nhasian / pop_total,
    pop_nhpac = nhpac,
    pct_nhpac = pop_nhpac / pop_total,
    pop_nhother = nhother,
    pct_nhother = pop_nhother / pop_total,
    pop_latino = latino,
    pct_latino = pop_latino / pop_total,
    year = 2000
  ) %>%
  bind_rows(
    raceeth_cleaner(
      race_tract_10 %>%
        rename_with(
          ~str_replace(., "total", "estimate_total") %>%
            str_remove("population_of_one_race_")
        ), 2010
    ),
    raceeth_cleaner(race_tract_11, 2011),
    raceeth_cleaner(race_tract_12, 2012),
    raceeth_cleaner(race_tract_13, 2013),
    raceeth_cleaner(race_tract_14, 2014),
    raceeth_cleaner(race_tract_15, 2015),
    raceeth_cleaner(race_tract_16, 2016),
    raceeth_cleaner(race_tract_17, 2017),
    raceeth_cleaner(race_tract_18, 2018),
    raceeth_cleaner(race_tract_19, 2019),
    raceeth_cleaner(
      race_xwalktract_20 %>%
        rename_with(
          ~str_replace(., "_total", "estimate_total") %>%
            str_remove("population_of_one_race_")
        ),
      2020
    ),
    raceeth_cleaner(race_xwalktract_21, 2021),
    raceeth_cleaner(race_xwalktract_22, 2022)
  ) %>%
  raceeth_join("trct")

## Home value
hval_xwalktract_22 <-
  hval_bg_22 %>%
  xwalker_20to10(wt = wt_hu) %>%
  rename_with(~str_replace_all(., "estimate_total", "hval")) %>%
  rename(hval_hu = hval) %>%
  select(geography, starts_with("hval_"))

medhval_xwalktract_22 <-
  hval_xwalktract_22 %>%
  select(-hval_hu) %>%
  pivot_longer(
    cols = starts_with("hval_"),
    names_to = "hval_range",
    values_to = "hval_hu"
  ) %>%
  mutate(
    upper_bound =
      str_remove_all(hval_range, "\\,+") %>% str_extract("\\d{5,8}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        # $2M+ is upper bound, set to $3M
        is.na(upper_bound) ~ "3000000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_remove_all(hval_range, "\\,+") %>%
      str_extract("(?<=^hval_\\$)\\d{5,8}") %>%
      as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = hval_hu) %>%
  rename(medhval = grpmed) %>%
  group_by(geography, medhval) %>%
  summarise(hval_hu = sum(hval_hu)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medhval = if_else(medhval >= 2000000, 2000000, medhval),
    medhval = if_else(hval_hu <= 20, NA_real_, medhval)
  )

hval_xwalktract_21 <-
  hval_bg_21 %>%
  xwalker_20to10(wt = wt_hu) %>%
  rename_with(~str_replace_all(., "estimate_total", "hval")) %>%
  rename(hval_hu = hval) %>%
  select(geography, starts_with("hval_"))

medhval_xwalktract_21 <-
  hval_xwalktract_21 %>%
  select(-hval_hu) %>%
  pivot_longer(
    cols = starts_with("hval_"),
    names_to = "hval_range",
    values_to = "hval_hu"
  ) %>%
  mutate(
    upper_bound =
      str_remove_all(hval_range, "\\,+") %>% str_extract("\\d{5,8}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        # $2M+ is upper bound, set to $3M
        is.na(upper_bound) ~ "3000000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_remove_all(hval_range, "\\,+") %>%
      str_extract("(?<=^hval_\\$)\\d{5,8}") %>%
      as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = hval_hu) %>%
  rename(medhval = grpmed) %>%
  group_by(geography, medhval) %>%
  summarise(hval_hu = sum(hval_hu)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medhval = if_else(medhval >= 2000000, 2000000, medhval),
    medhval = if_else(hval_hu <= 20, NA_real_, medhval)
  )

hval_xwalktract_20 <-
  hval_bg_20 %>%
  xwalker_20to10(wt = wt_hu) %>%
  rename_with(~str_replace_all(., "estimate_total", "hval")) %>%
  rename(hval_hu = hval) %>%
  select(geography, starts_with("hval_"))

medhval_xwalktract_20 <-
  hval_xwalktract_20 %>%
  select(-hval_hu) %>%
  pivot_longer(
    cols = starts_with("hval_"),
    names_to = "hval_range",
    values_to = "hval_hu"
  ) %>%
  mutate(
    upper_bound =
      str_remove_all(hval_range, "\\,+") %>% str_extract("\\d{5,8}$"),
    upper_bound =
      case_when(
        upper_bound == "10000" ~ "9999",
        # $2M+ is upper bound, set to $3M
        is.na(upper_bound) ~ "3000000",
        TRUE ~ upper_bound
      ) %>% as.double(),
    lower_bound =
      str_remove_all(hval_range, "\\,+") %>%
      str_extract("(?<=^hval_\\$)\\d{5,8}") %>%
      as.double(),
    lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
  ) %>%
  grouped_median(geography = geography, observations = hval_hu) %>%
  rename(medhval = grpmed) %>%
  group_by(geography, medhval) %>%
  summarise(hval_hu = sum(hval_hu)) %>%
  ungroup() %>%
  mutate(
    # adjust interpolated medians to better align with given census data
    medhval = if_else(medhval >= 2000000, 2000000, medhval),
    medhval = if_else(hval_hu <= 20, NA_real_, medhval)
  )

# Note: median derived from home value distribution data for xwalked data
medhval_tract2010_df <-
  tract00_10 %>%
  select(geography = tract_2010, medhval = medhval_00, hval_hu = hval_hu_00) %>%
  medhval_cleaner(2000) %>%
  bind_rows(
    medhval_cleaner(
      medhval_tract_10 %>%
        left_join(
          hval_tract_10 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2010
    ),
    medhval_cleaner(
      medhval_tract_11 %>%
        left_join(
          hval_tract_11 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2011
    ),
    medhval_cleaner(
      medhval_tract_12 %>%
        left_join(
          hval_tract_12 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2012
    ),
    medhval_cleaner(
      medhval_tract_13 %>%
        left_join(
          hval_tract_13 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2013
    ),
    medhval_cleaner(
      medhval_tract_14 %>%
        left_join(
          hval_tract_14 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2014
    ),
    medhval_cleaner(
      medhval_tract_15 %>%
        left_join(
          hval_tract_15 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2015
    ),
    medhval_cleaner(
      medhval_tract_16 %>%
        left_join(
          hval_tract_16 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2016
    ),
    medhval_cleaner(
      medhval_tract_17 %>%
        left_join(
          hval_tract_17 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2017
    ),
    medhval_cleaner(
      medhval_tract_18 %>%
        left_join(
          hval_tract_18 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2018
    ),
    medhval_cleaner(
      medhval_tract_19 %>%
        left_join(
          hval_tract_19 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2019
    ),
    medhval_cleaner(medhval_xwalktract_20, 2020),
    medhval_cleaner(medhval_xwalktract_21, 2021),
    medhval_cleaner(medhval_xwalktract_22, 2022)
  ) %>%
  medhval_join("trct")

# Create final 2010 dataset
tract2010_df <-
  # Join all ACS/census tract data
  medrent_tract2010_df %>%
  left_join(medinc_tract2010_df) %>%
  left_join(inc_tract2010_df) %>%
  left_join(race_tract2010_df) %>%
  left_join(medhval_tract2010_df) %>%
  rename(tract2010 = geography) %>%
  mutate(
    county = str_extract(tract2010, "^\\d{0,5}"),
    county = str_remove(county, "^06")
  ) %>%
  # join to county dataframe
  left_join(county_df, by = c("county" = "geography")) %>%
  # join with regions
  ## NOTE: does not include rural BGs (assigned to non-rural region)
  left_join(
    tcac_opp_df %>%
      transmute(county = str_remove(county, "^06"), region = region) %>%
      filter(!str_detect(region, "Rural")) %>%
      distinct(),
    by = "county"
  ) %>%
  mutate(region = if_else(is.na(region), "Rural Counties", region)) %>%
  # join with regional median incomes/rents and race
  left_join(medinc_region_df) %>%
  left_join(medrent_region_df) %>%
  left_join(race_region_df) %>%
  # rearrange data
  select(tract2010, county, region, everything()) %>%
  rename_with(.cols = contains("hval_hu"), ~ str_replace(., "hval_hu", "ownhu"))

## 2020 boundaries ------------------------------------------------------

## 2010-2020 BG Crosswalk function
xwalker_10to20 <- function(df, geo = geography, wt) {

  wt <- enquo(wt)
  geo <- enquo(geo)

  df <-
    df %>%
    rename(geography = !!geo) %>%
    left_join(
      xwalk_10to20 %>% select(bg2020ge, bg2010ge, !!wt),
      by = c("geography" = "bg2010ge"),
      multiple = "all"
    ) %>%
    select(-c(geography, contains("geographic_area_name"))) %>%
    mutate(across(.cols = contains("estimate"), ~ . * !!wt)) %>%
    mutate(geography = str_extract(bg2020ge, "\\d{11}")) %>%
    select(geography, everything()) %>%
    select(-c(!!wt, bg2020ge)) %>%
    group_by(geography) %>%
    summarise(across(.cols = everything(), ~sum(., na.rm = TRUE) %>% round()))
  df
}

# NOTE: Create additional helper functions to crosswalk 2010 boundary data to
# 2020 boundaries in each subsection
rent_xwalk_pre15_clean <- function(df) {

  df %>%
    rename_with(~str_remove(., "estimate_total_with_cash_")) %>%
    pivot_longer(
      cols = starts_with("rent_"),
      names_to = "rent_range",
      values_to = "renter_hh"
    ) %>%
    mutate(
      upper_bound = str_extract(rent_range, "\\d{3,4}$"),
      upper_bound =
        case_when(
          upper_bound == "100" ~ "99",
          is.na(upper_bound) ~ "5000",
          TRUE ~ upper_bound
        ) %>% as.double(),
      lower_bound =
        str_extract(rent_range, "(?<=^rent_\\$)\\d{3,4}") %>% as.double(),
      lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
    ) %>%
    grouped_median(geography = geography, observations = renter_hh) %>%
    rename(medrent = grpmed)  %>%
    group_by(geography, medrent) %>%
    summarise(renter_hh = sum(renter_hh)) %>%
    ungroup() %>%
    mutate(
      # adjust interpolated medians to better align with given census data
      medrent = if_else(medrent >= 2000, 2000, medrent),
      medrent = if_else(renter_hh <= 20, NA_real_, medrent)
    )
}

rent_xwalk_post15_clean <- function(df) {

  df %>%
    rename_with(~str_remove(., "estimate_total_with_cash_")) %>%
    pivot_longer(
      cols = starts_with("rent_"),
      names_to = "rent_range",
      values_to = "renter_hh"
    ) %>%
    mutate(
      upper_bound = str_extract(rent_range, "\\d{3,4}$"),
      upper_bound =
        case_when(
          upper_bound == "100" ~ "99",
          is.na(upper_bound) ~ "5000",
          TRUE ~ upper_bound
        ) %>% as.double(),
      lower_bound =
        str_extract(rent_range, "(?<=^rent_\\$)\\d{3,4}") %>% as.double(),
      lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
    ) %>%
    grouped_median(geography = geography, observations = renter_hh) %>%
    rename(medrent = grpmed)  %>%
    group_by(geography, medrent) %>%
    summarise(renter_hh = sum(renter_hh)) %>%
    ungroup() %>%
    mutate(
      # adjust interpolated medians to better align with given census data
      medrent = if_else(medrent >= 3500, 3500, medrent),
      medrent = if_else(renter_hh <= 20, NA_real_, medrent)
    )
}

medinc_xwalk_clean <- function(df) {
  df %>%
    rename_with(~str_replace(., "estimate_total_", "inc_")) %>%
    select(geography, starts_with("inc_")) %>%
    # derive medians for income and rent from grouped data
    pivot_longer(
      cols = starts_with("inc_"),
      names_to = "inc_range",
      values_to = "n_hh"
    ) %>%
    mutate(
      upper_bound = str_extract(inc_range, "\\d{5,6}$"),
      upper_bound =
        case_when(
          upper_bound == "10000" ~ "9999",
          is.na(upper_bound) ~ "500000",
          TRUE ~ upper_bound
        ) %>% as.double(),
      lower_bound =
        str_extract(inc_range, "(?<=^inc_\\$)\\d{5,6}") %>% as.double(),
      lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
    ) %>%
    grouped_median(geography = geography, observations = n_hh) %>%
    rename(medinc = grpmed) %>%
    group_by(geography, medinc) %>%
    summarise(total_hh = sum(n_hh)) %>%
    ungroup() %>%
    mutate(
      # adjust interpolated medians to better align with given census data
      #medinc = if_else(medinc >= 200000, 200000, medinc),
      medinc = if_else(total_hh <= 20, NA_real_, medinc)
    )
}

## Rent
medrent_xwalktract_13 <-
  rent_bg_13 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_pre15_clean()
medrent_xwalktract_14 <-
  rent_bg_14 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_pre15_clean()
medrent_xwalktract_15 <-
  rent_bg_15 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_post15_clean()
medrent_xwalktract_16 <-
  rent_bg_16 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_post15_clean()
medrent_xwalktract_17 <-
  rent_bg_17 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_post15_clean()
medrent_xwalktract_18 <-
  rent_bg_18 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_post15_clean()
medrent_xwalktract_19 <-
  rent_bg_19 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_post15_clean()

# Note: median derived from rent distribution data for xwalked data
medrent_tract2020_df <-
  tract00_20 %>%
  select(
    geography = tract_2020, medrent = medrent_00, renter_hh = renter_hh_00
  ) %>%
  medrent_cleaner(2000) %>%
  bind_rows(
    medrent_cleaner(medrent_xwalktract_13, 2013),
    medrent_cleaner(medrent_xwalktract_14, 2014),
    medrent_cleaner(medrent_xwalktract_15, 2015),
    medrent_cleaner(medrent_xwalktract_16, 2016),
    medrent_cleaner(medrent_xwalktract_17, 2017),
    medrent_cleaner(medrent_xwalktract_18, 2018),
    medrent_cleaner(medrent_xwalktract_19, 2019),
    medrent_cleaner(medrent_tract_20, 2020),
    medrent_cleaner(medrent_tract_21, 2021),
    medrent_cleaner(medrent_tract_22, 2022)
  ) %>%
  medrent_join("trct")

## Income
inc_xwalktract_13 <- inc_bg_13 %>% xwalker_10to20(wt = wt_hh)
inc_xwalktract_14 <- inc_bg_14 %>% xwalker_10to20(wt = wt_hh)
inc_xwalktract_15 <- inc_bg_15 %>% xwalker_10to20(wt = wt_hh)
inc_xwalktract_16 <- inc_bg_16 %>% xwalker_10to20(wt = wt_hh)
inc_xwalktract_17 <- inc_bg_17 %>% xwalker_10to20(wt = wt_hh)
inc_xwalktract_18 <- inc_bg_18 %>% xwalker_10to20(wt = wt_hh)
inc_xwalktract_19 <- inc_bg_19 %>% xwalker_10to20(wt = wt_hh)

medinc_xwalktract_13 <- inc_xwalktract_13 %>% medinc_xwalk_clean()
medinc_xwalktract_14 <- inc_xwalktract_14 %>% medinc_xwalk_clean()
medinc_xwalktract_15 <- inc_xwalktract_15 %>% medinc_xwalk_clean()
medinc_xwalktract_16 <- inc_xwalktract_16 %>% medinc_xwalk_clean()
medinc_xwalktract_17 <- inc_xwalktract_17 %>% medinc_xwalk_clean()
medinc_xwalktract_18 <- inc_xwalktract_18 %>% medinc_xwalk_clean()
medinc_xwalktract_19 <- inc_xwalktract_19 %>% medinc_xwalk_clean()

medinc_tract2020_df <-
  tract00_20 %>%
  select(
    geography = tract_2020, medinc = medinc_00, total_hh = total_hh_00
  ) %>%
  medinc_cleaner(2000) %>%
  bind_rows(
    medinc_cleaner(medinc_xwalktract_13, 2013),
    medinc_cleaner(medinc_xwalktract_14, 2014),
    medinc_cleaner(medinc_xwalktract_15, 2015),
    medinc_cleaner(medinc_xwalktract_16, 2016),
    medinc_cleaner(medinc_xwalktract_17, 2017),
    medinc_cleaner(medinc_xwalktract_18, 2018),
    medinc_cleaner(medinc_xwalktract_19, 2019),
    medinc_cleaner(medinc_tract_20, 2020),
    medinc_cleaner(medinc_tract_21, 2021),
    medinc_cleaner(medinc_tract_22, 2022)
  ) %>%
  medinc_join("trct")

inc_tract2020_df <-
  tract00_20 %>%
  select(
    geography = tract_2020,
    starts_with("inc_")
  ) %>%
  rename_with(
    .cols = starts_with("inc_"),
    ~str_replace(., "inc_", "estimate_total_") %>% str_remove("_00$")
  ) %>%
  mutate(estimate_total = "") %>%
  inc_cleaner(2000) %>%
  bind_rows(
    inc_cleaner(inc_xwalktract_13, 2013),
    inc_cleaner(inc_xwalktract_14, 2014),
    inc_cleaner(inc_xwalktract_15, 2015),
    inc_cleaner(inc_xwalktract_16, 2016),
    inc_cleaner(inc_xwalktract_17, 2017),
    inc_cleaner(inc_xwalktract_18, 2018),
    inc_cleaner(inc_xwalktract_19, 2019),
    inc_cleaner(inc_tract_20, 2020),
    inc_cleaner(inc_tract_21, 2021),
    inc_cleaner(inc_tract_22, 2022)
  ) %>%
  inc_join("trct")

## Race
race_xwalktract_10 <-
  race_bg_10 %>% select(-contains("errata")) %>%  xwalker_10to20(wt = wt_pop)
race_xwalktract_13 <- race_bg_13 %>% xwalker_10to20(wt = wt_pop)
race_xwalktract_14 <- race_bg_14 %>% xwalker_10to20(wt = wt_pop)
race_xwalktract_15 <- race_bg_15 %>% xwalker_10to20(wt = wt_pop)
race_xwalktract_16 <- race_bg_16 %>% xwalker_10to20(wt = wt_pop)
race_xwalktract_17 <- race_bg_17 %>% xwalker_10to20(wt = wt_pop)
race_xwalktract_18 <- race_bg_18 %>% xwalker_10to20(wt = wt_pop)
race_xwalktract_19 <- race_bg_19 %>% xwalker_10to20(wt = wt_pop)

race_tract2020_df <-
  tract00_20 %>%
  rename_with(~str_remove(., "_00$")) %>%
  transmute(
    geography = tract_2020,
    pop_total = total_pop,
    pop_nhwhite = nhwhite,
    pct_nhwhite = pop_nhwhite / pop_total,
    pop_nhblack = nhblack,
    pct_nhblack = pop_nhblack / pop_total,
    pop_nhaian = nhaian,
    pct_nhaian = pop_nhaian / pop_total,
    pop_nhasian = nhasian,
    pct_nhasian = pop_nhasian / pop_total,
    pop_nhpac = nhpac,
    pct_nhpac = pop_nhpac / pop_total,
    pop_nhother = nhother,
    pct_nhother = pop_nhother / pop_total,
    pop_latino = latino,
    pct_latino = pop_latino / pop_total,
    year = 2000
  ) %>%
  bind_rows(
    race_xwalktract_10 %>%
      rename(
        estimate_total = total,
        estimate_total_hispanic_or_latino = total_hispanic_or_latino,
        estimate_total_not_hispanic_or_latino_white_alone =
          total_not_hispanic_or_latino_population_of_one_race_white_alone,
        estimate_total_not_hispanic_or_latino_black_or_african_american_alone =
          total_not_hispanic_or_latino_population_of_one_race_black_or_african_american_alone,
        estimate_total_not_hispanic_or_latino_american_indian_and_alaska_native_alone =
          total_not_hispanic_or_latino_population_of_one_race_american_indian_and_alaska_native_alone,
        estimate_total_not_hispanic_or_latino_asian_alone =
          total_not_hispanic_or_latino_population_of_one_race_asian_alone,
        estimate_total_not_hispanic_or_latino_native_hawaiian_and_other_pacific_islander_alone =
          total_not_hispanic_or_latino_population_of_one_race_native_hawaiian_and_other_pacific_islander_alone
      ) %>%
      raceeth_cleaner(2010),
    raceeth_cleaner(race_xwalktract_13, 2013),
    raceeth_cleaner(race_xwalktract_14, 2014),
    raceeth_cleaner(race_xwalktract_15, 2015),
    raceeth_cleaner(race_xwalktract_16, 2016),
    raceeth_cleaner(race_xwalktract_17, 2017),
    raceeth_cleaner(race_xwalktract_18, 2018),
    raceeth_cleaner(race_xwalktract_19, 2019),
    raceeth_cleaner(
      race_tract_20 %>%
        rename_with(
          ~str_replace(., "_total", "estimate_total") %>%
            str_remove("population_of_one_race_")
        ),
      2020
    ),
    raceeth_cleaner(race_tract_21, 2021),
    raceeth_cleaner(race_tract_22, 2022)
  ) %>%
  raceeth_join("trct")

## Home Value

# NOTE: two xwalkers needed, $1M cap expanded to $2M in 2015 ACS

hvalearly_xwalk_clean <- function(df) {
  df %>%
    rename_with(~str_replace(., "estimate_total", "hval")) %>%
    select(geography, starts_with("hval_")) %>%
    pivot_longer(
      cols = starts_with("hval_"),
      names_to = "hval_range",
      values_to = "hval_hu"
    ) %>%
    mutate(
      upper_bound =
        str_remove_all(hval_range, "\\,+") %>% str_extract("\\d{5,8}$"),
      upper_bound =
        case_when(
          upper_bound == "10000" ~ "9999",
          # $1M+ is upper bound, set to $2M
          is.na(upper_bound) ~ "2000000",
          TRUE ~ upper_bound
        ) %>% as.double(),
      lower_bound =
        str_remove_all(hval_range, "\\,+") %>%
        str_extract("(?<=^hval_\\$)\\d{5,8}") %>%
        as.double(),
      lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
    ) %>%
    grouped_median(geography = geography, observations = hval_hu) %>%
    rename(medhval = grpmed) %>%
    group_by(geography, medhval) %>%
    summarise(hval_hu = sum(hval_hu)) %>%
    ungroup() %>%
    mutate(
      # adjust interpolated medians to better align with given census data
      medhval = if_else(medhval >= 1000000, 1000000, medhval),
      medhval = if_else(hval_hu <= 20, NA_real_, medhval)
    )
}

hvallate_xwalk_clean <- function(df) {
  df %>%
    rename_with(~str_replace(., "estimate_total", "hval")) %>%
    select(geography, starts_with("hval_")) %>%
    pivot_longer(
      cols = starts_with("hval_"),
      names_to = "hval_range",
      values_to = "hval_hu"
    ) %>%
    mutate(
      upper_bound =
        str_remove_all(hval_range, "\\,+") %>% str_extract("\\d{5,8}$"),
      upper_bound =
        case_when(
          upper_bound == "10000" ~ "9999",
          # $2M+ is upper bound, set to $3M
          is.na(upper_bound) ~ "3000000",
          TRUE ~ upper_bound
        ) %>% as.double(),
      lower_bound =
        str_remove_all(hval_range, "\\,+") %>%
        str_extract("(?<=^hval_\\$)\\d{5,8}") %>%
        as.double(),
      lower_bound = if_else(is.na(lower_bound), 0, lower_bound)
    ) %>%
    grouped_median(geography = geography, observations = hval_hu) %>%
    rename(medhval = grpmed) %>%
    group_by(geography, medhval) %>%
    summarise(hval_hu = sum(hval_hu)) %>%
    ungroup() %>%
    mutate(
      # adjust interpolated medians to better align with given census data
      medhval = if_else(medhval >= 2000000, 2000000, medhval),
      medhval = if_else(hval_hu <= 20, NA_real_, medhval)
    )
}


medhval_xwalktract_13 <-
  hval_bg_13 %>% xwalker_10to20(wt = wt_hu) %>% hvalearly_xwalk_clean()
medhval_xwalktract_14 <-
  hval_bg_14 %>% xwalker_10to20(wt = wt_hu) %>% hvalearly_xwalk_clean()

medhval_xwalktract_15 <-
  hval_bg_15 %>% xwalker_10to20(wt = wt_hu) %>% hvallate_xwalk_clean()
medhval_xwalktract_16 <-
  hval_bg_16 %>% xwalker_10to20(wt = wt_hu) %>% hvallate_xwalk_clean()
medhval_xwalktract_17 <-
  hval_bg_17 %>% xwalker_10to20(wt = wt_hu) %>% hvallate_xwalk_clean()
medhval_xwalktract_18 <-
  hval_bg_18 %>% xwalker_10to20(wt = wt_hu) %>% hvallate_xwalk_clean()
medhval_xwalktract_19 <-
  hval_bg_19 %>% xwalker_10to20(wt = wt_hu) %>% hvallate_xwalk_clean()

# Note: median derived from home value distribution data for xwalked data
medhval_tract2020_df <-
  tract00_20 %>%
  select(
    geography = tract_2020, medhval = medhval_00, hval_hu = hval_hu_00
  ) %>%
  medhval_cleaner(2000) %>%
  bind_rows(
    medhval_cleaner(medhval_xwalktract_13, 2013),
    medhval_cleaner(medhval_xwalktract_14, 2014),
    medhval_cleaner(medhval_xwalktract_15, 2015),
    medhval_cleaner(medhval_xwalktract_16, 2016),
    medhval_cleaner(medhval_xwalktract_17, 2017),
    medhval_cleaner(medhval_xwalktract_18, 2018),
    medhval_cleaner(medhval_xwalktract_19, 2019),
    medhval_cleaner(
      medhval_tract_20 %>%
        left_join(
          hval_tract_20 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2020
    ),
    medhval_cleaner(
      medhval_tract_21 %>%
        left_join(
          hval_tract_21 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2021
    ),
    medhval_cleaner(
      medhval_tract_22 %>%
        left_join(
          hval_tract_22 %>%
            select(geography, hval_hu = estimate_total,
                   moe_hval_hu = margin_of_error_total)
        ),
      2022
    )
  ) %>%
  medhval_join("trct")

# Create final 2020 dataset
tract2020_df <-
  medrent_tract2020_df %>%
  left_join(medinc_tract2020_df) %>%
  left_join(inc_tract2020_df) %>%
  left_join(race_tract2020_df) %>%
  left_join(medhval_tract2020_df) %>%
  rename(tract2020 = geography) %>%
  mutate(
    county = str_extract(tract2020, "^\\d{0,5}"),
    county = str_remove(county, "^06")
  ) %>%
  # join to county df
  left_join(county_df, by = c("county" = "geography")) %>%
  distinct() %>%
  filter(!is.na(tract2020)) %>%
  # join with regional data
  left_join(
    tcac_opp_df %>%
      transmute(county = str_remove(county, "^06"), region = region) %>%
      filter(!str_detect(region, "Rural")) %>%
      distinct(),
    by = "county"
  ) %>%
  mutate(region = if_else(is.na(region), "Rural Counties", region)) %>%
  left_join(region_df) %>%
  rename_with(
    .cols = contains("hval_hu"), ~ str_replace(., "hval_hu", "ownhu")
  ) %>%
  select(tract2020, county, region, everything())

# Save Results ------------------------------------------------------------

# Save 2010 universal file
tract2010_df %>%
  write_csv(paste0(homedir, savedir, "/tract2010_", todays_date, ".csv"))

# Save 2020 universal file
tract2020_df %>%
  write_csv(paste0(homedir, savedir, "/tract2020_", todays_date, ".csv"))
