# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Sept. 1, 2023                
# Last revised: Sept. 1, 2023                 
# Project: AFFH    
# Subproject: 2023 Neighborhood Change Workplan Research
# Re: Clean and prepare raw data for neighborhood change tool (streamlined version)
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script cleans and prepares raw data from ACS and other sources for use
# in the neighborhood change tool. Crosswalks 2020 census geographies to 2010
# borders and vice versa. It is a streamlined version of the original code, pared
# down to only include necessary demographic data

# Inputs:
# ACS data
# NHGIS crosswalk
# TCAC/HCD Opportunity Map, 2023

# Outputs:
# Cleaned dataset w/ 2010 boundaries
# Cleaned dataset w/ 2020 boundaries

# Update log: 

# Setup -------------------------------------------------------------------

# Packages: 
library(tidyverse)
library(glue)
library(readxl)
library(data.table)
library(sf)

# Directories: 
homedir <- here::here()
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

# TCAC/HCD Opportunity map
tcac_23 <-
  read_xlsx(
    paste0(homedir, workdir, "TCAC/final_2023_public.xlsx"),
    guess_max = 11000
  )

# Parameters
#todays_date <- format(Sys.Date(), "%m%d%Y") %>% as.character()

# Main Script -------------------------------------------------------------

# Pre-processing ---------------------------------------------------------

# Join TCAC maps (2018-2023)

## 2020-2023 do separate by BG, join together and then w/ 2018/2019
tcac_opp_df <-
  tcac_23 %>% 
  select(tract = `Census Tract`, bg = `Census Block Group`, 
         county_name = County, region = Region, 
         oppindex23 = `Composite Index Score`, oppcat23 = `Final Category`) %>% 
  # create a county FIPS column
  mutate(county = str_extract(tract, "^\\d{0,5}"))

rm(tcac_23)

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
    # remove margin of error/annotations
    select(-c(contains("Margin"), contains("Annotation"))) %>%
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
      subject == "Race" ~ "race"
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

## Join all 2000 BGs together, sum up to tracts
### 2010 boundaries
tract00_10 <-
  rent_bg_00 %>% 
  left_join(inc_bg_00) %>% 
  left_join(race_bg_00) %>% 
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

## Join all 2000 data together (2010 boundaries)
tract00_10 <-
  tract00_10 %>% 
  left_join(tract00_medinc) %>% 
  left_join(tract00_medrent) %>% 
  select(tract_2010, total_pop, renter_hh, total_hh, everything()) %>% 
  rename_with(.cols = 2:ncol(.), ~ paste0(., "_00"))

## 2020 boundaries
tract00_20 <-  
  # xwalk 2010 boundaries to 2020 tracts (build up from BG)
  rent_bg_00 %>% 
  left_join(inc_bg_00) %>% 
  left_join(race_bg_00) %>% 
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
  mutate(across(.cols = c(contains("inc"), contains("rent")), ~.*wt_hh)) %>% 
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

## Join all 2000 data by 2020 tract boundaries
tract00_20 <-
  tract00_20 %>% 
  left_join(tract00_medinc) %>% 
  left_join(tract00_medrent) %>% 
  select(tract_2020, total_pop, renter_hh, total_hh, everything()) %>% 
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
      ~if_else(str_detect(., "median_gross_rent"), "medrent", .)
    ) %>% 
    transmute(
      geography = geography, 
      year = year, 
      medrent = 
        str_replace(medrent, "0\\-", "00") %>% 
        str_remove("\\+") %>% 
        str_remove("\\-") %>%
        str_remove("\\,") %>% 
        str_remove("null") %>% 
        as.double()
    )
}

medrent_join <- function(df, geo) {
  
  df %>% 
    pivot_wider(
      names_from = year, 
      values_from = medrent, 
      names_glue = "medrent_{year}"
    ) %>% 
    rename_with(.cols = contains("medrent_"), ~paste0(geo, "_", .))
}

# Income helpers

medinc_cleaner <- function(df, year) {
  df %>%  
    rename(
      medinc = contains("median_household_income_")
    ) %>%
    transmute(
      year = year, 
      geography = geography,
      medinc =
        str_replace(medinc, "0\\-", "00") %>% 
        str_remove("\\+") %>% 
        str_remove("\\-") %>%
        str_remove("\\,") %>% 
        str_remove("null") %>% 
        as.double()
    )
}

medinc_join <- function(df, geo) {
  df %>%
    pivot_wider(
      names_from = year,
      values_from = medinc,
      names_glue = "medinc_{year}"
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
    select(
      geography,
      pop_total = estimate_total,
      pop_nhwhite = estimate_total_not_hispanic_or_latino_white_alone,
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
    ) 
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
    medrent_cleaner(medrent_county_21, 2021)
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
    medinc_cleaner(medinc_county_21, 2021)
  ) %>% 
  medinc_join("cnty")

# reshape the data to help create income groups later (eg. ELI, VLI, etc)
medinc_long <-
  medinc_county_df %>% 
  pivot_longer(
    cols = 2:ncol(.),
    names_to = "year",
    values_to = "medinc"
  ) %>% 
  mutate(year = str_remove(year, "\\w+_medinc_") %>% as.double()) %>% 
  rename(county = geography)

inc_county_df <-
  inc_cleaner(
    inc_county_00 %>% rename_with(~str_replace(., "total", "estimate_total")), 
    2000
  ) %>% 
  bind_rows(
    inc_cleaner(inc_county_10, 2010),
    inc_cleaner(inc_county_11, 2011),
    inc_cleaner(inc_county_12, 2012),
    inc_cleaner(inc_county_13, 2013),
    inc_cleaner(inc_county_14, 2014),
    inc_cleaner(inc_county_15, 2015),
    inc_cleaner(inc_county_16, 2016),
    inc_cleaner(inc_county_17, 2017),
    inc_cleaner(inc_county_18, 2018),
    inc_cleaner(inc_county_19, 2019),
    inc_cleaner(inc_county_20, 2020),
    inc_cleaner(inc_county_21, 2021)
  ) %>% 
  inc_join("cnty")

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
    raceeth_cleaner(race_county_21, 2021)
  ) %>% 
  raceeth_join("cnty")

## All
county_df <-
  medrent_county_df %>% 
  left_join(medinc_county_df) %>% 
  left_join(inc_county_df) %>% 
  left_join(race_county_df) %>% 
  mutate(geography = str_remove(geography, "^06")) 

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
### Crosswalk 2020 and 2021 data to 2010 boundaries
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
    medrent = if_else(medrent >= 2000, 2000, medrent),
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
    medrent = if_else(medrent >= 2000, 2000, medrent),
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
    medrent_cleaner(medrent_xwalktract_21, 2021)
  ) %>% 
  medrent_join("trct")

## Income
### Crosswalk 2020 and 2021 data to 2010 boundaries
inc_xwalktract_21 <- inc_bg_21 %>% xwalker_20to10(wt = wt_hh)
inc_xwalktract_20 <- inc_bg_20 %>% xwalker_20to10(wt = wt_hh)

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
    medinc = if_else(medinc >= 200000, 200000, medinc),
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
    medinc = if_else(medinc >= 200000, 200000, medinc),
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
    medinc_cleaner(medinc_xwalktract_21, 2021)
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
    inc_cleaner(inc_xwalktract_21, 2021)
  ) %>% 
  inc_join("trct")

## Race
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
    raceeth_cleaner(race_xwalktract_21, 2021)
  ) %>% 
  raceeth_join("trct")

# Create final 2010 dataset 
tract2010_df <-
  # Join all ACS/census tract data
  medrent_tract2010_df %>% 
  left_join(medinc_tract2010_df) %>% 
  left_join(inc_tract2010_df) %>% 
  left_join(race_tract2010_df) %>% 
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
  # rearrange data
  select(tract2010, county, region, everything())

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
rent_xwalk_clean <- function(df) {
  
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
      medinc = if_else(medinc >= 200000, 200000, medinc),
      medinc = if_else(total_hh <= 20, NA_real_, medinc)
    )
  
}

## Rent
medrent_xwalktract_13 <- 
  rent_bg_13 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_clean()
medrent_xwalktract_14 <- 
  rent_bg_14 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_clean()
medrent_xwalktract_15 <- 
  rent_bg_15 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_clean()
medrent_xwalktract_16 <- 
  rent_bg_16 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_clean()
medrent_xwalktract_17 <- 
  rent_bg_17 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_clean()
medrent_xwalktract_18 <-
  rent_bg_18 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_clean()
medrent_xwalktract_19 <- 
  rent_bg_19 %>% xwalker_10to20(wt = wt_hh) %>% rent_xwalk_clean()

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
    medrent_cleaner(medrent_tract_21, 2021)
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
    medinc_cleaner(medinc_tract_21, 2021)
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
    inc_cleaner(inc_tract_21, 2021)
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
    raceeth_cleaner(race_tract_21, 2021)
  ) %>% 
  raceeth_join("trct")

# Create final 2020 dataset
tract2020_df <-
  medrent_tract2020_df %>% 
  left_join(medinc_tract2020_df) %>% 
  left_join(inc_tract2020_df) %>% 
  left_join(race_tract2020_df) %>% 
  rename(tract2020 = geography) %>% 
  mutate(
    county = str_extract(tract2020, "^\\d{0,5}"),
    county = str_remove(county, "^06")
  ) %>% 
  # join to county df
  left_join(county_df, by = c("county" = "geography")) %>% 
  distinct() %>% 
  filter(!is.na(tract2020)) %>% 
  # join with regions
  left_join(
    tcac_opp_df %>% 
      transmute(county = str_remove(county, "^06"), region = region) %>%
      filter(!str_detect(region, "Rural")) %>% 
      distinct(),
    by = "county"
  ) %>% 
  mutate(region = if_else(is.na(region), "Rural Counties", region)) %>% 
  select(tract2020, county, region, everything())

# Save Results ------------------------------------------------------------

# Save 2010 universal file
tract2010_df %>%
  write_csv(paste0(homedir, savedir, "/tract2010.csv"))

# Save 2020 universal file
tract2020_df %>%
  write_csv(paste0(homedir, savedir, "/tract2020.csv"))
