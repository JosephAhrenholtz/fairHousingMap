#' Evaluate 9% new construction LIHTC across opportunity categories pre- and post-policy adoption
#'
#' @source TCAC Projects: https://www.treasurer.ca.gov/ctcac/projects.asp
#'
#' @import dplyr tidyr stringr tidygeocoder tigris



# STEP 1: LOAD, REDUCE, FORMAT FIPS
get_devels <- function(year = current_year){
  filepaths(year = year)

  tcac_devels <- read_zip(lihtc, year = year, type = 'excel', sheet = "California Mapped Projects")

  # rename and select necessary cols
  tcac_devels <- tcac_devels %>%
    rename(appid = `Application.Number`, app_stage = `Application.Stage`,
           PISdate = `Placed.in.Service.(PIS).Date`,
           address = `Project.Address`, housing_type = `Housing.Type`,
           lowinc_units = `Low.Income.Units`,
           county_name = `Project.County`, construct_type = `Construction.Type`,
           zipcode = `Project.Zip.Code`, credit_type = `Type.of.tax.credit.funding`,
           ) %>%
    # create application year column from appid
    mutate(app_year = substr(appid, 4,7)) %>%
    select(appid, app_year, app_stage, PISdate, address, zipcode, county_name,
           housing_type, construct_type, credit_type, lowinc_units)


  # ensure target variables are formatted consistently (could be written more elegantly)
  tcac_devels$housing_type[which(tcac_devels$housing_type == 'Large family')] <- 'Large Family'
  tcac_devels$housing_type[which(tcac_devels$construct_type == 'New Construction ')] <- 'New Construction'
  tcac_devels$credit_type <- str_replace_all(tcac_devels$credit_type, c("9% ARRA" = "9 %", "0.09" = "9 %"))
  tcac_devels$credit_type <- str_replace_all(tcac_devels$credit_type, c("4% ARRA" = "4 %", "0.04" = "4 %", "4%" = "4 %"))

  # filter to exclusively new construction, family-serving developments after 2014 and ensure there is an associated credit type
  tcac_devels <- tcac_devels %>%
    filter(housing_type == 'Large Family' & construct_type == 'New Construction' & as.numeric(app_year) >= 2014 & !is.na(credit_type))


  # NOTE: WE MAY NEED ADDITIONAL FILTER ON APPLICATION STAGE BUT NEED TO CONFIRM

  return(tcac_devels)

}


# STEP 2: GEOCODE
geocode_devels <- function(){
  # get devels
  devels <- get_devels()

  # taking sample for small geocoding request
  devels <- devels[sample(nrow(devels), 10),]

  devels <- devels %>% tidygeocoder::geocode(address = address, method = 'arcgis')

  # add step to filter by performance

  # writing this function to avoid successive geocoding requests
  write_csv(devels, paste0('data/intermediate/', year, '/geocoded_devels.csv'))

}



# STEP 3: JOIN TO OPPORTUNITY
join_opp <- function(year = current_year){


  # load devels and make sf
  devels <- sf::st_read(paste0("data/intermediate/", year, "/geocoded_devels.csv"))
  devels <- devels %>%
    st_as_sf(coords = c("long","lat")) %>%
    st_sf(crs = 4326)

  # load opp and reduce to necessary columns
  opp <- final_opp(write = F, as_geo = T) %>%
    select(fips, fips_bg, region, oppcat, pov_seg_flag, nbrhood_chng)

  # spatial join devels to opp cats
  opped_devels <- opp %>%
    sf::st_join(devels)

  # return to df
  opped_devels <- opped_devels %>%
    st_drop_geometry()


  return(opped_devels)


}

# STEP 4: JOIN SEG
# note: seg data is to 2010 boundaries so need to create this seperately
join_seg <- function(year = current_year){
  filepaths(year = year)

  # load devels and make sf
  devels <- sf::st_read(paste0("data/intermediate/", year, "/geocoded_devels.csv"))
  devels <- devels %>%
    st_as_sf(coords = c("long","lat")) %>%
    st_sf(crs = 4326)

  # load seg cat and make spatial
  segcat <- read_zip(segcat, year = year, type = 'csv')
  segcat <- segcat %>% select(geoid, segcat) %>% rename('fips' = geoid)
  tract_2010 <- tracts(state = "CA", year = 2010) %>% select(GEOID10, geometry) %>% rename('fips' = GEOID10)
  segcat <- segcat %>%
    left_join(tract_2010, by = 'fips') %>%
                st_as_sf(crs = 4326)

  # spatial join devels to opp cats
  segcat_devels <- segcat %>%
    sf::st_join(devels)

  # return to df
  segcat_devels <- segcat_devels %>%
    st_drop_geometry()

  return(segcat_devels)


}


# STEP 5: CREATE SUMMARY TABLES
# params need to be re-written to be more clear
# for now can call:
#         summarize_lihtc(neighb_type = 'opp', grouping =  oppcat) or
#         summarize_lihtc(neighb_type = 'seg', grouping =  segcat)

summarize_lihtc <- function(neighb_type = 'opp', grouping =  oppcat){

  if(neighb_type != 'opp'){
    df <- join_seg()
    levels <- c('High POC Segregation', 'High White Segregation', 'Low-Medium Segregation', 'Racially Integrated')
  } else {
    df <- join_opp()
    levels <- c('Low Resource', 'Moderate Resource', 'High Resource', 'Highest Resource')
  }

  tbl <- df %>%
    group_by({{grouping}}) %>%
    summarize(developments = n(), lowinc_units = sum(as.numeric(lowinc_units), na.rm = TRUE)) %>%
    mutate_at(vars(developments, lowinc_units),
              list(perc = ~round(./sum(.), 3))) %>%
     arrange(factor({{grouping}}, levels = levels))

  return(tbl)
}




