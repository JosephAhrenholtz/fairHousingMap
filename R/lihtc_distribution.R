#' Evaluate 9% new construction LIHTC across opportunity categories pre- and post-policy adoption
#'
#' @source TCAC Projects: https://www.treasurer.ca.gov/ctcac/projects.asp
#'
#' @import ggmap


# load LIHTC data, format tract numbers, filter for 9% new construction
#   in prelim reservation phase or placed in service
get_lihtc <- function(year = current_year){
  filepaths(year = year)

  tcac_devels <- read_zip(lihtc, year = year, type = 'excel', sheet = "California Mapped Projects")

  # rename and select necessary cols
  tcac_devels <- tcac_devels %>%
    rename(appid = `Application.Number`, app_stage = `Application.Stage`,
           PISdate = `Placed.in.Service.(PIS).Date`,
           address = `Project.Address`, housing_type = `Housing.Type`,
           lowinc_units = `Low.Income.Units`, tract = `Census.Tract`,
           county_name = `Project.County`, construct_type = `Construction.Type`,
           zipcode = `Project.Zip.Code`, credit_type = `Type.of.tax.credit.funding`,
           fedaward = `Annual.Federal.Award`, state_award =  `Total.State.Award`,) %>%
    # create application year column from appid
    mutate(app_year = substr(appid, 4,7)) %>%
    select(tract, county_name, app_year, app_stage, PISdate, address, zipcode, appid,
           housing_type, construct_type, credit_type, lowinc_units, fedaward, state_award)


  # format tract numbers
  tcac_devels <- tcac_devels %>%
    # separate projects with multiple developments
    tidyr::separate_rows(tract, sep = ',') %>%
    # separate digits before and after decimal
    tidyr::separate(tract, c('tract_prefix', 'tract_suffix'), sep = '\\.',
             fill = 'right') %>%
    # add trailing zeroes to tracts
    mutate(tract_prefix = str_pad(tract_prefix, 4, 'left', '0'),
           tract_suffix = str_pad(tract_suffix, 2, 'right', '0'))

  #add zeroes for missing tract suffixes
  tcac_devels$tract_suffix[which(is.na(tcac_devels$tract_suffix))] <- '00'
  #remove uncatagorizable tracts
  tcac_devels <- filter(tcac_devels, nchar(tract_prefix) < 5) %>%
    # merge tract numbers together
    unite('fips', tract_prefix, tract_suffix, sep = '')


  # merge in county codes and to make the full tract number
  county_codes <- all_census_data()[c('county_name', 'countyid')]
  county_codes <- county_codes[which(duplicated(county_codes$county_name) ==
                                       F),]
  # join lihtc to county codes
  tcac_devels <- inner_join(tcac_devels, county_codes, by='county_name') %>%
    mutate(fips = paste0(countyid, fips), zipcode =
             str_sub(zipcode, 1, 5))

  # make fips codes 11 characters long
  tcac_devels <- tcac_devels %>%
    mutate(fips = substr(fips, 1 , 11)) %>%
    mutate(fips = gsub(" ", "0", fips)) %>%
    # end tract formatting
    # Note on tract formatting: there's data entry error in the original 'Census Tract' column;
    # need to implement a system for invalidating/handling erroneous or inconsistent entries

  # start address formatting here

  return(tcac_devels)

}








