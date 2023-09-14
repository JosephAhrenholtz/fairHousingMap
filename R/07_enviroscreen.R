#' Create site-based environmental hazards variable
#'
#'
#' @description
#' Imports CalEnviroScreen 4.0 site-based measurements (cleanup sites, hazardous waste, groundwater threats, solid waste)
#' from the final 2023 TCAC file, creates a binary score for tracts in the bottom 5% of site-based hazards within regions, then
#' crosswalks to 2020 tracts using an overlay method of >= 5% of intersecting land area.  The cross walking approach is a stop gap
#' until OEHHA updates to 2020 boundaries.  The overlay approach was chosen over a weighted allocation (e.g. by area or population)
#' because the site-based measures are already interpolated by OEHHA from points to tracts, and the research partners wanted to
#' avoid re-interpolating already interpolated data.
#'
#'
#' @param year designates the appropriate filepaths
#' @param write write the intermediate file
#' @param read read an existing intermediate file
#'
#'
#' @return a data frame
#'
#'
#' @examples
#' xwalk_ces(year = 2024, write = TRUE) # writes a new file to the intermediate directory
#'
#'
#' @import dplyr readr
#'
#' @export
xwalk_ces <- function(year = current_year, write = FALSE, read = !write) {
  filepaths(year = year)


  filename = paste0("data/intermediate/", year,"/ces_indicators.csv")
  if(read == T) {
    return(readr::read_csv(filename, col_types = readr::cols()))
  }


  # load last year's data and select only relevant vars
  env <- final_2023 %>% select(fips, fips_bg, region, county_name,
                                  cleanup_sites, hazard_waste, groundwater, solid_waste)


  # re-level so rural areas within each county are first level
  env <- env %>%
    mutate(region = relevel(factor(region), ref = "Rural Areas")) %>%
    arrange(county_name, region)

  # group by county and reassign rural areas to urban region if in a county with urban tracts
  env <- env %>%
    group_by(county_name) %>%
    mutate(regionid = ifelse(region == "Rural Areas", last(as.character(region)),
                             as.character(region))) %>%
    ungroup()

  # assign remaining rural block groups to county of origin
  env <- env %>%
    mutate(regionid = ifelse(regionid == "Rural Areas", county_name, regionid))

  # filter to tracts (from block groups)
  env <- env %>%
    distinct(fips, regionid, cleanup_sites, hazard_waste, groundwater, solid_waste)


  # calculate percentiles by regionid
  env <- env %>%
    group_by(regionid) %>%
    mutate_at(vars(cleanup_sites:solid_waste), percent_rank) %>% ungroup() %>%
    mutate(env_site_mean = rowMeans(select(., cleanup_sites:solid_waste), na.rm = TRUE)) %>%
    # additional field to feed interface chart
    mutate(env_site_pctl = percent_rank(env_site_mean))

  # identify bottom 95% of site-based values
  # note: for scoring, high site-hazard geographies receive a score of 0
  env <- env %>%
    mutate(env_site_score = ifelse(env_site_mean <= quantile(env_site_mean, 0.95, na.rm = TRUE), 1, 0)) %>%
    ungroup()


  # crosswalk for 2010-2020 transformation
  xwalk <- readr::read_delim("https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_natl.txt", delim = "|") %>%
    transmute(GEOID10 = GEOID_TRACT_10, GEOID20 = GEOID_TRACT_20,
              AREALAND20 = AREALAND_TRACT_20, AREALAND_PART20 = AREALAND_PART) %>%
    filter(substr(GEOID10, 1, 2) == "06") %>% #group_by(GEOID) %>% summarize(n = n())
    mutate(AREALAND_PART20 = AREALAND_PART20/AREALAND20)

  # join to 2020 crosswalk and retain tracts overlapping by >= 5% of 2020 area
  env <- env %>%
    rename(GEOID10 = fips) %>% left_join(xwalk) %>% filter(AREALAND_PART20 >= 0.05) %>%
    # keep only first row by tract (If site == 0 in any overlap, 2020 tract site value = 0)
    arrange(GEOID20, env_site_score) %>% rename(fips = GEOID20) %>%
    select(fips, cleanup_sites:solid_waste, env_site_mean, env_site_pctl, env_site_score) %>%
    group_by(fips) %>% summarize_all(first) %>% ungroup()


  if(write == TRUE){
    readr::write_csv(env, filename)
  }
  else env
}
