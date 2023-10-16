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
#' @import dplyr
#'
#' @export
xwalk_ces <- function(year = current_year, write = FALSE, read = !write, testing_handle=FALSE) {
  filepaths(year = year)


  filename = paste0("data/intermediate/", year,"/ces_indicators.csv")
  if(read == T) {
    return(readr::read_csv(filename, col_types = readr::cols()))
  }


  # load last year's data and select only relevant vars
  env <- final_2023 %>% select(fips, fips_bg, region, county_name,
                               cleanup_sites, hazard_waste, groundwater, solid_waste)

  # compute statewide ranking
  env_state <- env %>%
    distinct(fips, cleanup_sites, hazard_waste, groundwater, solid_waste) %>%
    mutate_at(vars(cleanup_sites:solid_waste), percent_rank) %>% ungroup() %>%
    mutate(env_site_mean_state = rowMeans(select(., cleanup_sites:solid_waste), na.rm = TRUE),
           env_site_pctl_state = percent_rank(env_site_mean_state))


  # assign region id
  env <- env %>%
    mutate(regionid = ifelse(region == "Rural Areas", county_name, region))

  # filter to tracts (from block groups)
  env <- env %>%
    distinct(fips, regionid, cleanup_sites, hazard_waste, groundwater, solid_waste)


  # calculate percentiles by regionid
  env <- env %>%
    group_by(regionid) %>%
    mutate_at(vars(cleanup_sites:solid_waste), percent_rank) %>% ungroup() %>%
    mutate(env_site_mean = rowMeans(select(., cleanup_sites:solid_waste), na.rm = TRUE),
           env_site_pctl = percent_rank(env_site_mean))


  if(testing_handle==TRUE){
    return(env)
  }


  # identify bottom 95% of site-based values by region if n >= 20, by state if n < 20
  # note: for scoring, high site-hazard geographies receive a score of 0
  env <- env %>%
    group_by(regionid) %>%
    mutate(n_geo = n()) %>%
    mutate(env_site_score = ifelse(n_geo >= 20 & env_site_pctl <= 0.95, 1, 0)) %>%
    ungroup()


  # join statewide percentile and use for regions with less than 20 geos
  env <-  env %>%
    left_join(env_state %>% select(fips, env_site_pctl_state)) %>%

    mutate(env_site_score_state = ifelse(env_site_pctl_state <= 0.95, 1, 0),
           env_site_score = ifelse(n_geo >= 20, env_site_score, env_site_score_state))


  # crosswalk for 2010-2020 transformation
  xwalk <- readr::read_delim("https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_natl.txt", delim = "|") %>%
    transmute(GEOID10 = GEOID_TRACT_10, GEOID20 = GEOID_TRACT_20,
              AREALAND20 = AREALAND_TRACT_20, AREALAND_PART20 = AREALAND_PART) %>%
    filter(substr(GEOID10, 1, 2) == "06") %>% #group_by(GEOID) %>% summarize(n = n())
    mutate(AREALAND_PART20 = AREALAND_PART20/AREALAND20)

  # apply 80% 2010-2020 xwalk land area overlap threshold for scoring
  env <- env %>%
    rename(GEOID10 = fips) %>% left_join(xwalk) %>%
    ## (If env_site_score == 0 in any overlap, 2020 tract env_site_score value = 0)
    mutate(env_site_score = ifelse(AREALAND_PART20 >= 0.8 & env_site_score == 0, 0, 1)) %>%
    select(GEOID20, env_site_score) %>%
    group_by(GEOID20) %>% summarize(env_site_score = min(env_site_score)) %>% ungroup() %>%
    rename('fips' = GEOID20)


  if(write == TRUE){
    readr::write_csv(env, filename)
  }
  else env
}
