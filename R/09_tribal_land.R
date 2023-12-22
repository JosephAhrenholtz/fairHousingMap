#' Create tribal land flag
#'
#'
#' @description
#' Imports tribal lands under of the control of federally-recognized tribes, computes intersection with Census tracts, and flags
#' any tract where at least 25 percent of the geography’s land area is within federally-recognized tribal lands.  In final_data.R,
#' High-Poverty & Segregated is not assessed in tracts where the tribal land flag is raised.
#'
#'
#' @return a dataframe
#'
#'
#' @examples
#' tribal_overlap() # loads tracts with flag for tribal land
#'
#'
#' @import sf tigris dplyr
#'
#' @export


tribal_overlap <- function(){

  # load reservation data from the census
  reservations_sf <- native_areas(year = 2021) %>% st_transform(4326)


  # reduce reservations to CA
  ca_reservations_sf <-
    reservations_sf %>%
    st_intersection(shape_CA_state) %>%
    select(NAME, NAMELSAD) %>%
    st_as_sf()

  # compute tract intersection with tribal lands
  sf_use_s2(FALSE)
  tract_intersect_pct <-
    shape_CA_tract %>%
    st_as_sf() %>%
    st_intersection(ca_reservations_sf) %>%
    mutate(intersect_area = st_area(.)) %>%
    select(fips, intersect_area) %>%
    st_drop_geometry() %>%
    group_by(fips) %>%
    summarise(intersect_area = sum(intersect_area))

  tract_res <-
    shape_CA_tract %>%
    mutate(geo_area = st_area(shape_CA_tract)) %>%
    left_join(tract_intersect_pct) %>%
    mutate(
      coverage = as.numeric(intersect_area/geo_area),
      coverage = replace_na(coverage, 0),
      # flag if 25% or more of tract is within native land
      nativeland_flag = if_else(coverage >= .25, 1, 0)
    ) %>%
    st_drop_geometry() %>%
    select(fips, nativeland_flag)

  return(tract_res)


}


