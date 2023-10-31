#' Creates tract/block group education scores
#'
#' @details Finds school distance to tract or block group centroids, and averages
#'   the reading and math scores, frpm, and graduation rates of the three nearest
#'   schools.  4th grade and FRPM are weighted by the enrollment of schools that
#'   serve 4th-graders, and graduation rates are weighted by cohort size.
#'
#' @param year designates the appropriate filepaths
#' @param geo tract or bg
#' @param write write the intermediate file
#' @param read read an existing intermediate file
#'
#' @return a data frame
#'
#' @examples
#' school_distances(year = 2024, geo = 'tract', write = TRUE, read = FALSE) # writes a new file to the intermediate directory
#'
#' @import dplyr testit
#' @importFrom geosphere distGeo
#'
#' @export
school_distances <- function(year = current_year, geo = 'tract', write = FALSE, read = !write, testing_handle=FALSE){
  filename = paste0("data/intermediate/", year,'/education_indicators_', geo, '.csv.gz')
  if(read == TRUE){
    distance_indicators <- readr::read_csv(filename, col_types = readr::cols())
    return(distance_indicators)
  }

  #read tract or block group centers, depending on geography selection
  centroids <- all_census_data(year = year, geo = geo) %>%
    dplyr::select(dplyr::contains('fips'), dplyr::starts_with('total_pop_'), 'county_name')
  if(geo == 'tract'){
    centroids <- dplyr::inner_join(centroids,
      read_tract_centers(year = year)[c('fips', 'lon','lat')], by = 'fips')
  } else{
    centroids <- dplyr::inner_join(centroids,
      read_block_group_centers(year = year)[c('fips', 'fips_bg', 'lon','lat')], by = c('fips', 'fips_bg'))

    }
  filepaths(year = year)



  # Load elementary school data, then separate scores and frpm
  elem_schls <- read_educ_pov(year = year)
  #limit test scores to schools with observed 4th-grade test scores.
  test_scores <- dplyr::select(elem_schls, CDSCode, enrollment, lat, lon,
    read_prof, math_prof) %>% na.omit()
  frpm <- dplyr::select(elem_schls, CDSCode, enrollment, pct_frpm, pct_freemeals,
    lat, lon)

  school_list <- vector('list', nrow(centroids))

  high_schls <- graduation_rates(year = year)

  for(i in 1:nrow(centroids)){
    row <- centroids[i,]
    #limiting scope to schools within a ~30 mile radius for speed of processing
    near_elem <- dplyr::filter(test_scores, dplyr::between(lon, row$lon -0.5, row$lon+0.5) &
        dplyr::between(lat, row$lat -0.5, row$lat+0.5))
    near_frpm <- dplyr::filter(frpm, dplyr::between(lon, row$lon -0.5, row$lon+0.5) &
        dplyr::between(lat, row$lat -0.5, row$lat+0.5))
    near_high <- dplyr::filter(high_schls, dplyr::between(lon, row$lon -0.5, row$lon+0.5) &
        dplyr::between(lat, row$lat -0.5, row$lat+0.5))
    # for tracts with fewer than 3 schools in the above radius, include all schools
    if(nrow(near_elem) < 3 | nrow(near_high) <3){
      near_elem <- test_scores
      near_frpm <- frpm
      near_high <- high_schls
    }

    # measures distance between school long/lat and tract/bg centers.
    near_elem$distance_elem <- geosphere::distGeo(near_elem[c('lon','lat')],
      as.numeric(row[c('lon', 'lat')]))
    near_frpm$distance_frpm <- geosphere::distGeo(near_frpm[c('lon','lat')],
      as.numeric(row[c('lon', 'lat')]))
    near_high$distance_high <- geosphere::distGeo(near_high[c('lon','lat')],
      as.numeric(row[c('lon', 'lat')]))

    #sorting by distance; index will move with it for accurate join
    near_elem <- dplyr::arrange(near_elem, distance_elem)
    near_frpm <- dplyr::arrange(near_frpm, distance_frpm)
    near_high <- dplyr::arrange(near_high, distance_high)

    #reducing to top 3 results
    nearest_elem <- near_elem[1:3,]
    nearest_frpm <- near_frpm[1:3,]
    nearest_high <- near_high[1:3,]

    # use full join so both matching and non-matching schools are included
    nearest <- dplyr::full_join(nearest_elem, nearest_frpm,
      by = c('CDSCode', 'enrollment')) %>%
      dplyr::full_join(nearest_high, by = 'CDSCode', suffix = c('','_hs'))

    if(testing_handle==TRUE){
      #there should be three schools under each category (elem, frpm, high)
      testit::assert("three nearest elementary schools",sum(!is.na(nearest$distance_elem))==3)
      testit::assert("three nearest high schools",sum(!is.na(nearest$distance_high))==3)
      testit::assert("three nearest free or reduced-price meal schools",sum(!is.na(nearest$distance_frpm))==3)
      testit::assert("all schools are unique",length(unique(nearest$CDSCode))==dim(nearest)[1])
    }


    # adding tract/block group
    if(geo == 'tract') nearest$fips <- row$fips
    else nearest$fips_bg <- row$fips_bg

    #add school info using the indexes created above
    school_list[[i]] <- dplyr::select(nearest, -dplyr::matches('l[oa][nt]'))
  }

distance_indicators_Sch <- dplyr::bind_rows(school_list)
#group by either geographic boundary and summarize school results
if(geo == 'tract'){
distance_indicators_Sch <- dplyr::group_by(distance_indicators_Sch, fips)
} else{
  distance_indicators_Sch <- dplyr::group_by(distance_indicators_Sch, fips_bg)
}
distance_indicators <- dplyr::summarize(distance_indicators_Sch,
                                        pct_frpm = weighted.mean(pct_frpm,enrollment, na.rm = T),
                                        read_prof = weighted.mean(read_prof,enrollment, na.rm = T),
                                        math_prof = weighted.mean(math_prof,enrollment, na.rm = T),
                                        grad_rate = weighted.mean(grad_rate,enrollment_hs, na.rm = T),
                                        distance_elem_avg = mean(distance_elem, na.rm=TRUE)/1609.34,
                                        distance_elem_closest = min(distance_elem, na.rm = T)/1609.34,
                                        distance_elem_farthest = max(distance_elem, na.rm = T)/1609.34) # include across() as a summarize argument to keep school info


distance_indicators$pct_not_frpm <- 1 - distance_indicators$pct_frpm

# distance_indicators <- dplyr::left_join(distance_indicators, graduation_rates(), by = 'fips')

if(write == TRUE){
    readr::write_csv(distance_indicators, filename)
}
else distance_indicators
}

