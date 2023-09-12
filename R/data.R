#' fairHousingMap data dictionary
#'
#' Final 2024 Fair Housing Map data dictionary for descriptions of all variables included in the mapping interface.
#'
#' \itemize{
#'   \item fips: Census tract ID.
#'   \item fips_bg: Census block group ID.
#'   \item region: TCAC regions and Rural Areas designation.
#'   \item regionid: Reference region from which regional medians are derived.  Is distinct from region field for rural block groups.
#'   \item county_name: County name.
#'   \item pct_above_200_pov: Percent of population above 200 percent of the federal poverty level.
#'   \item home_value: Median home-value of owner occupied units.
#'   \item pct_bachelors_plus: Percent of adults with a bachelor's degree or above.
#'   \item pct_employed: Percent of adults aged 20-64 who are employed in the civilian labor force or in the armed forces.
#'   \item math_prof: Percentage of 4th graders who meet or exceed math proficiency standards.
#'   \item read_prof: Percentage of 4th graders who meet or exceed literacy standards.
#'   \item grad_rate: Percentage of high school cohort that graduated on time.
#'   \item pct_not_frpm: Percent of students not receiving free or reduced-price meals (FRPM).
#'   \item env_site_pctl: Regional percentile of averaged site-based environmental hazard indicators.
#'   \item pct_above_200_pov_median: Regional median of above 200 percent poverty indicator.
#'   \item home_value_median: Regional median of home-value indicator.
#'   \item pct_bachelors_plus_median: Regional median of bachelor's+ indicator.
#'   \item pct_employed_median: Regional median of employment indicator.
#'   \item math_prof_median: Regional median of math proficiency indicator.
#'   \item read_prof_median: Regional median of reading proficiency indicator.
#'   \item grad_rate_median: Regional median of graduation rate indicator.
#'   \item pct_not_frpm_median: Regional median of FRPM indicator.
#'   \item env_site_score: Binary score which identifies the bottom 95 percent of site-based environmental hazards.  High environmental hazard geographies receive a score of 0, all others receive a score of 1.
#'   \item pct_above_200_pov_score: Binary score where 1 denotes the indicator is above the regional median.
#'   \item home_value_score: Binary score where 1 denotes the indicator is above the regional median.
#'   \item pct_bachelors_plus_score: Binary score where 1 denotes the indicator is above the regional median.
#'   \item pct_employed_score: Binary score where 1 denotes the indicator is above the regional median.
#'   \item math_prof_score: Binary score where 1 denotes the indicator is above the regional median.
#'   \item read_prof_median: Binary score where 1 denotes the indicator is above the regional median.
#'   \item grad_rate_score: Binary score where 1 denotes the indicator is above the regional median.
#'   \item pct_not_frpm_score: Binary score where 1 denotes the indicator is above the regional median.
#'   \item oppscore: Final opportunity score:  0 or –1 = “Highest Resource”; –2 or –3  = “High Resource”; –4 or –5 = “Moderate Resource”; –6 or lower = “Low Resource”.
#'   \item pct_below_pov: Percent of population below the federal poverty line.
#'   \item lq_poc: Location quotient of all people of color relative to the county.
#'   \item lq_asian: Location quotient of Asian population relative to the county.
#'   \item lq_black: Location quotient of Black population relative to the county.
#'   \item lq_hispanic: Location quotient of Hispanic population relative to the county.
#'   \item oppcat: Opportunity designation.
#'   \item pov_seg_cat: High Poverty and Segregated designation.  Defined as least 30 percent of the population falling under the federal poverty line and Location Quotient of higher than 1.25 for Black, Hispanic, Asian, or all people of color in comparison to the county.
#'   \item nbrhood_chng: Binary flag if a tract meets the definition of either long-term (2000-2021) or recent neighborhood change (2013-2021).
#'   \item trct_raceeth_chng0021: Percentage point change in tract's non-Hispanic white population between 2000 and 2021.
#'   \item trct_raceeth_chng1321: Percentage point change in tract's non-Hispanic white population between 2013 and 2021.
#'   \item trct_inc_chng0021: Percentage point change in tract's above-moderate-income households between 2000 and 2021.
#'   \item trct_inc_chng1321: Percentage point change in tract's above-moderate-income households between 2013 and 2021.
#'   \item halfmile_buffer: Binary flag if a tract's population-weighted centroid is within 1/2-mile of the population-weighted centroid of a tract that experienced long-term change (2000-2021).
#'   \item raceeth_half0021: Countywide 50 percent threshold for non-Hispanic white tract-level percentage point increase (2000-2021).
#'   \item raceeth_half1321: Countywide 50 percent threshold for non-Hispanic white tract-level percentage point increase (2013-2021).
#'   \item inc_half0021: Countywide 50 percent threshold for above-moderate-income households tract-level percentage point increase (2000-2021).
#'   \item inc_half1321: Countywide 50 percent threshold for above-moderate-income households tract-level percentage point increase (2013-2021).
#'   \item rent_quarter1321: Countywide top 25 percent threshold for percent change in median rent (2013-2021).
#'   \item trct_pctchng_medrent1321: Percent change in tract's median rent 2013-2021.
#'
#' }
#'
#' @docType data
#' @keywords datasets
#' @usage ?final_opp_data
#' @format A data frame with 11,347 rows and 51 variables
#'
#' @name final_opp_data
NULL
