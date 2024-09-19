#' fairHousingMap data dictionary
#'
#' Final 2025 Fair Housing Map data dictionary for descriptions of all variables included in the mapping interface.
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
#'   \item oppscore: Final opportunity score:  9 or 8 = “Highest Resource”; 7 or 6  = “High Resource”; 5 or 4 = “Moderate Resource”; 3 or lower = “Low Resource”.
#'   \item pct_below_pov: Percent of population below the federal poverty line.
#'   \item lq_poc: Location quotient of all people of color relative to the county.
#'   \item lq_asian: Location quotient of Asian population relative to the county.
#'   \item lq_black: Location quotient of Black population relative to the county.
#'   \item lq_hispanic: Location quotient of Hispanic population relative to the county.
#'   \item oppcat: Opportunity designation.
#'   \item pov_seg_cat: High Poverty and Segregated designation.  Defined as least 30 percent of the population falling under the federal poverty line and Location Quotient of higher than 1.25 for Black, Hispanic, Asian, or all people of color in comparison to the county.
#'   \item trct_raceeth_chng1322: Percentage point change in tract's non-Hispanic white population between 2013 and 2022
#'   \item raceeth_half0022: Regionwide 50% threshold for non-Hispanic white tract-level percentage point increase (2000-2022)
#'   \item raceeth_half1322: Regionwide 50% threshold for non-Hispanic white tract-level percentage point increase (2013-2022)
#'   \item raceeth_quarter1322: Regionwide 75% threshold for non-Hispanic white tract-level percentage point increase (2013-2022)
#'   \item baseline_raceinc0022:	Binary flag if tract was a LMI and BIPOC neighborhood in 2000
#'   \item baseline_race0022:	Binary flag if tract was a BIPOC neighborhood in 2000
#'   \item baseline_race1322:	Binary flag if tract was a BIPOC neighborhood in 2013
#'   \item change_race0022:	Binary flag if a LMI and BIPOC neighborhood in 2000 experienced an increase in the non-Hispanic white population above the regionwide threshold (2000-2022)
#'   \item change_race1322:	Binary flag if a BIPOC neighborhood in 2013 experienced an increase in the non-Hispanic white population above the 50% regionwide threshold (2013-2022)
#'   \item change_race1321_75pct:	Binary flag if a BIPOC neighborhood in 2013 experienced an increase in the non-Hispanic white population above the 75% regionwide threshold (2013-2022)
#'   \item exclusion_flag	Binary flag if that tract has unreliable data or meets other exclusion parameters (e.g., institutionalized population)
#'   \item trct_medinc_pctchng_0022:	Percent change in tract's household median income between 2000 and 2022
#'   \item trct_medinc_pctchng_1322:	Percent change in tract's household median income between 2013 and 2022
#'   \item medinc_half0022:	Regionwide 50% threshold for tract-level median income percentage increase (2000-2022)
#'   \item medinc_half1322:	Regionwide 50% threshold for tract-level median income percentage increase (2013-2022)
#'   \item medinc_quarter1322:	Regionwide 75% threshold for tract-level median income percentage increase (2013-2022)
#'   \item baseline_income1322:	Binary flag if tract was a LMI neighborhood in 2013 (used in rising rents and home value/income gap)
#'   \item change_income0022:	Binary flag if a LMI and BIPOC neighborhood in 2000 experienced an increase in median household income above the regionwide threshold (2000-2022)
#'   \item change_income1322:	Binary flag if a LMI and BIPOC neighborhood in 2013 experienced an increase in median household income above the regionwide threshold (2013-2022)
#'   \item change_income1321_75pct:	Binary flag if a LMI neighborhood in 2013 experienced an increase in median household income above the 75% regionwide threshold (2013-2022)
#'   \item trct_pctchng_medrent1322:	% change in tract's median rent 2013-2022
#'   \item rent_half1322:	Regionwide top 50% threshold for % change in median rent (2013-2022)
#'   \item medrent_disp1322:	Binary flag if an LMI neighborhood in 2013 experienced rent increases above the regionwide threshold (2013-2022)
#'   \item income_percentile:	Percentile of tract household median income relative to the region (2022)
#'   \item hval_percentile: Percentile of tract median home value relative to the region (2022)
#'   \item pct_gap: Difference between the tract's household median income percentile (2022) and its median home value percentile (2022)
#'   \item hvalinc_gap: Binary flag if the difference between the tract's household median income percentile (2022) and its median home value percentile (2022) is greater than 25 percentage points
#'   \item pathway1a: Pathway 1A - Binary flag if a tract meets both racial/ethnic change and economic change between 2000-2022 (50% threshold)
#'   \item halfmile_buffer: Binary flag if a tract's population-weighted centroid is within 1/2-mile of the population-weighted centroid of a tract that meets Pathway 1a
#'   \item pathway2: Pathway 2 - Binary flag if a tract that is within 1/2-mile (population-weighted) of a tract that meets Pathway 1A and has rising rents and/or a home value/income gap, and experienced either racial/ethnic or economic change between 2013-2022
#'   \item pathway1b: Pathway 1B - Binary flag if a tract meets both racial/ethnic change and economic change between 2013-2022 (75% threshold)
#'   \item nbrhood_chng: Binary flag if a tract meets the definition of neighborhood change (Pathway 1A, Pathway 1B, or Pathway 2)
#'
#' }
#'
#' @docType data
#' @keywords datasets
#' @usage ?data_dict_2025
#'
#' @name data_dict_2025
NULL
