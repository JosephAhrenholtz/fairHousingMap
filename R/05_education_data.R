#' Imports education data and saves the relevant variables to the intermediate directory
#'
#' @details Reading data is weighted by total enrollment of schools that
#'   return 4th-grade test scores. FRPM data is limited to and weighted
#'   by enrollment in schools that serve elementary school students. Grad
#'   data is weighted by high school enrollment.
#'
#'
#' @param year designates the appropriate filepaths
#' @param write will write a new file to the intermediate directory
#' @param read rill read an existing file from the intermediate directory
#'
#' @return a data frame
#' @export
#'
#' @examples
#' graduation_rates() # reads an existing intermediate file at the default year
#'
#'
#' @import dplyr
#'
#'
#' @source School data: https://www.cde.ca.gov/ds/si/ds/pubschls.asp
#' @source Test data: https://caaspp-elpac.ets.org/caaspp/ResearchFileListSB?
#' @source FRPM: http://www.cde.ca.gov/ds/sd/sd/filessp.asp
#' @source Cohort Grad data: https://www.cde.ca.gov/ds/ad/filesacgr.asp
#'
#'
#' @inheritParams final_TCAC
#'
read_educ_pov <- function(year = current_year) {
  filepaths(year = year)

  newed <- read_zip(frpm, year = year, type = 'excel', sheet = 2, startRow=2)

  newed <- tidyr::unite(newed, CDSCode, County.Code, District.Code, School.Code,
    sep = '', remove = FALSE)

  #only including traditional schools
  newed <- dplyr::filter(newed, Educational.Option.Type == 'Traditional')

  districts <- read_zip(pubschls, year = year, type = 'tsv')

  #join frpm with school data
  districts <- dplyr::left_join(newed, districts, by = 'CDSCode')

  #exclude virtual and closed schools
  districts <- dplyr::filter(districts, StatusType == 'Active', Virtual != "F")
  #create street address
  districts <- tidyr::unite(districts, 'address', Street, City, State, Zip, sep = ', ')
  #keep only schools that serve 3rd and 4th graders
  districts <- dplyr::filter(districts, SOC == 60 | SOC == 61 | SOC == 65)
  #reformat longitude/latitude to numbers
  districts <- dplyr::mutate(districts, lon = as.numeric(Longitude),
                             lat = as.numeric(Latitude))
  #select only relevant data
  educ_districts <- dplyr::select(districts, CDSCode, enrollment = `Enrollment.(K-12)`,
                                  pct_frpm = `Percent.(%).Eligible.FRPM.(K-12)`,
                                  StatusType,address, SOC, SOCType,Virtual,lat, lon,
                                  NCESDist, pct_freemeals = `Percent.(%).Eligible.Free.(K-12)`,
                                  county_name = `County.Name`)

  #english/math proficiency
  english_math <- read_zip(ED_english_math, year,
                                  col_types = readr::cols(), progress = FALSE, type = 'delim')
  english_math <- tidyr::unite(english_math, CDSCode, `County Code`, `District Code`, `School Code`,
                               sep = '', remove = FALSE)
  #subset 4th grade english & math tests
  english_math <- subset(english_math, Grade == 4 & (`Test ID` == 1 | `Test ID` == 2), select =
                           c(`Percentage Standard Met and Above`, `Test ID`, CDSCode))
  english_math <- tidyr::spread(english_math, `Test ID`, `Percentage Standard Met and Above`)

  english_math <- suppressWarnings(
    dplyr::transmute(english_math, read_prof = as.numeric(`1`)/100,
      math_prof = as.numeric(`2`)/100, CDSCode = CDSCode))

  #merge with frpm
  educ_districts <- dplyr::left_join(educ_districts, english_math, by = 'CDSCode')

  educ_districts
}



#' Imports graduation data and saves the relevant variables
#'
#' Graudation rates are calculated as the enrollment-weighted average
#'   of the three schools closest to the tract centroid.
#'
#'
#' @details 2019 version allocates schools to school districts, but 2020 code
#'   simply reads in lat/lon data, as it they are distributed by allocating
#'   the three closest schools to the block group, as with primary schoool
#'   data.
#'
#' @note In 2020+, DASS schools are omitted. Charter schools are included.
#'
#' @source https://www.cde.ca.gov/ds/sd/sd/filescohort.asp
#'
#' @export
graduation_rates <- function(year = current_year, write = FALSE,
  read = !write){
  if(read == T){
    filename = paste0("data/intermediate/", year,"/graduation_rates.csv")
    grad_rate <- readr::read_csv(filename, col_types = cols())
  } else{

    filepaths(year = year)

    #nces crosswalk
    xwalk_nces <- read_zip(pubschls, year = year, type = 'tsv', col_type = cols())

    #cohort graduation rates
    graduation <- read_zip(name = ED_cohort, type = 'tsv', year = year, guess_max = 10000,
      col_types = readr::cols(), na = c("", "NA", '*'))
    #only include totals from districts COEs, and direct funded charters
    graduation <- dplyr::filter(graduation, AggregateLevel == 'S' & CharterSchool == 'All' &
        DASS == 'No' & ReportingCategory == 'TA') %>%
      dplyr::mutate(CDS = paste0(CountyCode, DistrictCode, SchoolCode),
        enrollment = CohortStudents, grad_rate = `Regular HS Diploma Graduates (Rate)`,
        hs_drop_rate = `Dropout (Rate)`,
        countyid = paste0('06',stringr::str_pad(CountyCode,3,'left','0'))) %>%
      na.omit()

    grad_rate <- dplyr::inner_join(xwalk_nces, graduation, by = c('CDSCode' = 'CDS'))

    #select only active districts
    grad_rate <- subset(grad_rate, ReportingCategory = 'TA', StatusType == 'Active')# (SOC == '65' | SOC == '66' | SOC == '67' | SOC == '68' | SOC == '69'),

    grad_rate <- dplyr::transmute(grad_rate, CDSCode, grad_rate = grad_rate/100,
      county_name = County, hs_drop_rate = hs_drop_rate/100,
      enrollment, countyid, lon = Longitude,lat = Latitude)

    delete_temp()
  }
if(write == TRUE){
  filename = paste0("data/intermediate/", year,"/graduation_rates.csv")
  readr::write_csv(grad_rate, filename)
}
else grad_rate
}


