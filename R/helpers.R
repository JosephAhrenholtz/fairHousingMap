#' Reader functions for zipped files
#'
#' Currently, most data is read from a zipped datafile to save space. This is in flux, as I
#'   decide whether to host all data in zip files or uncompressed. Will be re-written in a
#'   future version. Extracted files are placed in a temp director and deleted
#'
#' @inheritParams final_TCAC
#' @param name File within zip directory.
#' @param pattern Name of the shapefile
#' @param filename Name of file being written
#' @param type Can be 'excel', 'csv', 'tsv', or 'table.'
#' @param folder Name of the subfolder for writing data. Always resides in data directory unless 'year' is NULL.
#' @param removeRapid Remove "rapidly changing" category and combine with the vanilla moderate resource
#' category.
#' @param explicitNA If not null, retitle NA factor to the provided string.
#' @param ... Arguments to pass through to read.xlsx, read_csv, read_tsv, or read_txt from openxlsx and readr.
#'
#' @details \describe{
#'  \item{`delete_temp`}{Deletes all files in the temporary directory. }
#'  \item{`read_zip`}{Deploys readr and openxlsx functions on zipped files}
#'  \item{`filepaths`}{Creates filenames for variables used within package for easy reading
#'    across years. Returns variables for reading when testing package functions; if used
#'    externally, they will be used in the global environment}
#'  \item{`censusVar`} {Wrapper for `paste0` that simplifies calling the variable with the correct year.}
#'  \item{`sumna`, `meanna`} {wrappers for the respective base functions, setting na.rm to True}
#'  \item{`factor_oppcat`} {Automatically factors tcac scores.}
#' }
#'
#' The data in this package is stored in two folder: data and output. The data folder
#'  contains zip folders for each year of data - for example, 2015 LODES data is stored
#'  in the 2015 zip file. Data spanning 2 years, e.g. the 2017-2018 school files, will be
#'  kept in the latter year's zipfile, e.g. 2018. The output directory contains files
#'  generated with the R package, for each year in which TCAC opportunity maps are produced.
#'
#' In the filepaths function, all data is read in for the 2019 year. Data that is updated
#'  in 2020 is written or overwritten if that year is set. Data for 2021 will overwrite
#'  both 2019 and 2020 data, and so on. This is to make it clear which files are updated in
#'  which years, without the possibility of accidentally duplicating a file between years.
#'
#' @name helper_functions
NULL


#' capture the current year for default function years
current_year = 2024

#' @rdname helper_functions
read_zip <- function(name, year, type = 'csv', ...) {
  # syear <- year %% 100
  # import all filenames
  filepaths(year = year)
  # get the path and year for the specified 'name'
  path <- get0(name)
  data_year <- path[1]
  name <- path[2]
  # Execute

  if(type %in% c('excel', 'xls','xlsx')){
    # Excel can't be read from zipped file, so I unzip and then delete the file instead.
    #extract file path, and rewrite the path back to the temp folder it will be extracted into.
    tempFolder = tempdir()
    output <- paste0(tempFolder, "/",name)
    unzip(paste0('data-raw/',data_year,'.zip'), name,
          exdir = tempFolder)
    #read the excel file
    df <- openxlsx::read.xlsx(output, ...)
    #delete the file after reading into R
    file.remove(output)
  }
  else{ #readr functions
    #get the connection inside the zip folder
    connection <- unz(description = paste0("data-raw/", data_year,".zip"), filename = name)

    if(type == 'csv') df <- readr::read_csv(connection, ...)
    else if(type == 'tsv') df <- readr::read_tsv(connection,  ...)
    else if(type %in% c('table', 'txt')) df <- readr::read_table(connection, ...)
    else stop('Error: Invalid "type" selected.')
  }
  df
}
#' @rdname helper_functions
#' censusVar <- function(varName, year = year){
#'   acs_year = year - 3
#'   paste0(varName, '_', acs_year)
#' }
#' #' @rdname helper_functions
#' sumna <- function(...){
#'   sum(..., na.rm = T)
#' }
#' #' @rdname helper_functions
#' meanna <- function(...){
#'   mean(..., na.rm = T)
#' }
#' #' @rdname helper_functions
#' delete_temp <- function(year = current_year){
#'   do.call(file.remove, list(list.files(tempdir(), full.names = TRUE)))
#' }
#' #' @rdname helper_functions
#' verify_geo <- function(x){
#'   if(!x %in% c('tract', 'bg')) stop("geo must be either 'tract' or 'bg' (for block groups)")
#' }
#' #' @rdname helper_functions
#' factor_oppcat <- function(oppcat, removeRapid=FALSE, explicitNA=FALSE){
#'
#'   tcac_levels <- c("High Segregation & Poverty","Low Resource","Moderate Resource","Moderate Resource (Rapidly Changing)","High Resource","Highest Resource")
#'
#'   oppcat_fac <- factor(oppcat, levels = tcac_levels)
#'   # remove missing if set
#'   if(removeRapid==T) oppcat_fac <- forcats::fct_collapse(oppcat_fac,
#'                                                          "Moderate Resource"=c("Moderate Resource","Moderate Resource (Rapidly Changing)"))
#'
#'   # create NA category
#'   if(!isFALSE(explicitNA)){
#'     oppcat_fac <- forcats::fct_explicit_na(oppcat_fac, explicitNA)
#'     oppcat_fac <- forcats::fct_relevel(oppcat_fac, explicitNA, after=0)
#'   }
#'   return(oppcat_fac)
#' }
#'
#'
#' # Reduce TCAC data frame to indicators
#' tcac_indics <- function(df){
#'
#'   indics <- c('fips', 'fips_bg', 'region', 'county_name', 'above_200_pov', 'pct_bachelors_plus', 'pct_employed', 'jobs_lowed', 'home_value',
#'               'toxrelease','pm25','dieselpm','drinkingwater','traffic', 'ozone','lead','pesticides','cleanup_sites','groundwater','hazard_waste','impaired_water', 'solid_waste',
#'               'read_prof', 'math_prof', 'grad_rate','pct_not_frpm','econ_domain', 'env_health_domain','ed_domain', 'oppcat')
#'
#'   df <- df %>% select(all_of(indics))
#' }
