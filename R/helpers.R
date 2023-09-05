#' Reader functions for zipped files
#'
#'
#' @importFrom readr read_csv cols
#' @importFrom openxlsx read.xlsx
#'
#' @param name File within zip directory.
#' @param pattern Name of the shapefile
#' @param filename Name of file being written
#' @param type Can be 'excel', 'csv', 'tsv', or 'table.'
#' @param folder Name of the subfolder for writing data. Always resides in data directory unless 'year' is NULL.
#' @param explicitNA If not null, retitle NA factor to the provided string.
#' @param ... Arguments to pass through to read.xlsx, read_csv, read_tsv, or read_txt from openxlsx and readr.
#'
#' @details \describe{
#'  \item{`delete_temp`}{Deletes all files in the temporary directory. }
#'  \item{`read_zip`}{Deploys readr and openxlsx functions on zipped files}
#'  \item{`filepaths`}{Creates filenames for variables used within package for easy reading
#'    across years. Returns variables for reading when testing package functions; if used
#'    externally, they will be used in the global environment}
#' }
#'
#' @name helper_functions


#' capture the current year for default function years
current_year = 2024

#' @rdname helper_functions
read_zip <- function(name, year, type = 'csv', ...) {
  # syear <- year %% 100
  # import all filenames
  filepaths(year = year)
  # get the path and year for the specified 'name'
  #path <- name
  data_year <- name[1]
  file_name <- name[2]
  # Execute

  if(type %in% c('excel', 'xls','xlsx')){
    # Excel can't be read from zipped file, so I unzip and then delete the file instead.
    #extract file path, and rewrite the path back to the temp folder it will be extracted into.
    tempFolder = tempdir()
    output <- paste0(tempFolder, "/",file_name)
    unzip(paste0('data-raw/',data_year,'.zip'), file_name,
          exdir = tempFolder)
    #read the excel file
    df <- openxlsx::read.xlsx(output, ...)
    #delete the file after reading into R
    file.remove(output)
  }
  else{ #readr functions
    #get the connection inside the zip folder
    connection <- unz(description = paste0("data-raw/", data_year,".zip"), filename = file_name)

    if(type == 'csv') df <- readr::read_csv(connection, ...)
    else if(type == 'tsv') df <- readr::read_tsv(connection,  ...)
    # reading and math switched to caret delimited in 2023
    else if(type == 'delim') df <- readr::read_delim(connection, delim = '^',  ...)
    else if(type %in% c('table', 'txt')) df <- readr::read_table(connection, ...)
    else stop('Error: Invalid "type" selected.')
  }
  df
}

#' #' @rdname helper_functions
delete_temp <- function(year = current_year){
  do.call(file.remove, list(list.files(tempdir(), full.names = TRUE)))
}
#' @rdname helper_functions
verify_geo <- function(x){
  if(!x %in% c('tract', 'bg')) stop("geo must be either 'tract' or 'bg' (for block groups)")
}

