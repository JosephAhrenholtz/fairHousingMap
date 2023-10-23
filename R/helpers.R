#' Reader functions for zipped files
#'
#'
#' @param name file within zip directory
#' @param type Can be 'excel', 'csv', 'tsv', or 'table'
#'
#'
#' @returns the raw data file
#'
#'
#' @examples
#' read_zip(acs_variables, year = 2024, type = 'csv')
#'
#'
#' @importFrom readr read_csv cols
#' @importFrom openxlsx read.xlsx
#'

# capture the current year for default function years
#current_year = 2024
current_year = as.numeric(strsplit(as.character(Sys.Date()),"-")[[1]][1])

#' @export
read_zip <- function(name, year, type = 'csv', ...) {

  # import all filenames
  filepaths(year = year)
  # get the path and year for the specified 'name'
  data_year <- name[1]
  file_name <- name[2]


  # Execute
  if(type %in% c('excel', 'xls','xlsx')){


    # Excel can't be read from zipped file, so unzip and then delete the file.
    #extract file path, and rewrite the path back to the temp folder it will be extracted into.
    tempFolder = tempdir()
    output <- paste0(tempFolder, "\\", file_name)
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
    else if(type == 'delim') df <- readr::read_delim(connection, delim = '^',  ...)
    else if(type %in% c('table', 'txt')) df <- readr::read_table(connection, ...)
    else stop('Error: Invalid "type" selected.')
  }
  df
}

delete_temp <- function(year = current_year){
  do.call(file.remove, list(list.files(tempdir(), full.names = TRUE)))
}

verify_geo <- function(x){
  if(!x %in% c('tract', 'bg')) stop("geo must be either 'tract' or 'bg' (for block groups)")
}

