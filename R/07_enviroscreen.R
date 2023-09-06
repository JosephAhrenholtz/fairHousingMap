#' Imports CalEnviroScreen 4.0 data and site-based measurements (point sources of pollution)
#'
#'
#'

read_ces <- function(year = current_year, write = FALSE, read = !write) {
  filepaths(year = year)


  filename = paste0("data/intermediate/", year,"/ces_indicators.csv")
  if(read == T) {
    return(readr::read_csv(filename, col_types = readr::cols()))
  }
  #read_zip exports the requested spreadsheet to 'xlsx_path
  ces <- read_zip(ces4results, year = year, type = 'excel', sheet = 1)

  ces <- dplyr::transmute(ces, fips = paste0(0,Census.Tract),
    cleanup_sites = Cleanup.Sites, groundwater = Groundwater.Threats,
    hazard_waste = Haz..Waste, solid_waste = Solid.Waste)

  ces <- dplyr::select(ces, fips, cleanup_sites, hazard_waste, solid_waste, groundwater)


  if(write == TRUE){
    readr::write_csv(ces, filename)
  }
  else ces
}
