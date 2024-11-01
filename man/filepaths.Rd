% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filepaths.R
\name{filepaths}
\alias{filepaths}
\title{Shortcuts to zipped files in the data-raw directory. See `?read_zip`}
\usage{
filepaths(year = current_year)
}
\arguments{
\item{year}{designates the map year's filepaths}
}
\value{
filepaths to raw data are loaded into environment
}
\description{
Shortcuts to zipped files in the data-raw directory. See `?read_zip`
}
\note{
Year 2023 filepaths are used to process 2023 OM education data at 2020 geographies, which is required
for implementation of 3-year rolling education averages.
}
\examples{
filepaths(year = 2024)

}
