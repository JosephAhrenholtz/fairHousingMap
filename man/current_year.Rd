% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\docType{data}
\name{current_year}
\alias{current_year}
\title{Includes miscellaneous helpers, including setting the default year, reader functions for zipped files,
and data visualization themes.}
\format{
An object of class \code{numeric} of length 1.
}
\usage{
current_year
}
\arguments{
\item{name}{file within zip directory}

\item{type}{Can be 'excel', 'csv', 'tsv', or 'table'}
}
\value{
the raw data file
}
\description{
Includes miscellaneous helpers, including setting the default year, reader functions for zipped files,
and data visualization themes.
}
\examples{
read_zip(acs_variables, year = 2024, type = 'csv')


}
\keyword{datasets}
