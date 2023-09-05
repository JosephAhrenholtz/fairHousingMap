README
================
2023-08-08

## Fair Housing Map

CTCAC/HCD Opportunity and Neighborhood Change Map.

### Package Manual

\[\]

### Installation

1.  Clone repository

2.  Download data-raw directory and extract into root directory:
    <https://berkeley.app.box.com/folder/224592265510>

3.  Open package using “fairHousingMap.Rproj” file

4.  Load package and install dependencies:

<!-- -->

    ## ℹ Loading fairHousingMap
    ## The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
    ## which was just loaded, will retire in October 2023.
    ## Please refer to R-spatial evolution reports for details, especially
    ## https://r-spatial.org/r/2023/05/15/evolution4.html.
    ## It may be desirable to make the sf package available;
    ## package maintainers should consider adding sf to Suggests:.
    ## The sp package is now running under evolution status 2
    ##      (status 2 uses the sf package in place of rgdal)

    ## Warning: package 'testthat' was built under R version 4.2.3

### Census API

1.  Register for Census API key
    [here](https://api.census.gov/data/key_signup.html).

2.  Install the key in the tidycensus package. See
    `?tidycensus::census_api_key()` for details

3.  Build directory paths not included in repository:

<!-- -->

    ## Warning in dir.create(paste0("data/intermediate/", x), recursive = T):
    ## 'data\intermediate\2024' already exists

    ## Warning in dir.create(paste0("output/", x), recursive = T): 'output\2024'
    ## already exists

    ## Warning in dir.create("output/alternative_scenarios"):
    ## 'output\alternative_scenarios' already exists

    ## [[1]]
    ## [1] FALSE

### Directory Structure

- *data* - .Rda files of census polygon geographies.

  - *intermediate* - intermediate data files.

- *analysis* - .Rmd files of internal reports, exploratory analysis,
  etc.

- *output* - Published datasets

- *README* - project root documentation

- *.gitignore* - instructions for git to ignore files.

- *R* - R package functions, executes all code that goes into published
  tables and maps.

- *map* - contains the webmaps for each year since 2019. Produced in
  javascript after 2019.

- *tests* - testthat tests to ensure package is working as expected

### TODO

- The legacy packages maptools, rgdal, and rgeos, underpinning the sp
  package, which was just loaded, will retire in October 2023. Replace
  functions that depend on sp with sf package equivalents
- Develop testthat test suite
