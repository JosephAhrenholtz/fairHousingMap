README
================
2023-08-08

## Fair Housing Map

In process documentation for CTCAC/HCD Opportunity and Neighborhood
Change Map package, temporarily called the “Fair Housing Map” until
further directed by CTCAC and HCD.

### Package Manual

\[\]

### Installation

1.  Clone repository

2.  Download data-raw directory and extract into root directory:
    <https://berkeley.app.box.com/folder/224592265510>

3.  Open package using “fairHousingMap.Rproj” file

4.  Load package and install dependencies:

``` r
devtools::install_dev_deps()
```

``` r
devtools::load_all()
```

### Census API

1.  Register for Census API key
    [here](https://api.census.gov/data/key_signup.html).

2.  Install the key in the tidycensus package. See
    `?tidycensus::census_api_key()` for details

3.  Build directory paths not included in repository:
    ‘data/intermediate/2024/’ and ‘output/2024/’

### Directory Structure

- *data* - .Rda files of census polygon geographies.

  - *intermediate* - intermediate data files.

- *analysis* - .Rmd files of internal reports, exploratory analysis,
  etc.

- *output* - Published datasets

- *README* - project root documentation

- *.gitignore* - instructions for git to ignore files

- *R* - R package functions, executes all code that goes into published
  tables and maps

- *map* - javascript webmaps

- *tests* - testthat tests to ensure package is working as expected

### TODO

- Develop testthat test suite.
- The legacy packages maptools, rgdal, and rgeos, underpinning the sp
  package, which is a dependency, will retire in October 2023. Replace
  functions that depend on sp with sf package equivalents.
