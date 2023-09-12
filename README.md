README
================
2023-08-08

## Fair Housing Map

In process documentation for CTCAC/HCD Opportunity and Neighborhood
Change Map package, temporarily called the “Fair Housing Map” until
directed further by CTCAC and HCD. Please see the [draft
methodology](https://docs.google.com/document/d/1_wk_B9l0ZVSAr6bSoVROc7xuwTteyJxc/edit?usp=sharing&ouid=106736372141423442162&rtpof=true&sd=true)
for details on the mapping approach.

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

- Develop testthat test suite (in progress as of 9/4/2023).
- The legacy packages maptools, rgdal, and rgeos, underpinning the sp
  package, which is a dependency, will retire in October 2023. Replace
  functions that depend on sp with sf package equivalents.

### Running the package

Generating the final opportunity designations requires calling a
sequence of functions in a specific order. Only functions which write
intermediate files are necessary to generate a new final output.

#### **01_census_geography.R**

Contains `read_tract_centers()`, `read_block_group_centers()`, and
`read_block_centers()`, functions for reading population weighted
centroids. The centroid-reading functions are called within other
subsequent scripts, and do not themselves produce any intermediate
files.

#### **02_create_rural_shapefile.R**

Contains `shape_rural()` , which imports the USDA shapefile for areas
ineligible for rural development housing programs, then shrinks to only
California data and subtracts them from the complete California
shapefile to get a rural areas shapefile.
`shape_rural(write = T)`generates the first necessary intermediate file,
rural_shapefile.shp, which must exist in the intermediate directory to
proceed.

``` r
shape_rural(write = F) # write = T will generate a new file
```

``` r
mapview::mapview(shape_rural(write = F)) # to map the shapefile
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### **03_create_region_designation.R**

Contains `create_regions()`, which evaluates which regions that each
county belongs to, then uses `rural_overlay()` to pinpoint rural tracts.
`rural_overlay()` merges block centers with the rural_shapefile, and
classifies the population of any block with its centroid inside the
rural shapefile as rural. It then collapses to tract level, and any
tract with over 50 percent population as rural is classified as “Rural
Areas.” `create_regions(write = T)` generates the required intermediate
file, tract_county_region.csv.

``` r
create_regions(read = T) # reads the existing file, set to False to generate new
```

#### 04_census_data.R

Contains `all_census_data`() which downloads all relevant decennial and
ACS data from Census API using tidycensus, then derives percentages and
calculates margins of error for derived variables. Both
`all_census_data(geo = 'tract', write = T)` and
`all_census_data(geo = 'bg', write = T)` are necessary to generate
intermediate tract- and block group-level data.

``` r
all_census_data(geo = 'tract', read = T, write = F) # for tracts
```

``` r
all_census_data(geo = 'bg', read = T, write = F) # for block groups
```

#### 05_education_data.R

Contains `read_educ_pov()` which imports relevant math/reading scores
and share of students receiving free or reduced priced meals (frpm).
`read_educ_pov()` is called in subsequent scripts and does not produce
any intermediate files. Reading/math data is weighted by total
enrollment of schools that return 4th-grade test scores. FRPM data is
limited to and weighted by enrollment in schools that serve elementary
school students. `graduation_rates()` imports and writes cohort-weighted
high school graduate rates to the intermediate directory.

``` r
graduation_rates(write = F) # set to True to generate new data
```

#### 06_education_distance.R

Contains `school_distances()` which finds school distance to tract or
block group centroids, and averages the reading and math scores, frpm,
and graduation rates of the three nearest schools.

``` r
school_distances(read = T, write = F) # set read to False, write to True to generate new data
```

#### 07_enviroscreen.R

Contains `xwalk_ces()` which imports CalEnviroScreen 4.0 site-based
measurements (cleanup sites, hazardous waste, groundwater threats, solid
waste) from the final 2023 TCAC file, creates a binary score for tracts
in the bottom 5% of site-based hazards within regions, then crosswalks
to 2020 tracts using an overlay method of \>= 5% of intersecting land
area.

``` r
xwalk_ces(read = T, write = F)
```

#### 08_neighborhood_change.R

Contains `read_neighborhood_change()` which imports neighborhood change
data generated by CHPC. CHPC’s scripts are located in
R/neighborhood_change/ and identify tracts that have experienced both
long-term (since 2000) and recent (since 2013) racial/ethnic and
economic change. `read_neighborhood_change()` is called in the
subsequent script.

#### 09_final_data.R

Contains `final_opp()` which loads and combines all intermediate files
of economic, education, and environmental indicators, creates the final
opportunity scores and designations. `read_neighborhood_change()` is
called and joined to the final data frame.

``` r
final_opp()
```

``` r
mapview::mapview(final_opp(as_geo = T), zcol = 'oppcat') # to map the output
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
