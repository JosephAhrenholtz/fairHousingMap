README
================
Joseph Ahrenholtz
2024-08-19

## fairHousingMap

### Statement of Purpose

The
[`fairHousingMap`](https://github.com/JosephAhrenholtz/fairHousingMap "Go to definition")
package is designed to facilitate the creation of the Affirmatively
Furthering Fair Housing (AFFH) Mapping Tool, used by the California Tax
Credit Allocation Committee (CTCAC) and the California Department of
Housing and Community Development (HCD) to guide the siting of
incentive-eligible affordable housing using a federal tax credit. This
package provides tools to process various datasets from the census, CA
department of education, and CalEnviroScreen, among others, and
classifies geographies in terms of opportunity, gentrification, and
segregated areas of poverty. The package is designed to streamline the
workflow for the annual update process. The latest release of the tool
can be found
[here](https://belonging.berkeley.edu/2024-hcd-affh-mapping-tool).

### Installation

1.  Clone GitHub repository and download data-raw directory from
    [Drive](https://drive.google.com/drive/folders/1p0_igbmI02SInUi22O5naaki1f1Q_YKl?usp=drive_link)
    and extract into root directory.

2.  Open package using “fairHousingMap.Rproj” file

3.  Load package and install dependencies:

``` r
devtools::install_dev_deps()
```

``` r
devtools::load_all()
```

### Final Dataset

The most recent dataset can be loaded by calling `data(final_2025)`. All
variables are documented in the the data dictionary which can be loaded
using `?data_dict_2025`.

### Census API

1.  Register for Census API key
    [here](https://api.census.gov/data/key_signup.html).

2.  Install the key in the tidycensus package. See
    `?tidycensus::census_api_key()` for details

### Directory Structure

- *data* - .Rda files of census polygon geographies, as well published
  datasets for convenient loading.

  - *intermediate* - intermediate data files.

- *analysis* - .Rmd files of internal reports, exploratory analysis,
  etc.

- *output* - final map data, public summary and shapefiles files

- *README* - project root documentation

- *.gitignore* - instructions for git to ignore files

- *R* - R package functions, executes all code that goes into published
  tables and maps

- products

  - map - javascript mapping tool

  - charts - LIHTC distribution

- *tests* - testthat unit tests to ensure package is working as expected

### TO DO

- The legacy packages maptools, rgdal, and rgeos, underpinning the sp
  package, which is a dependency, are set to retire. Replace functions
  that depend on sp with sf package equivalents.

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
shape_rural(write = T) # write = T will generate a new file
```

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
create_regions(write = T) # reads the existing file, set to False to generate new
```

#### 04_census_data.R

Contains `all_census_data`() which downloads all relevant decennial and
ACS data from Census API using tidycensus, then derives percentages and
calculates margins of error for derived variables. Both
`all_census_data(geo = 'tract', write = T)` and
`all_census_data(geo = 'bg', write = T)` are necessary to generate
intermediate tract- and block group-level data.

``` r
all_census_data(geo = 'tract', read = T, write = T) # for tracts
```

``` r
all_census_data(geo = 'bg', read = T, write = T) # for block groups
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
graduation_rates(write = T) # set to True to generate new data
```

#### 06_education_distance.R

Contains `school_distances()` which finds school distance to tract or
block group centroids, and averages the reading and math scores, frpm,
and graduation rates of the three nearest schools.

``` r
school_distances(write = T, geo='tract')
school_distances(write = T, geo='bg')
```

#### 07_enviroscreen.R

Contains `xwalk_ces()` which imports CalEnviroScreen 4.0 site-based
measurements (cleanup sites, hazardous waste, groundwater threats, solid
waste) from the final 2023 processed opportunity data, creates a binary
score for tracts in the bottom 5% of site-based hazards within regions,
then crosswalks to 2020 tracts using an overlay method of \>= 5% of
intersecting land area. This is a temporary stop-gap method until OEHHA
releases CalEnviroScreen at 2020 geographies.

``` r
xwalk_ces(write = T)
```

#### 08_neighborhood_change.R

Contains `read_neighborhood_change()` which imports neighborhood change
data. Neighborhood change code was developed by Matt Alvarez-Nissen with
California Housing Partnership. Neighborhood change scripts and output
are located in R/neighborhood_change/. This directory is seperated from
the rest of the package and does not contain documented functions. The
scripts identify tracts that have experienced both long-term (since
2000) and recent (since 2013) racial/ethnic and economic change, as well
as markers of disproportionate housing needs.
`read_neighborhood_change()` is called in the final script.

#### 09_tribal_land.R

Contains `tribal_overlap()`which imports tribal lands under the control
of federally-recognized tribes, computes intersection with Census
tracts, and flags any tract where at least 25 percent of the geography’s
land area is within federally-recognized tribal lands. In final_data.R,
High-Poverty & Segregated is not assessed in tracts where the tribal
land flag is raised.

#### 10_final_data.R

Contains `final_opp()` which loads and combines all intermediate files
of economic, education, and environmental indicators, creates the final
opportunity scores and designations. `read_neighborhood_change()` is
called and joined to the final data frame.

``` r
final_opp(write = T)
```

#### 11_final_public.R

Loads final opportunity and neighborhood change data and creates excel
workbook summary files, shapefiles, and data dictionaries to be
published on CTCAC and HCD websites.

``` r
final_opp_public(write = T, change = T)
```
