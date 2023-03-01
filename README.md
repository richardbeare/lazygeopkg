
# lazygeopkg

<!-- badges: start -->
<!-- badges: end -->

The goal of lazygeopkg is to provide a lazy, tidyverse style interface to
geopackage format geospatial data. This is identical in concept to `lazysf`,
differing in that the SQL engine is provided by DBI, rather than GDAL.

This offers some capabilities, including filtering joins, that aren't possible
via GDAL.

## Installation

You can install the development version of lazygeopkg like so:

``` r
devtools::install_github("richardbeare/lazygeopkg")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lazygeopkg)

ncf <- system.file("gpkg/nc.gpkg", package="sf")

# create a connection object
nc <- connectGPKG(ncf)
nc

# create a lazy table
tbl(nc, "nc.gpkg")

# create a local tibble with interesting names
nicenames <- tibble::tibble(NAME=c("Northampton", "Stokes"))

# load just those rows
tbl(nc, "nc.gpkg") %>% 
semi_join(nicenames, copy=TRUE, by = "NAME") %>% 
collect()

```


# Setup notes

Commands used when creating the package

```r
usethis::create_package("lazygeopkg")
usethis::use_package("sf")
usethis::use_package("DBI")
usethis::use_package("dplyr")
usethis::use_package("RSQLite")
usethis::use_package("methods")
usethis::use_package("tibble")
usethis::use_package("rlang")
usethis::use_readme_md()
```
