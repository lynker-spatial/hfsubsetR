
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hfsubsetR <a href="https://github.com/lynker-spatial/hfsubsetR"><img src="man/figures/logo.png" align="right" width="25%"/></a>

<!-- badges: start -->

[![R CMD
Check](https://github.com/lynker-spatial/hfsubsetR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lynker-spatial/hfsubsetR/actions/workflows/R-CMD-check.yaml)
[![Test
coverage](https://github.com/lynker-spatial/hfsubsetR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lynker-spatial/hfsubsetR/actions/workflows/test-coverage.yaml)
[![Dependencies](https://img.shields.io/badge/dependencies-8/71-orange?style=flat)](#)
[![License: GPL (\>=
3)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%203%29-blue.svg)](https://choosealicense.com/licenses/gpl-3.0/)
<!-- badges: end -->

The goal of `hfsubsetR` is to extract hydrofabric subsets from cloud or
local archives built in `lynker-spatial`.

## Installation

You can install the development version of `hfsubsetR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("lynker-spatial/hfsubsetR")
```

``` r
library(hfsubsetR)
```

## Basic Use

### From local file

We can download a version (default = ‘2.2’) hydrofabric (default =
‘nextgen’) for any domain (default = ‘conus’) using `get_hydrofabric`.

``` r
gpkg <- './conus_nextgen.gpkg'

get_hydrofabric(outfile = gpkg)
```

From that, we can extract data for a VPU:

``` r
get_vpu_fabric(gpkg, 
               vpuid = "01", 
               outfile = './01_nextgen.gpkg')
```

Or, we can extract a subset based on an input identifier (one of `id`,
`comid`, `hl_uri`, `poi_id`, `nldi_feature`, `xy`):

``` r
subset_fabric <- get_subset(gpkg = gpkg, comid = 101)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### From Remote

Coming soon…

### Questions?

Please reach out via an issue or PR if you have comments, concerns, or
questions!
