
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hfsubsetR

<!-- badges: start -->

[![R CMD
Check](https://github.com/lynker-spatial/hfsubsetR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lynker-spatial/hfsubsetR/actions/workflows/R-CMD-check.yaml)
[![Test
coverage](https://github.com/lynker-spatial/hfsubsetR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lynker-spatial/hfsubsetR/actions/workflows/test-coverage.yaml)
[![Dependencies](https://img.shields.io/badge/dependencies-6/70-orange?style=flat)](#)
<!-- badges: end -->

The goal of `hfsubsetR` is to extract hydrofabric subsets from cloud or
local archives built in `lynker-spatial`.

## Installation

You can install the development version of `hfsubsetR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lynker-spatial/hfsubsetR")
```

## Remote Access

This is a basic example showing how to access hydrofabric data
(reference v2.2) upstream of COMID 101 from remote parquet datasets
hosted by [lynker-spatial](https://staging.lynker-spatial.com/).

``` r
library(hfsubsetR)

get_subset(comid = 101, 
           source  = "s3://lynker-spatial/hydrofabric",
           type = 'reference',
           hf_version = "2.2")
```

## Local

This is a basic example showing how to access the same hydrofabric data
from local parquet datasets:

``` r
get_subset(comid = 101, source  = "/Volumes/MyBook/conus-hydrofabric")
```

## Hydrolocation Extraction

This is a basic example showing how to access the same hydrolocation
data from a local parquet dataset. A remote source could be used
identically:

``` r
# Could use remote source too!
source <- '/Volumes/MyBook/conus-hydrofabric'

pois <- arrow::open_dataset(glue::glue("{source}/v2.2/conus_hl")) |>
  dplyr::filter(hl_source == 'GFv20') |>
  dplyr::collect()
```
