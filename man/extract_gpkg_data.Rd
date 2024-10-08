% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_subset.R
\name{extract_gpkg_data}
\alias{extract_gpkg_data}
\title{Extract Data from Arrow Stores}
\usage{
extract_gpkg_data(gpkg, vpu, ids, lyrs, outfile = NULL)
}
\arguments{
\item{gpkg}{a local gpkg file}

\item{vpu}{an optional VPU to quicken search}

\item{ids}{all identifiers to extract}

\item{lyrs}{hydrofabric layers to subset}

\item{outfile}{a path to write resulting geopackage}
}
\value{
list or file path
}
\description{
Extract Data from Arrow Stores
}
