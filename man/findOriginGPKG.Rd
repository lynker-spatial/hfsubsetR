% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_origin_gpkg.R
\name{findOriginGPKG}
\alias{findOriginGPKG}
\title{Find Origin From ID}
\usage{
findOriginGPKG(
  gpkg,
  id = NULL,
  comid = NULL,
  hl_uri = NULL,
  poi_id = NULL,
  nldi_feature = NULL,
  xy = NULL
)
}
\arguments{
\item{gpkg}{a local geopackages file path}

\item{id}{hydrofabric id. datatype: string / vector of strings e.g., 'wb-10026' or c('wb-10026', 'wb-10355')}

\item{comid}{NHDPlusV2 COMID. datatype: int / vector of int e.g., 61297116 or c(61297116 , 6129261)}

\item{hl_uri}{hydrolocation URI. datatype: string / vector of string / a url e.g., HUC12-010100100101 or c(HUC12-010100100101 , HUC12-010100110104)}

\item{poi_id}{POI identifier. datatype: int / vector of int e.g., 266387 or c(266387, 266745)}

\item{nldi_feature}{list with names 'featureSource' and 'featureID' where 'featureSource' is derived from the "source" column of the response of dataRetrieval::get_nldi_sources() and the 'featureID' is a known identifier from the specified 'featureSource'. datatype: a url e.g., 'https://labs.waterdata.usgs.gov/api/nldi/linked-data/census2020-nhdpv2'}

\item{xy}{Location given as vector of XY in EPSG:4326 (longitude, latitude, crs)}
}
\value{
data.frame
}
\description{
Find Origin From ID
}
