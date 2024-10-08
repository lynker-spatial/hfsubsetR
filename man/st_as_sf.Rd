% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_ogr.R
\name{st_as_sf}
\alias{st_as_sf}
\alias{st_as_sf.tbl_OGRSQLConnection}
\alias{collect}
\title{Force collection of a OGR query
Convert as_ogr to a data frame or sf object}
\usage{
\method{st_as_sf}{tbl_OGRSQLConnection}(x, ...)
}
\arguments{
\item{x}{output of [as_ogr()]}

\item{...}{passed to [collect()]}
}
\value{
a data frame from `collect()`, sf data frame from `st_as_sf()` (only if it contains an `sfc` geometry column)
}
\description{
Force collection of a OGR query
Convert as_ogr to a data frame or sf object
}
