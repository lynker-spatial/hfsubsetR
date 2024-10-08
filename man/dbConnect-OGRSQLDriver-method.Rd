% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_ogr.R
\name{dbConnect,OGRSQLDriver-method}
\alias{dbConnect,OGRSQLDriver-method}
\title{dbConnect}
\usage{
\S4method{dbConnect}{OGRSQLDriver}(drv, DSN = "", readonly = TRUE, ...)
}
\arguments{
\item{drv}{OGRSQLDriver created by \code{OGRSQL()}}

\item{DSN}{data source name}

\item{readonly}{open in readonly mode (`TRUE` is the only option)}

\item{...}{ignored}
}
\description{
dbConnect for sources that can be read by package sf
}
\details{
The 'OGRSQL' available is documented with GDAL: https://gdal.org/user/ogr_sql_dialect.html
}
