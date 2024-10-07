#' @include OGRSQLConnection.R
#' @include OGRSQLDriver.R
NULL

#' OGRSQL
#' OGRSQL driver, use to [dbConnect()] to a data source readable by sf
#' @export

OGRSQL <- function(){ new("OGRSQLDriver") }

#' dbConnect
#'
#' dbConnect for sources that can be read by package sf
#'
#' The 'OGRSQL' available is documented with GDAL: https://gdal.org/user/ogr_sql_dialect.html
#' @param drv OGRSQLDriver created by \code{OGRSQL()}
#' @param DSN  data source name
#' @param readonly open in readonly mode (`TRUE` is the only option)
#' @param ... ignored
#' @export

setMethod("dbConnect", "OGRSQLDriver",
          function(drv, DSN = "", readonly = TRUE, ...) {
            ## FIXME: could be a new MEM dataset
            if (nchar(DSN) < 1) stop("DSN must be a valid data source name (file, connection string, url, ...)")
            new("OGRSQLDriver", DSN = DSN,  readonly = readonly, ...)
          })

#' @rdname OGRSQLConnection-class
#' @export

setMethod("dbDisconnect", "OGRSQLConnection", function(conn, ...) {
  conn@DSN <- ""
  conn
})


#' Delayed read for vector resources
#'
#' A lazy data frame for GDAL vector data sources. as_ogr is DBI compatible and designed to work with dplyr. 
#'
#' The output of `as_ogr()` is a 'tbl_OGRSQLConnection` that extends `tbl_dbi` and
#' may be used with functions and workflows in the normal DBI way, see [OGRSQL()] for
#' the as_ogr DBI support.
#'
#' To obtain an in memory data frame use an explict `collect()` or `st_as_sf()`.
#' A call to `collect()` is triggered by `st_as_sf()` and will add the sf class
#' to the output.
#'
#' @inheritParams sf::read_sf
#' @param x the data source (file path, url, or database connection)
#' @param query SQL query to pass in directly
#' @param ignore_lyrs pattern for layers to be ignored description
#' @return a 'tbl_OGRSQLConnection'
#' @export

as_ogr <- function(x, layer, query = NA, ignore_lyrs = "gpkg_|rtree_|sqlite_") {
  UseMethod("as_ogr")
}

#' @name as_ogr
#' @export
as_ogr.character <- function(x, layer, query, ignore_lyrs) {
  db <- dbConnect(OGRSQL(), x)
  as_ogr(db, layer, query = query, ignore_lyrs = ignore_lyrs)
}

#' @name as_ogr
#' @export
as_ogr.OGRSQLConnection <- function(x, layer, query, ignore_lyrs) {
  
  if (!is.na(query)) {
    if (!missing(layer)) message("'layer' argument ignored, using 'query'")
    return(dplyr::tbl(x, dbplyr::sql(query)))
  }
  
  if (missing(layer)) {
    tbls = dbListTables(x)
    tbls = tbls[!grepl(ignore_lyrs, tbls)]
    
    if(length(tbls) == 1){
      layer = tbls
    } else {
      layer = NULL
      print(tbls)
    }
  } 
  
  if(!is.null(layer)){
    if(layer %in% dbListTables(x)){
      x = tbl(x, layer)
    } else {
      stop(layer, ' not in gpkg.', call. = FALSE)
      dbDisconnect(x)
    }
  }
  
    x
    
}

#' Force collection of a OGR query
#' Convert as_ogr to a data frame or sf object
#' @param x output of [as_ogr()]
#' @param ... passed to [collect()]
#' @name st_as_sf
#' @return a data frame from `collect()`, sf data frame from `st_as_sf()` (only if it contains an `sfc` geometry column)
#' @importFrom sf st_as_sf
#' @importFrom dplyr collect
#' @export
#' @export st_as_sf
#' @export collect
#' @aliases collect

st_as_sf.tbl_OGRSQLConnection <- function(x, ...) {
  
  d <- collect(x, ...)

  if(any(c("geom", "geomtry") %in% colnames(d))){
    st_as_sf(d)
  } else {
    d
  }
  
}


