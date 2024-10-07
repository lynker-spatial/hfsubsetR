#' Class OGRSQLDriver
#'
#' OGRSQLDriver objects are created by [OGRSQL()] and used to select the correct
#' method in [dbConnect()].
#' They are a superclass of the [DBIDriver-class] class, and used purely for dispatch.
#' @keywords internal
#' @export

setClass("OGRSQLDriver", contains = "DBIDriver")

#' @rdname OGRSQLDriver-class
#' @export

setMethod("dbDataType", "OGRSQLDriver", function(dbObj, obj, ...) {
  ## see "type of the fields" http://www.gdal.org/ogr_sql.html
  if (is.factor(obj)) return("character")
  if (is.data.frame(obj)) return(callNextMethod(dbObj, obj))
  
  switch(typeof(obj),
         logical = "boolean",
         character = "character",
         double = "numeric",
         integer = "integer",
         list = "character",
         raw = "character",
         blob = "character",
         stop("Unsupported type", call. = FALSE)
  )
}
)

#' @rdname OGRSQLDriver-class
#' @export

setMethod("dbIsValid", "OGRSQLDriver", function(dbObj, ...) { TRUE })

#' @rdname OGRSQLDriver-class
#' @export

setMethod("dbUnloadDriver", "OGRSQLDriver", function(drv, ...) {
  TRUE
})

#' @rdname OGRSQLDriver-class
#' @export

setMethod("dbGetInfo", "OGRSQLDriver", function(dbObj, ...) {
  vers <- sf::sf_extSoftVersion()
  list(
    name = "OGRSQLDriver",
    note = "virtual SQL driver for OGR Resources",
    driver.version = vers["GDAL"],
    client.version = utils::packageVersion("hfsubsetR")
  )
})




