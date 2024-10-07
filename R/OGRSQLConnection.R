#' Class OGRSQLConnection (and methods)
#'
#' OGRSQLConnection objects are created by passing [OGRSQL()] as first
#' argument to [DBI::dbConnect()].
#' They are a superclass of the [DBIConnection-class] class.
#' @keywords internal
#' @export
setClass(
  "OGRSQLConnection",
  contains = "DBIConnection",
  slots = list(DSN = "character", 
               readonly = "logical")
)

#' @rdname OGRSQLConnection-class
#' @export

setMethod("show", "OGRSQLConnection", function(object) {
  cat("<OGRSQLConnection>\n")
  tables <- DBI::dbListTables(object)
  dsn <- object@DSN
  if (grepl("pass", dsn, ignore.case = TRUE) ||
      grepl("user", dsn, ignore.case = TRUE)) {
    dsn <- paste0(strsplit(dsn, "\\s")[[1L]][1L], "...")
  }
  cat("\tDSN: ", dsn, "\n", sep = "")
  cat("tables: ", paste(tables, collapse = ", "), "\n", sep = "")
})

#' @rdname OGRSQLConnection-class
#' @export

setMethod("dbSendQuery", "OGRSQLConnection", function(conn, statement, ...) {
  ## quiet and fake layer because we aren't using layer  = (it's in the query)
  args <- list(...)
  args$dsn <- conn@DSN
  args$layer <- "<fake layer>"
  args$query <- statement   
  args$quiet <- TRUE
  args$as_tibble <- TRUE
  #browser()
  qu <- as.character(args$query)
  
  if (grepl("AS.*q", qu) &&
      grepl("WHERE \\(0 = 1)", qu)) {
    ## workaround for non-DB sources
    args$query <- dbplyr::sql(gsub("WHERE \\(0 = 1)", "LIMIT 0", qu))
  }
  op <- options(warn = -1)
  on.exit(options(op), add = TRUE)
  layer_data <- do.call(sf::st_read, args)
  
  if (getOption("ogr.query.debug")) {
    message(
      sprintf(
        "-------------\nOGR debug ....\nSQL:\n%s\nnrows read:\n%i",
        statement
      ),
      nrow(layer_data)
    )
  }
  if (inherits(layer_data, "try-error")) {
    if (length(gregexpr("SELECT", statement, ignore.case = TRUE)[[1]]) > 1) {
      stop(
        sprintf(
          "executing SQL failed: \n%s\n\nperhaps driver in use does not support sub-queries?",
          statement
        )
      )
    } else {
      stop("executing SQL failed")
    }
  }
  new("OGRSQLResult", layer_data = layer_data)
  
})

#' @rdname OGRSQLConnection-class
#' @export
setMethod("dbReadTable", c(conn = "OGRSQLConnection", name = "character"), function(conn, name, ...) {
  x <- dbSendQuery(conn, sprintf("SELECT * FROM %s", name))
  dbFetch(x)
})

#' @rdname OGRSQLConnection-class
#' @export

setMethod("dbListTables", c(conn = "OGRSQLConnection"), function(conn, ...) {
  layers <- sf::st_layers(conn@DSN, ...)
  layers$name
})

#' @rdname OGRSQLConnection-class
#' @export
setMethod("dbExistsTable", c(conn = "OGRSQLConnection"), function(conn, name, ...) {
  name %in% dbListTables(conn, ...)
})
