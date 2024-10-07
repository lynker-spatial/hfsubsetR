#' Class OGRSQLResult (and methods)
#'
#' OGRSQLResult objects are created by [dbSendQuery()] or [dbSendStatement()],
#' and encapsulate the result of an SQL statement.
#' They are a superclass of the [DBIResult-class] class.
#' The "Usage" section lists the class methods overridden by \pkg{lazsf}.
#' @seealso
#' The corresponding generic functions
#' [DBI::dbFetch()], [DBI::dbClearResult()], and
#' [DBI::dbHasCompleted()].
#'
#' @export
#' @keywords internal
setClass("OGRSQLResult", contains = "DBIResult", slots = c(layer_data = "ANY"))


#' @rdname OGRSQLResult-class
#' @export

setMethod("show", "OGRSQLResult", function(object) {
            cat(sprintf("Field names: %s\n",
                        paste(names(object@layer_data), collapse = ", ")))
            invisible(NULL)
})

#' @rdname OGRSQLResult-class
#' @export
setMethod("dbFetch", "OGRSQLResult", function(res, n = -1, ...) { res@layer_data })

#' @rdname OGRSQLResult-class
#' @export
setMethod("dbClearResult", "OGRSQLResult", function(res, ...) { TRUE })

#' @rdname OGRSQLResult-class
#' @export
setMethod("dbHasCompleted", "OGRSQLResult", function(res, ...) { TRUE })


