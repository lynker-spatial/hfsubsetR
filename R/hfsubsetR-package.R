#' @keywords internal
#' @aliases hfsubsetR-package
"_PACKAGE"

#' @importFrom utils packageVersion
#' @importFrom methods setMethod setClass setOldClass callNextMethod new show
#' @importFrom DBI  dbConnect  dbSendQuery dbFetch  dbDisconnect dbClearResult dbIsValid
#'  dbHasCompleted dbReadTable dbListTables dbExistsTable dbDataType dbGetInfo dbUnloadDriver
#' @importFrom dbplyr sql
#' @importFrom sf read_sf st_layers sf_extSoftVersion st_sfc st_set_crs st_point st_crs st_bbox st_as_sf write_sf
#' @importFrom nhdplusTools discover_nhdplus_id get_sorted
#' @importFrom jsonlite toJSON
#' @importFrom glue glue
#' @importFrom dplyr tbl select mutate rename if_any filter everything distinct collect any_of `%>%`
#' @importFrom arrow open_dataset
#' @importFrom httr GET progress write_disk
NULL
