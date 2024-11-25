#' Check if an object is an hfsubset query source
#' @param query_source An object
#' @returns TRUE if `query_source` inherits from 'hfsubset_query_source'.
#' @keywords internal
is_query_source <- function(query_source) {
  inherits(query_source, "hfsubset_query_source")
}

#' Read/open a layer from a query source
#' @keywords internal
query_source_layer <- function(query_source, layer, ...) {
  UseMethod("query_source_layer")
}

#' List out all available layers from a query source
#' @keywords internal
query_source_layers <- function(query_source, ...) {
  UseMethod("query_source_layers")
}
