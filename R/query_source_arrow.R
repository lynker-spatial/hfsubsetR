#' Create a new `arrow` query source.
#' @param srcname URI to `arrow`-compatible dataset
#' @param ... Unused
#' @returns An `hfsubset_query_source_arrow` object
#' @export
query_source_arrow <- function(srcname, ...) {
  structure(srcname, class = c("hfsubset_query_source_arrow", "hfsubset_query_source"))
}

#' @method query_source_layer hfsubset_query_source_arrow
#' @keywords internal
query_source_layer.hfsubset_query_source_arrow <- function(query_source, layer, ...) {
  arrow::open_dataset(paste0(query_source, "_", layer))
}

#' @method query_source_layers hfsubset_query_source_arrow
#' @keywords internal
query_source_layers.hfsubset_query_source_arrow <- function(query_source, ...) {
  NULL # TODO: how do we do this with arrow datasets?
}
