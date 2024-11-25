#' Create a new `sf` query source.
#' @param srcname Path or VSI URI to source
#' @param ... Unused
#' @returns An `hfsubset_query_source_sf` object
#' @export
query_source_sf <- function(srcname, ...) {
  structure(srcname, class = c("hfsubset_query_source_sf", "hfsubset_query_source"))
}

#' @method query_source_layer hfsubset_query_source_sf
#' @keywords internal
query_source_layer.hfsubset_query_source_sf <- function(query_source, layer, ...) {
  as_ogr(as.character(query_source), layer = layer)
}

#' @method query_source_layers hfsubset_query_source_sf
#' @keywords internal
query_source_layers.hfsubset_query_source_sf <- function(query_source, ...) {
  sf::st_layers(query_source)
}
