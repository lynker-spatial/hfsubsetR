#' Initialize a new hfsubset query
#' @returns A `hfsubset_query` object
#' @export
query <- function() {
  structure(list(), class = "hfsubset_query")
}

#' Set the identifier of a query
#' @param query A `hfsubset_query` object
#' @param identifier Identifier value
#' @param type Identifier type
#' @returns `query` with the identifier included
#' @export
query_set_id <- function(query, identifier, type = c("id", "comid", "hl_uri", "poi_id", "nldi_feature", "xy")) {
  .assert_query(query)

  # TODO: Check if id is already added?

  type <- match.arg(type)
  query$identifier <- structure(identifier, class = type)
  query
}

#' Set the layers of a query
#' @inheritParams query_set_id
#' @param layers A `character` vector of layer names
#' @returns `query` with the layers included
#' @export
query_set_layers <- function(query, layers) {
  .assert_query(query)

  if (!is.vector(layers, mode = "character")) {
    stop("Argument `layers` is not a vector of character.")
  }

  # TODO: Check if layers are already added?

  query$layers <- layers
  query
}

#' Set the source of a query
#' @inheritParams query_set_id
#' @param src A `hfsubset_query_source` object
#' @returns `query` with the source included
#' @seealso query_source_arrow query_source_sf
#' @export
query_set_source <- function(query, src) {
  .assert_query(query)

  if (!inherits(src, "hfsubset_query_source")) {
    stop("Argument `src` is not an `hfsubset_query_source` object.")
  }

  # TODO: Check if source is already added?

  query$source <- src
  query
}

#' Set the sink of a query
#' @inheritParams query_set_id
#' @param sink A character path to sink
#' @param overwrite If TRUE, then if the sink exists, it should be overwritten
#' @returns `query` with the sink included
#' @export
query_set_sink <- function(query, sink, overwrite = FALSE) {
  .assert_query(query)

  # TODO: Check if sink is already added?

  query$sink <- sink
  attr(query$sink, "overwrite") <- overwrite
  query
}

#' Accessor to get the ID of a query
#' @inheritParams query_set_id
#' @keywords internal
query_get_id <- function(query) {
  query$identifier
}

#' Accessor to get the layers of a query
#' @inheritParams query_set_id
#' @keywords internal
query_get_layers <- function(query) {
  query$layers
}

#' Accessor to get the source of a query
#' @inheritParams query_set_id
#' @keywords internal
query_get_source <- function(query) {
  query$source
}

#' Accessor to get the sink of a query
#' @inheritParams query_set_id
#' @keywords internal
query_get_sink <- function(query) {
  query$sink
}

#' @keywords internal
.assert_query <- function(query) {
  if (!inherits(query, "hfsubset_query")) {
    stop("Argument `query` is not an `hfsubset_query` object.", call. = FALSE)
  }
}
