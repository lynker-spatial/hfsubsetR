.assert_query <- function(query) {
  if (!inherits(query, "hfsubset_query")) {
    stop("Argument `query` is not an `hfsubset_query` object.", call. = FALSE)
  }
}

#' Initialize a new hfsubset query
#' @export
query <- function() {
  structure(list(), class = "hfsubset_query")
}

#' @export
query_set_id <- function(query, identifier, type = c("id", "comid", "hl_uri", "poi_id", "nldi_feature", "xy")) {
  .assert_query(query)

  # TODO: Check if id is already added?

  type <- match.arg(type)
  query$identifier <- structure(identifier, class = type)
  query
}

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

#' @export
query_set_sink <- function(query, sink, overwrite = FALSE) {
  .assert_query(query)

  # TODO: Check if sink is already added?

  query$sink <- sink
  attr(query$sink, "overwrite") <- overwrite
  query
}

#' @keywords internal
query_get_id <- function(query) {
  query$identifier
}

#' @keywords internal
query_get_layers <- function(query) {
  query$layers
}

#' @keywords internal
query_get_source <- function(query) {
  query$source
}

#' @keywords internal
query_get_sink <- function(query) {
  query$sink
}
