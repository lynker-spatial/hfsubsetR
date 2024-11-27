#' Execute a query subset
#' @param query A `hfsubset_query` object
#' @returns A list of hydrofabric layers, or the path to the sink of the query
#' @export
query_subset <- function(query) {
  
  identifier <- query_get_id(query)
  
  origin <- find_origin(
    network = query_source_layer(query$source, "network"),
    id = identifier,
    type = class(identifier)
  )

  network <- query_source_layer(query$source, "network") |>
    dplyr::filter(vpuid == !!origin$vpuid) |>
    dplyr::select(dplyr::any_of(c("id", "toid", "divide_id", "poi_id"))) |>
    dplyr::distinct() |>
    dplyr::collect()

  topology <- suppressWarnings(nhdplusTools::get_sorted(network, outlets = origin$toid))
  topology$toid[nrow(topology)] <- NA

  all_identifiers <-
    as.matrix(topology) |>
    as.vector() |>
    unique()

  all_identifiers <-
    all_identifiers[!is.na(all_identifiers)]

  query$vpuid <- origin$vpuid
  query$requested <- all_identifiers

  query_extract(query)
}

#' Perform data extraction from a query
#' @param query A `hfsubset_query` object
#' @returns A list of hydrofabric layers, or the path to the sink of the query
#' @note This should be called from query_subset().
#' @keywords internal
query_extract <- function(query) {
  layers  <- query_get_layers(query)
  result  <- new.env(size = length(layers))
  outfile <- query_get_sink(query)

  for (layer in layers) {
    layer_data <-
      query_get_source(query) |>
      query_source_layer(layer)

    if (inherits(layer_data, "try-error")) {
      warning("Layer ", layer, " not available", call. = FALSE)
      next
    }

    layer_data <- dplyr::filter(layer_data, vpuid == !!query$vpuid)
    variables <- c("COMID", "FEATUREID", "divide_id", "link", "to", "id", "toid", "ID", "poi_id")
  
    if ("poi_id" %in% colnames(layer_data)) {
      layer_data <- dplyr::mutate(layer_data, poi_id = as.character(poi_id))
    }

    layer_data <- dplyr::filter(layer_data, dplyr::if_any(
      .cols = dplyr::any_of(variables),
      .fns = ~ . %in% !!query$requested
    ))

    # TODO: Refactor, unneeded complexity
    if (inherits(layer_data, "tbl_OGRSQLConnection")) {
      layer_data <- sf::st_as_sf(layer_data)
    } else if (inherits(layer_data, "arrow_dplyr_query")) {
      if (any(c("geom", "geometry") %in% names(layer_data))) {
        layer_data <- read_sf_dataset(layer_data)
      } else {
        layer_data <- dplyr::collect(layer_data)
      }
    }

    # TODO: Refactor? abstract environment vs file sink
    if (!is.null(outfile)) {
      sf::write_sf(layer_data, outfile, layer)
    } else {
      assign(layer, layer_data, envir = result)
    }
  }

  if (!is.null(outfile)) {
    outfile
  } else {
    as.list(result)
  }
}
