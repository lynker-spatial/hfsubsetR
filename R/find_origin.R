#' Find an origin from indexed IDs
#' @param network A `dplyr`-compatible object.
#' @param id A queryable identifier of type `type`.
#' @param type An index type describing `id`.
#' @returns A network origin. If a single origin is not found,
#'          then an exception is raised.
#' @export
find_origin <- function(
  network,
  id,
  type = c("id", "comid", "hl_uri", "poi_id", "nldi_feature", "xy")
) {
  type <- match.arg(type)
  query <- structure(id, class = type)

  origin <- try(find_origin_query(query, network))

  if (inherits(origin, "try-error")) {
    stop(origin, call. = FALSE)
  }
  
  origin <-
    origin |>
    dplyr::select(id, toid, vpuid, topo, hydroseq) |>
    dplyr::distinct() |>
    dplyr::collect() |> 
    dplyr::slice_min(hydroseq, with_ties = TRUE)

  if (nrow(origin) == 0) {
    stop("No origin found")
  } else if (nrow(origin) > 1) {
    stop("Multiple origins found: ", dput(origin$id))
  } else {
    return(origin)
  }
}

#' S3 dispatch on query identifier type
#' @param id A queryable identifier, see `find_origin`.
#' @param network A `dplyr`-compatible object.
#' @returns `network` after applying a [dplyr::filter] expression.
#' @keywords internal
find_origin_query <- function(id, network) {
  UseMethod("find_origin_query")
}

#' @method find_origin_query default
#' @keywords internal
find_origin_query.default <- function(id, network) {
  stop(paste(
    "identifier of class",
    paste0("`", class(id), "`", collapse = "/"),
    "not supported"
  ))
}

#' @method find_origin_query id
#' @keywords internal
find_origin_query.id <- function(id, network) {
  id <- unclass(id)
  dplyr::filter(network, id == !!id)
}

#' @method find_origin_query comid
#' @keywords internal
find_origin_query.comid <- function(comid, network) {
  comid <- unclass(comid)
  dplyr::filter(network, hf_id == !!comid)
}

#' @method find_origin_query hl_uri
#' @keywords internal
find_origin_query.hl_uri <- function(hl_uri, network) {
  hl_uri <- unclass(hl_uri)
  dplyr::filter(network, hl_uri == !!hl_uri)
}

#' @method find_origin_query poi_id
#' @keywords internal
find_origin_query.poi_id <- function(poi_id, network) {
  poi_id <- unclass(poi_id)
  dplyr::filter(network, poi_id == !!poi_id)
}

#' @method find_origin_query nldi_feature
#' @keywords internal
find_origin_query.nldi_feature <- function(nldi_feature, network) {
  .Class <- "comid"

  nldi_feature <- structure(
    nhdplusTools::discover_nhdplus_id(nldi_feature = nldi_feature),
    class = "comid"
  )

  NextMethod()
}

#' @method find_origin_query xy
#' @keywords internal
find_origin_query.xy <- function(xy, network) {
  .Class <- "comid"

  xy <- structure(
    nhdplusTools::discover_nhdplus_id(point = sf::st_sfc(sf::st_point(xy), crs = 4326)),
    class = "comid"
  )

  NextMethod()
}
