#' Find Origin From ID
#' @param gpkg a local geopackages file path
#' @inheritParams get_subset
#' @return data.frame
#' @export

findOriginGPKG <- function(
    gpkg,
    id = NULL,
    comid = NULL,
    hl_uri = NULL,
    poi_id = NULL,
    nldi_feature = NULL,
    xy = NULL
) {
  

  # Capture arguments
  .args <- c(as.list(environment()))
  # Pop `network` off arguments
  .args <- .args[seq.int(2, length(.args), 1)]
  # Get all non-NULL arguments
  .args <- .args[!vapply(.args, is.null, logical(1))]
  
  if (length(.args) == 0) {
    stop("at least one argument other than `network` is required.")
  }
  
  if (length(.args) > 1) {
    stop(paste(
      "only one identifier type should be passed, but received",
      paste0("`", names(.args), "`", collapse = "/")
    ))
  }
  
  .query <- .makeOriginQueryClass(
    .args[[1]],
    ifelse(names(.args) == "id", "hf_id", names(.args))
  )
  
  hf_id <- hydroseq <- poi_id <- toid <- topo <- vpuid <- NULL
  
  origin <-
    findOriginQueryGPKG(.query, gpkg) |> 
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


#' S3 method for dispatching on query type
#' @return SQLite connection
#' @keywords internal
findOriginQueryGPKG <- function(id, gpkg, ...) {
  if (!inherits(gpkg, "character")) {
    stop("`network` must be a path/URI")
  }
  
  UseMethod("findOriginQueryGPKG")
}


#' @method findOriginQuery default
#' @keywords internal
findOriginQueryGPKG.default <- function(id, gpkg, ...) {
  stop(paste(
    "identifier of class",
    paste0("`", class(id), "`", collapse = "/"),
    "not supported"
  ))
}


#' @method findOriginQuery hf_id
#' @keywords internal
findOriginQueryGPKG.hf_id <- function(id, gpkg, ...) {
  as_ogr(gpkg, "network") |>
    dplyr::filter(id == !!id)
}


#' @method findOriginQuery comid
#' @keywords internal
findOriginQueryGPKG.comid <- function(comid, gpkg, ...) {
  hf_id <- NULL
  as_ogr(gpkg, "network") |>
    dplyr::filter(hf_id == !!comid)
}


#' @method findOriginQuery hl_uri
#' @keywords internal
findOriginQueryGPKG.hl_uri <- function(hl_uri, gpkg, ...) {
  as_ogr(gpkg, "network") |>
    dplyr::filter(hl_uri == !!hl_uri) 
}


#' @method findOriginQuery poi_id
#' @keywords internal
findOriginQueryGPKG.poi_id <- function(poi_id, gpkg, ...) {
  as_ogr(gpkg, "network") |>
    dplyr::filter(poi_id == !!poi_id)
}


#' @method findOriginQuery nldi_feature
#' @keywords internal
findOriginQueryGPKG.nldi_feature <- function(nldi_feature, gpkg, ...) {
  .Class <- "comid"
  nldi_feature <-
    nhdplusTools::discover_nhdplus_id(nldi_feature = nldi_feature) |>
    .makeOriginQueryClass("comid")
  
  NextMethod()
}


#' @method findOriginQuery xy
#' @keywords internal
findOriginQueryGPKG.xy <- function(xy, gpkg, ...) {
  .Class <- "comid"
  xy <-
    sf::st_point(xy) |>
    sf::st_sfc(crs = 4326) |>
    nhdplusTools::discover_nhdplus_id(point = _) |>
    .makeOriginQueryClass("comid")
  
  NextMethod()
}
