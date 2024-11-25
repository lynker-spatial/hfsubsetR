#' @keywords internal
.dispatch_identifiers <- function(id, comid, hl_uri, poi_id, nldi_feature, xy) {
  mask <- !c(
    is.null(id),
    is.null(comid),
    is.null(hl_uri),
    is.null(poi_id),
    is.null(nldi_feature),
    is.null(xy)
  )

  if (sum(mask) == 0) {
    stop("One identifier type must be given.", call. = FALSE)
  } else if (sum(mask) > 1) {
    stop("No more than one identifier type must be given.", call. = FALSE)
  }

  index <- list(
    id = id,
    comid = comid,
    hl_uri = hl_uri,
    poi_id = poi_id,
    nldi_feature = nldi_feature,
    xy = xy
  )

  list(value = index[[which(mask)]], type = names(index)[[which(mask)]])
}

#' @title Build a hydrofabric subset
#' @param id hydrofabric id. datatype: string / vector of strings e.g., 'wb-10026' or c('wb-10026', 'wb-10355') 
#' @param comid NHDPlusV2 COMID. datatype: int / vector of int e.g., 61297116 or c(61297116 , 6129261) 
#' @param hl_uri hydrolocation URI. datatype: string / vector of string / a url e.g., HUC12-010100100101 or c(HUC12-010100100101 , HUC12-010100110104) 
#' @param poi_id POI identifier. datatype: int / vector of int e.g., 266387 or c(266387, 266745)
#' @param nldi_feature list with names 'featureSource' and 'featureID' where 'featureSource' is derived from the "source" column of the response of dataRetrieval::get_nldi_sources() and the 'featureID' is a known identifier from the specified 'featureSource'. datatype: a url e.g., 'https://labs.waterdata.usgs.gov/api/nldi/linked-data/census2020-nhdpv2'
#' @param xy Location given as vector of XY in EPSG:4326 (longitude, latitude, crs)
#' @param type hydrofabric type
#' @param hf_version hydrofabric version
#' @param source hydrofabric source (local root directory or s3 link)
#' @param gpkg a local gpkg file
#' @param outfile If gpkg file path is provided, data will be written to a file.
#' @param lyrs layers to extract
#' @param domain hydrofabric domain
#' @param overwrite overwrite existing outfile file path. Default is FALSE
#' @export
#' @importFrom glue glue
#' @importFrom nhdplusTools discover_nhdplus_id get_sorted
#' @importFrom arrow open_dataset
#' @importFrom dplyr select filter collect `%>%` everything if_any any_of distinct rename
#' @importFrom sf st_set_crs write_sf st_sfc st_point st_bbox
get_subset <- function(
  id = NULL, 
  comid = NULL,  
  hl_uri = NULL, 
  poi_id = NULL, 
  nldi_feature = NULL, 
  xy = NULL, 
  lyrs = c("divides", "flowpaths", "network", "nexus"),
  gpkg = NULL,
  source = "s3://lynker-spatial/hydrofabric",
  hf_version = "2.2", 
  type = "nextgen",
  domain = "conus",
  outfile = NULL, 
  overwrite = FALSE
) {
  kv <- .dispatch_identifiers(id, comid, hl_uri, poi_id, nldi_feature, xy)

  if (!is.null(gpkg)) {
    src <- query_source_sf(gpkg)
  } else {
    uri <- paste(source, paste0("v", hf_version), type, domain, sep = "/")
    src <- query_source_arrow(uri)
  }
  
  .new <-
    query() |>
    query_set_id(identifier = kv$value, type = kv$type) |>
    query_set_layers(layers = lyrs) |>
    query_set_source(src)

  if (!is.null(outfile)) {
    .new <- query_set_sink(.new, sink = outfile, overwrite = overwrite)
  }

  query_subset(.new)
}
