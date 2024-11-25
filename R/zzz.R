.onLoad <- function(...) {

  identifier_classes <- c(
    "default",
    "id",
    "comid",
    "hl_uri",
    "poi_id",
    "nldi_feature",
    "xy"
  )

  for (identifier_class in identifier_classes) {
    .S3method("find_origin_query", identifier_class)
  }

  query_source_classes <- c(
    "hfsubset_query_source_arrow",
    "hfsubset_query_source_sf"
  )

  for (query_source_class in query_source_classes) {
    .S3method("query_source_layer", query_source_class)
    .S3method("query_source_layers", query_source_class)
  }
  
  op <- getOption("ogr.query.debug")
  if (is.null(op) || is.na(op) || length(op) < 1) {
    op <- FALSE
    options(ogr.query.debug = op)
  }
  
  invisible()
}
