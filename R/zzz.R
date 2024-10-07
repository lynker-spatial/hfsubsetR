.onLoad <- function(...) {
  .S3method("findOriginQuery", "default")
  .S3method("findOriginQuery", "hf_id")
  .S3method("findOriginQuery", "comid")
  .S3method("findOriginQuery", "hl_uri")
  .S3method("findOriginQuery", "poi_id")
  .S3method("findOriginQuery", "nldi_feature")
  .S3method("findOriginQuery", "xy")
  
  .S3method("findOriginQueryGPKG", "default")
  .S3method("findOriginQueryGPKG", "hf_id")
  .S3method("findOriginQueryGPKG", "comid")
  .S3method("findOriginQueryGPKG", "hl_uri")
  .S3method("findOriginQueryGPKG", "poi_id")
  .S3method("findOriginQueryGPKG", "nldi_feature")
  .S3method("findOriginQueryGPKG", "xy")
  
  op <- getOption("ogr.query.debug")
  if (is.null(op) || is.na(op) || length(op) < 1) {
    op <- FALSE
    options(ogr.query.debug = op)
  }
  
  invisible()
}
