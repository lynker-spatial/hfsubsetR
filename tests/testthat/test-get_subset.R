helper_layer_expectations <- function(filepath, layer_name, layer_type) {
  layer_info <- sf::st_layers(filepath)
  testthat::expect_contains(layer_info$name, layer_name)
  testthat::expect_equal(layer_info$geomtype, as.list(layer_type))

  layer_data <- sf::read_sf(filepath, layer = layer_name[1])
  testthat::expect_contains(names(layer_data), c("id", "vpuid", "divide_id"))
  testthat::expect_contains(layer_data$id, test_id)

  layer_geom <- try(sf::st_geometry_type(layer_data), silent = TRUE)
  if (layer_name[1] == "flowpaths") {
    testthat::expect_setequal(layer_geom, "LINESTRING")
  } else if (layer_name[1] == "divides") {
    testthat::expect_setequal(layer_geom, "POLYGON")
  } else {
    testthat::expect_s3_class(layer_geom, "try-error", exact = TRUE)
  }
} # end helper_layer_expectations

helper_test_identifiers <- function() {
  # FIXME: 19542748

  c(
    19550502,
    19541902,
    19541910,
    19541920,
    19542124,
    19543764
  )
} # end helper_test_identifiers

helper_params <- function() {
  list(
    list(
      layer = "flowpaths",
      type = "Line String"
    ),
    list(
      layer = "divides",
      type = "Polygon"
    ),
    list(
      layer = "network",
      type = NA_character_
    ),
    list(
      layer = c("flowpaths", "divides", "network"),
      type = c("Line String", "Polygon", NA_character_)
    )
  )
} # end helper_params

helper_make_prefix <- function(...) {
  paste0("[", as.character(c(...)), "]", collapse = "")
}

for (test_id in helper_test_identifiers()) {
  # ===========================================================================
  # GPKG
  # ===========================================================================
  testthat::test_that(paste("Local geopackage subsetting works as expected for ID", test_id), {
    for (param in helper_params()) {
      # Get a random tempfile name
      memfile <- tempfile(tmpdir = "/vsimem", fileext = ".gpkg")

      # This will write the subset out to GDAL's /vsimem virtual filesystem
      get_subset(
        id = test_id,
        lyrs = param$layer,
        gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg"),
        outfile = memfile,
        overwrite = TRUE
      )

      testthat::expect_no_error(sf::st_layers(memfile))

      prefix <- helper_make_prefix(test_id, paste0(param$layer, collapse = ";"), "geopackage")
      testthat::test_that(paste0(prefix, "names and types are correct"), {
        helper_layer_expectations(memfile, param$layer, param$type)
      })

      # We can't use unlink() on the memfile, but sf::st_delete can safely handle it
      sf::st_delete(memfile, quiet = TRUE)
    }
  }) # end testthat::test_that

  # ===========================================================================
  # ARROW
  # ===========================================================================
  testthat::test_that(paste("Local arrow dataset subsetting works as expected for ID", test_id), {
    for (param in helper_params()) {
      memfile <- tempfile(tmpdir = "/vsimem", fileext = ".gpkg")

      withCallingHandlers(
        {
          get_subset(
            id = test_id,
            lyrs = param$layer,
            source = testthat::test_path("testdata", "parquet"),
            hf_version = "2.2",
            type = "reference",
            domain = "conus",
            outfile = memfile,
            overwrite = TRUE
          )
        },
        warning = function(w) {
          # Ignore this specific warning since it's unimportant
          if (grepl("geom\\_bbox", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        }
      )

      testthat::expect_no_error(sf::st_layers(memfile))
      
      prefix <- helper_make_prefix(test_id, paste0(param$layer, collapse = ";"), "arrow")
      testthat::test_that(paste0(prefix, "names and types are correct"), {
        helper_layer_expectations(memfile, param$layer, param$type)
      })

      sf::st_delete(memfile, quiet = TRUE)
    }
  }) # end testthat::test_that
} # end for (test_id in helper_test_identifiers())
