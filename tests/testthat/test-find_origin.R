testthat::test_that("find_origin works as expected", {
  params <- list(
    list(
      type = "id",
      value = 19541984,
      expected = list(
        id = 19541984,
        topo = "fl-fl",
        hydroseq = 1404202
      )
    ),
    list(
      type = "hl_uri",
      value = "huc12-060400010704",
      expected = list(
        toid = 19558132,
        topo = "fl-fl",
        hydroseq = 1356216
      )
    ),
    list(
      type = "comid",
      value = 19541968,
      expected = list(
        id = 19541968,
        toid = 19541946,
        topo = "fl-fl",
        hydroseq = 1355615
      )
    ),
    list(
      type = "xy",
      value = c(-88.1971, 35.38836),
      expected = list(
        id = 19550296,
        toid = 19550502,
        topo = "fl-fl",
        vpuid = "06",
        hydroseq = 1403112
      )
    )
  )

  bad_params <- list(
    list(type = "id", value = 99999999999999),
    list(type = "hl_uri", value = "notreal")
  )

  source_path <- testthat::test_path("testdata", "conus_reference_subset.gpkg")
  testthat::skip_if(!file.exists(source_path), paste("File", source_path, "does not exist"))

  origin_source <- query_source_sf(source_path)
  testthat::expect_s3_class(origin_source, c("hfsubset_query_source_sf", "hfsubset_query_source"))

  # Suppresses dbplyr warnings
  network <- suppressWarnings(query_source_layer(origin_source, "network"))
  testthat::expect_s3_class(network, c("tbl_OGRSQLConnection"))

  # Check that good parameters return expected data
  for (param in params) {
    testthat::test_that(paste0("query<", param$type, "> returns expected results"), {
      origin <- find_origin(
        network = network,
        id = param$value,
        type = param$type
      )

      testthat::expect_equal(nrow(origin), 1)
      for (nm in names(param$expected)) {
        testthat::expect_equal(origin[[nm]], param$expected[[nm]])
      }
    })
  }

  # Check that bad parameters throw exceptions
  for (param in bad_params) {
    testthat::test_that(paste0("query<", params$type, "> throws error"), {
      testthat::expect_error(
        find_origin(
          network = network,
          id = param$value,
          type = param$type
        ),
        "No origin found"
      )
    })
  }
})
