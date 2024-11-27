# Initialize constant variables

library(testthat)

# devtools::load_all()

outfile <- "subset_test.gpkg"

# --------------------- Reference fabric ---------------------------#

params <- dplyr::mutate(dplyr::tribble(
    ~layer,                               ~type,
    "flowpaths",                          "Line String",
    "divides",                            "Polygon",
    "network",                            NA_character_,
    c("flowpaths", "divides", "network"), c("Line String", "Polygon", NA_character_)
), TEST_ID = 19550502)

testthat::test_that("Local GPKG subsetting works as expected", {
    path <- testthat::test_path("testdata", "conus_reference_subset.gpkg")
    for (param in split(params, seq_len(nrow(params)))) {
        outfile <- tempfile(tmpdir = "/vsimem", fileext = ".gpkg")
        get_subset(outfile = outfile, id = param$TEST_ID, lyrs = unlist(param$layer), gpkg = path, overwrite = TRUE)

        testthat::test_that("names and types are correct", {
            layer_info <- sf::st_layers(outfile)
            testthat::expect_equal(layer_info$name, param$layer[[1]])
            testthat::expect_equal(layer_info$geomtype, as.list(param$type[[1]]))
        })

        sf::st_delete(outfile, quiet = TRUE)
    }
})


testthat::skip()



test_that("Local conus_reference gpkg flowpaths layer dimensions are correct and contains input id", {
  gpkg <- testthat::test_path("testdata", "conus_reference_subset.gpkg")
  TEST_ID <- 19550502

  get_subset(
    outfile = outfile,
    id = TEST_ID,
    lyrs = c("flowpaths"),
    gpkg = gpkg,
    overwrite = TRUE
  )

  layer_info <- sf::st_layers(outfile)

  subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowpaths")

  # Flowlines
  testthat::expect_true("id" %in% names(subset_flowpaths))
  testthat::expect_true("vpuid" %in% names(subset_flowpaths))
  testthat::expect_true("mainstemlp" %in% names(subset_flowpaths))
  testthat::expect_true(TEST_ID %in% subset_flowpaths$id)

  expect_equal(nrow(subset_flowpaths), 72)
  expect_equal(unique(subset_flowpaths$vpuid), "06")
  expect_equal(min(subset_flowpaths$mainstemlp), 1403109)
  expect_true(all(st_geometry_type(subset_flowpaths) %in% c("MULTILINESTRING", "LINESTRING")))

  unlink(outfile)
})

test_that("Local conus_reference gpkg divides layer dimensions are correct and contains input id ", {
  gpkg <- testthat::test_path("testdata", "conus_reference_subset.gpkg")
  TEST_ID <- 19550502

  get_subset(
    outfile = outfile,
    id = TEST_ID,
    lyrs = c("divides"),
    gpkg = gpkg,
    overwrite = TRUE
  )

  layer_info <- sf::st_layers(outfile)

  subset_divides <- read_sf("subset_test.gpkg", layer = "divides")

  # Flowlines
  testthat::expect_true("id" %in% names(subset_divides))
  testthat::expect_true("vpuid" %in% names(subset_divides))
  testthat::expect_true("divide_id" %in% names(subset_divides))
  testthat::expect_true(TEST_ID %in% subset_divides$id)


  # Divides
  expect_equal(nrow(subset_divides), 72)
  expect_equal(unique(subset_divides$vpuid), "06")
  expect_true(all(st_geometry_type(subset_divides) == "POLYGON"))

  unlink(outfile)
})

test_that("Local conus_reference gpkg network layer dimensions are correct and contains input id", {
  gpkg <- testthat::test_path("testdata", "conus_reference_subset.gpkg")
  TEST_ID <- 19550502

  get_subset(
    outfile = outfile,
    id = TEST_ID,
    lyrs = c("network"),
    gpkg = gpkg,
    overwrite = TRUE
  )

  layer_info <- sf::st_layers(outfile)

  subset_network <- read_sf("subset_test.gpkg", layer = "network")

  # Network
  testthat::expect_true("id" %in% names(subset_network))
  testthat::expect_true("vpuid" %in% names(subset_network))
  testthat::expect_true("divide_id" %in% names(subset_network))
  testthat::expect_true(TEST_ID %in% subset_network$id)

  expect_equal(nrow(subset_network), 73)
  expect_equal(unique(subset_network$vpuid), "06")

  unlink(outfile)
})

test_that("Local conus_reference gpkg flowpaths subset for a flowpath at the most upstream location
    results in a subset of the flowpath and its 1 neighbors", {
  gpkg <- testthat::test_path("testdata", "conus_reference_subset.gpkg")

  TEST_ID <- 19544390

  get_subset(
    outfile = outfile,
    id = TEST_ID,
    lyrs = c("flowpaths"),
    gpkg = gpkg,
    overwrite = TRUE
  )

  layer_info <- sf::st_layers(outfile)

  subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowpaths")

  mapview::mapview(dplyr::filter(subset_flowpaths, id != TEST_ID), color = "red") +
    mapview::mapview(dplyr::filter(subset_flowpaths, id == TEST_ID), color = "green")

  # Flowlines
  testthat::expect_true("id" %in% names(subset_flowpaths))
  testthat::expect_true("vpuid" %in% names(subset_flowpaths))
  testthat::expect_true("mainstemlp" %in% names(subset_flowpaths))
  testthat::expect_true(TEST_ID %in% subset_flowpaths$id)

  expect_equal(nrow(subset_flowpaths), 2)
  expect_equal(unique(subset_flowpaths$vpuid), "06")
  expect_equal(min(subset_flowpaths$mainstemlp), 1403141)
  expect_true(all(st_geometry_type(subset_flowpaths) %in% c("MULTILINESTRING", "LINESTRING")))

  unlink(outfile)
})

test_that("Local conus_reference gpkg flowpaths check 1 'id' from each VPU and make sure dimensions are correct and contains input id", {
  gpkg <- testthat::test_path("testdata", "conus_reference_subset.gpkg")
  # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"

  # # NOTE: Grabs 1 ID from each streamorder to build the TEST_IDS list below:
  # streamorder_ids = sf::read_sf(gpkg, query = "
  #                     SELECT *
  #                     FROM (
  #                         SELECT *, ROW_NUMBER() OVER (PARTITION BY streamorde ORDER BY id) AS row_num
  #                         FROM network
  #                     ) subquery
  #                     WHERE row_num <= 1
  #                 ")

  # # TODO: prints out the TEST_IDS list
  # streamorder_ids$id %>%
  # unique() %>%
  # paste0("'", ., "'", collapse = ", ")

  # NOTE: this is 1 ID from each stream order
  TEST_IDS <- c("19541902", "19541910", "19541920", "19542124", "19543764", "19542748")

  for (id in TEST_IDS) {
    # message(id)
    # id

    get_subset(
      outfile = outfile,
      id = id,
      lyrs = c("flowpaths"),
      gpkg = gpkg,
      overwrite = TRUE
    )

    subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowpaths")

    # mapview::mapview(dplyr::filter(subset_flowpaths, id != id) , color = "red") +
    # mapview::mapview(dplyr::filter(subset_flowpaths, id == id),color = "green")

    # Flowlines
    testthat::expect_true("id" %in% names(subset_flowpaths))
    testthat::expect_true("vpuid" %in% names(subset_flowpaths))
    testthat::expect_true("mainstemlp" %in% names(subset_flowpaths))
    testthat::expect_true(id %in% subset_flowpaths$id)
    expect_true(all(st_geometry_type(subset_flowpaths) %in% c("MULTILINESTRING", "LINESTRING")))

    # TODO: this is not actually testing the package but just making sure the test is writting and deleting the data in each loop
    testthat::expect_true(
      file.exists(outfile)
    )

    unlink(outfile)

    testthat::expect_false(
      file.exists(outfile)
    )
  }

  unlink(outfile)
})


test_that("Local conus_reference gpkg if outfile is specified a file is created at the given outfile, if not, no file is created", {
  gpkg <- testthat::test_path("testdata", "conus_reference_subset.gpkg")
  # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"

  TEST_ID <- 19550502

  get_subset(
    outfile = outfile,
    id = TEST_ID,
    lyrs = c("flowpaths"),
    gpkg = gpkg,
    overwrite = TRUE
  )

  testthat::expect_true(
    file.exists(outfile)
  )

  unlink(outfile)

  response_data <- get_subset(
    outfile = NULL,
    id = TEST_ID,
    lyrs = c("flowpaths"),
    gpkg = gpkg,
    overwrite = TRUE
  )

  testthat::expect_false(
    file.exists(outfile)
  )
})

unlink(outfile)
