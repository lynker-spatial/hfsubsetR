
# Initialize constant variables
library(sf)
library(dplyr)
library(arrow)
library(testthat)

# devtools::load_all()

outfile <- 'subset_test.gpkg'

# --------------------- Reference fabric ---------------------------#

test_that("Local conus_reference gpkg flowpaths layer names and types are correct", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    TEST_ID = 19550502 

    get_subset(outfile = outfile, 
                id = TEST_ID,
               lyrs = c('flowpaths'),
                gpkg = gpkg,
               overwrite = TRUE
               )

    
    layer_info = sf::st_layers(outfile)    
    
    testthat::expect_equal(
        layer_info$name,
        c("flowpaths")
    )

    testthat::expect_equal(
        layer_info$geomtype,
        list("Line String")
    )
   
    unlink(outfile)
})


test_that("Local conus_reference gpkg divides layer names and types are correct", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    TEST_ID = 19550502 

    get_subset(outfile = outfile, 
                id = TEST_ID,
               lyrs = c('divides'),
                gpkg = gpkg,
               overwrite = TRUE
               )
    
    layer_info = sf::st_layers(outfile)    
    
    testthat::expect_equal(
        layer_info$name,
        c("divides")
    )

    testthat::expect_equal(
        layer_info$geomtype,
        list("Polygon")
    )
    
    unlink(outfile)
})

test_that("Local conus_reference gpkg network layer names and types are correct", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    TEST_ID = 19550502 

    get_subset(outfile = outfile, 
                id = TEST_ID,
               lyrs = c('network'),
                gpkg = gpkg,
               overwrite = TRUE
               )

    
    layer_info = sf::st_layers(outfile)    

    testthat::expect_equal(
        layer_info$name,
        c("network")
    )

    testthat::expect_true(
        is.na(layer_info$geomtype[[1]])
    )
    
    unlink(outfile)
})


test_that("Local conus_reference gpkg flowpaths, divides, network layer names and types are correct", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    TEST_ID = 19550502 

    get_subset(outfile = outfile, 
                id = TEST_ID,
                lyrs = c('flowpaths', 'divides', 'network'),
                gpkg = gpkg,
                overwrite = TRUE
               )

    layer_info = sf::st_layers(outfile)    
    
    testthat::expect_equal(
        layer_info$name,
      c('flowpaths', 'divides', 'network')
    )

    testthat::expect_equal(
        layer_info$geomtype[1],
        list("Line String")
    )
    
    testthat::expect_equal(
        layer_info$geomtype[2],
        list("Polygon")
    )

    testthat::expect_true(
        is.na(layer_info$geomtype[[3]])
    )
    
    unlink(outfile)
})

test_that("Local conus_reference gpkg flowpaths layer dimensions are correct and contains input id", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    TEST_ID = 19550502 

    get_subset(outfile = outfile, 
                id = TEST_ID,
                lyrs = c('flowpaths'),
                gpkg = gpkg,
                overwrite = TRUE
               )

    layer_info = sf::st_layers(outfile)    

    subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowpaths")

    # mapview::mapview(dplyr::filter(subset_flowpaths, id != TEST_ID) , color = "red") +
    # mapview::mapview(dplyr::filter(subset_flowpaths, id == TEST_ID),color = "green")  

    # Flowlines
    testthat::expect_true("id" %in% names(subset_flowpaths))
    testthat::expect_true("vpuid" %in% names(subset_flowpaths))
    testthat::expect_true("mainstemlp" %in% names(subset_flowpaths))
    testthat::expect_true(TEST_ID %in% subset_flowpaths$id)

    expect_equal(nrow(subset_flowpaths), 1050)
    expect_equal(unique(subset_flowpaths$vpuid), "09")
    expect_equal(min(subset_flowpaths$mainstemlp), 2637849)
    expect_true(all(st_geometry_type(subset_flowpaths) %in% c("MULTILINESTRING", "LINESTRING")))

    unlink(outfile)
})

test_that("Local conus_reference gpkg divides layer dimensions are correct and contains input id ", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    TEST_ID = 19550502 

    get_subset(outfile = outfile, 
                id = TEST_ID,
                lyrs = c('divides'),
                gpkg = gpkg,
                overwrite = TRUE
               )

    layer_info = sf::st_layers(outfile)    
    
    subset_divides   <- read_sf("subset_test.gpkg", layer = "divides")

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

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    TEST_ID = 19550502 

    get_subset(outfile = outfile, 
                id = TEST_ID,
                lyrs = c('network'),
                gpkg = gpkg,
                overwrite = TRUE
               )

    layer_info = sf::st_layers(outfile)    
    
    subset_network   <- read_sf("subset_test.gpkg", layer = "network")

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
    results in a subset of the flowpath and its 2 neighbors", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    # sf::st_layers(gpkg)
    
    TEST_ID = 1861420

    get_subset(outfile = outfile, 
                id = TEST_ID,
                lyrs = c('flowpaths'),
                gpkg = gpkg,
                overwrite = TRUE
               )

    layer_info = sf::st_layers(outfile)    

    subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowpaths")

    # mapview::mapview(dplyr::filter(subset_flowpaths, id != TEST_ID) , color = "red") +
    # mapview::mapview(dplyr::filter(subset_flowpaths, id == TEST_ID),color = "green")  
    
    # Flowlines
    testthat::expect_true("id" %in% names(subset_flowpaths))
    testthat::expect_true("vpuid" %in% names(subset_flowpaths))
    testthat::expect_true("mainstemlp" %in% names(subset_flowpaths))
    testthat::expect_true(TEST_ID %in% subset_flowpaths$id)

    expect_equal(nrow(subset_flowpaths), 3)
    expect_equal(unique(subset_flowpaths$vpuid), "06")
    expect_equal(min(subset_flowpaths$mainstemlp), 1354178)
    expect_true(all(st_geometry_type(subset_flowpaths) %in% c("MULTILINESTRING", "LINESTRING")))

    unlink(outfile)

})

test_that("Local conus_reference gpkg flowpaths check 1 'id' from each VPU and make sure dimensions are correct and contains input id", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    # # NOTE: Grabs 1 ID from each streamorder
    # streamorder_ids = sf::read_sf(gpkg, query = "
    #                     SELECT *
    #                     FROM (
    #                         SELECT *, ROW_NUMBER() OVER (PARTITION BY streamorde ORDER BY id) AS row_num
    #                         FROM network
    #                     ) subquery
    #                     WHERE row_num <= 1
    #                 ")
    # streamorder_ids$id %>% unique()  %>% paste0("'", ., "'", collapse = ", ")

    # NOTE: this is 1 ID from each stream order
    TEST_IDS = c('1861418', '1861424', '1861440', '1861570', '1862438', '1861600', '1861886')

    for (id in TEST_IDS) {
        
        message(id)

        get_subset(outfile = outfile, 
                    id = id,
                    lyrs = c('flowpaths'),
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

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")
    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    
    TEST_ID = 19550502 

    get_subset(outfile = outfile, 
                id = TEST_ID,
               lyrs = c('flowpaths'),
                gpkg = gpkg,
               overwrite = TRUE
               )

      testthat::expect_true(
            file.exists(outfile)
            )

    unlink(outfile)

    response_data = get_subset(
            id = TEST_ID,
            lyrs = c('flowpaths'),
            gpkg = gpkg,
            overwrite = TRUE
            ) 
    
    testthat::expect_false(
            file.exists(outfile)
            )
})

unlink(outfile)