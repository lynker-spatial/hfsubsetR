library(dplyr)
library(arrow)
library(sf)
library(testthat)

# devtools::load_all()

test_that("find_origin for a single ID", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")

    # Input values: 
    id = 19541984 
    comid = NULL
    hl_uri = NULL
    poi_id = NULL
    nldi_feature = NULL
    xy = NULL
    outfile = NULL
    lyrs = c("flowpaths")

    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    kv <- .dispatch_identifiers(id, comid, hl_uri, poi_id, nldi_feature, xy)
  
    if (!is.null(gpkg)) {
        src <- query_source_sf(gpkg)
    } else {
        uri <- paste(source, paste0("v", hf_version), type, domain, sep = "/")
        src <- query_source_arrow(uri)
    }
    query <-
        query() |>
        query_set_id(identifier = kv$value, type = kv$type) |>
        query_set_layers(layers = lyrs) |>
        query_set_source(src)
    
    if (!is.null(outfile)) {
        query <- query_set_sink(query, sink = outfile, overwrite = overwrite)
    }

    identifier <- query_get_id(query)
  
    origin <- find_origin(
        network = query_source_layer(query$source, "network"),
        id = identifier,
        type = class(identifier)
    ) 

    testthat::expect_true(
        nrow(origin) == 1
    )
    
    testthat::expect_equal(
        origin$id,
        19541984
    )

    testthat::expect_equal(
        origin$topo,
        'fl-fl'
    )

    testthat::expect_equal(
        origin$hydroseq,
        1404202
    )
})


test_that("find_origin for a single hf_uri", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")

    # Input values: 
    id = NULL 
    comid = NULL
    hl_uri = "huc12-060400010704"
    poi_id = NULL
    nldi_feature = NULL
    xy = NULL
    outfile = NULL
    lyrs = c("flowpaths")

    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    kv <- .dispatch_identifiers(id, comid, hl_uri, poi_id, nldi_feature, xy)
  
    if (!is.null(gpkg)) {
        src <- query_source_sf(gpkg)
    } else {
        uri <- paste(source, paste0("v", hf_version), type, domain, sep = "/")
        src <- query_source_arrow(uri)
    }

    query <-
        query() |>
        query_set_id(identifier = kv$value, type = kv$type) |>
        # query_set_layers(layers = lyrs) |>
        query_set_source(src)
    
    if (!is.null(outfile)) {
        query <- query_set_sink(query, sink = outfile, overwrite = overwrite)
    }

    identifier <- query_get_id(query)
  
    origin <- find_origin(
        network = query_source_layer(query$source, "network"),
        id = identifier,
        type = class(identifier)
    ) 

    testthat::expect_true(
        nrow(origin) == 1
    )
    
    testthat::expect_equal(
        origin$toid,
        19558132
    )

    testthat::expect_equal(
        origin$topo,
        'fl-fl'
    )

    testthat::expect_equal(
        origin$hydroseq,
        1356216
    )
})


test_that("find_origin gives error on non existent ID", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")

    # Input values: 
    id = 99999999999999 
    comid = NULL
    hl_uri = NULL
    poi_id = NULL
    nldi_feature = NULL
    xy = NULL
    outfile = NULL
    lyrs = c("flowpaths")

    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    kv <- .dispatch_identifiers(id, comid, hl_uri, poi_id, nldi_feature, xy)
  
    if (!is.null(gpkg)) {
        src <- query_source_sf(gpkg)
    } else {
        uri <- paste(source, paste0("v", hf_version), type, domain, sep = "/")
        src <- query_source_arrow(uri)
    }

    query <-
        query() |>
        query_set_id(identifier = kv$value, type = kv$type) |>
        # query_set_layers(layers = lyrs) |>
        query_set_source(src)
    
    if (!is.null(outfile)) {
        query <- query_set_sink(query, sink = outfile, overwrite = overwrite)
    }

    identifier <- query_get_id(query)
    
    testthat::expect_error(
        origin <- find_origin(
            network = query_source_layer(query$source, "network"),
            id = identifier,
            type = class(identifier)
        ),
        "No origin found"
    )

})

test_that("find_origin gives error on non existent hf_uri", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")

    # Input values: 
    id = NULL 
    comid = NULL
    hl_uri = "a-fake-uri"
    poi_id = NULL
    nldi_feature = NULL
    xy = NULL
    outfile = NULL
    lyrs = c("flowpaths")

    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    kv <- .dispatch_identifiers(id, comid, hl_uri, poi_id, nldi_feature, xy)
  
    if (!is.null(gpkg)) {
        src <- query_source_sf(gpkg)
    } else {
        uri <- paste(source, paste0("v", hf_version), type, domain, sep = "/")
        src <- query_source_arrow(uri)
    }

    query <-
        query() |>
        query_set_id(identifier = kv$value, type = kv$type) |>
        # query_set_layers(layers = lyrs) |>
        query_set_source(src)
    
    if (!is.null(outfile)) {
        query <- query_set_sink(query, sink = outfile, overwrite = overwrite)
    }

    identifier <- query_get_id(query)
    
    testthat::expect_error(
        origin <- find_origin(
            network = query_source_layer(query$source, "network"),
            id = identifier,
            type = class(identifier)
        ),
        "No origin found"
    )

})

test_that("find_origin on COMID", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")

    # Input values: 
    id = NULL 
    comid = 19541968
    hl_uri = NULL
    poi_id = NULL
    nldi_feature = NULL
    xy = NULL
    outfile = NULL
    lyrs = c("flowpaths")

    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    kv <- .dispatch_identifiers(id, comid, hl_uri, poi_id, nldi_feature, xy)
  
    if (!is.null(gpkg)) {
        src <- query_source_sf(gpkg)
    } else {
        uri <- paste(source, paste0("v", hf_version), type, domain, sep = "/")
        src <- query_source_arrow(uri)
    }

    query <-
        query() |>
        query_set_id(identifier = kv$value, type = kv$type) |>
        # query_set_layers(layers = lyrs) |>
        query_set_source(src)
    
    if (!is.null(outfile)) {
        query <- query_set_sink(query, sink = outfile, overwrite = overwrite)
    }

    identifier <- query_get_id(query)

    origin <- find_origin(
        network = query_source_layer(query$source, "network"),
        id = identifier,
        type = class(identifier)
    )


    testthat::expect_true(
        nrow(origin) == 1
    )

    testthat::expect_equal(
        origin$id,
        19541968
    )

    testthat::expect_equal(
        origin$toid,
        19541946
    )

    testthat::expect_equal(
        origin$topo,
        'fl-fl'
    )

    testthat::expect_equal(
        origin$hydroseq,
        1355615
    )



})

test_that("find_origin on XY coord", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")

    # Input values: 
    id = NULL 
    comid = NULL
    hl_uri = NULL
    poi_id = NULL
    nldi_feature = NULL
    xy = NULL
    xy = c(-88.1971, 35.38836)
    outfile = NULL
    lyrs = c("flowpaths")

    # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
    kv <- .dispatch_identifiers(id, comid, hl_uri, poi_id, nldi_feature, xy)
  
    if (!is.null(gpkg)) {
        src <- query_source_sf(gpkg)
    } else {
        uri <- paste(source, paste0("v", hf_version), type, domain, sep = "/")
        src <- query_source_arrow(uri)
    }

    query <-
        query() |>
        query_set_id(identifier = kv$value, type = kv$type) |>
        # query_set_layers(layers = lyrs) |>
        query_set_source(src)
    
    if (!is.null(outfile)) {
        query <- query_set_sink(query, sink = outfile, overwrite = overwrite)
    }

    identifier <- query_get_id(query)

    origin <- find_origin(
        network = query_source_layer(query$source, "network"),
        id = identifier,
        type = class(identifier)
    )


    testthat::expect_true(
        nrow(origin) == 1
    )

    testthat::expect_equal(
        origin$id,
        19550296
    )

    testthat::expect_equal(
        origin$toid,
        19550502
    )

    testthat::expect_equal(
        origin$topo,
        'fl-fl'
    )

    testthat::expect_equal(
        origin$vpuid,
        "06"
    )

    testthat::expect_equal(
        origin$hydroseq,
        1403112
    )



})

test_that("find_origin on all 'id' in test conus_reference_subset.gpkg", {

    gpkg = testthat::test_path("testdata", "conus_reference_subset.gpkg")

    ALL_IDS = 
        gpkg  %>% 
        sf::read_sf(query = "SELECT id FROM flowpaths")  %>% 
        dplyr::distinct() %>%
        dplyr::pull()

    # unique(ALL_IDS$id)

    # Input values: 
    for (id in ALL_IDS) {
        
        message(id)
        # id = NULL 

        comid = NULL
        hl_uri = NULL
        poi_id = NULL
        nldi_feature = NULL
        xy = NULL
        outfile = NULL
        lyrs = c("flowpaths")

        # gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
        kv <- .dispatch_identifiers(id, comid, hl_uri, poi_id, nldi_feature, xy)
    
        if (!is.null(gpkg)) {
            src <- query_source_sf(gpkg)
        } else {
            uri <- paste(source, paste0("v", hf_version), type, domain, sep = "/")
            src <- query_source_arrow(uri)
        }

        query <-
            query() |>
            query_set_id(identifier = kv$value, type = kv$type) |>
            query_set_layers(layers = lyrs) |>
            query_set_source(src)
        
        if (!is.null(outfile)) {
            query <- query_set_sink(query, sink = outfile, overwrite = overwrite)
        }

        identifier <- query_get_id(query)

        origin <- find_origin(
            network = query_source_layer(query$source, "network"),
            id = identifier,
            type = class(identifier)
        )


        testthat::expect_true(
            nrow(origin) == 1
        )

        testthat::expect_equal(
            origin$id,
            id
        )
        
        testthat::expect_equal(
            origin$topo,
            'fl-fl'
        )

    }



})