# Initialize constant variables
outfile <- 'subset_test.gpkg'
library(sf)

# --------------------- Reference fabric ---------------------------#

# Test with an comid and network file 
test_that("Terminal point check for comid passed", {

    get_subset(outfile = outfile, 
               comid = 2899997, 
               source = 's3://lynker-spatial/hydrofabric', 
               type = "reference", 
               lyrs = c('divides', 'flowlines', 'network'),
               hf_version = "2.2",
               overwrite = TRUE)
    
    st_layers(outfile)

    subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowlines")
    subset_divides   <- read_sf("subset_test.gpkg", layer = "divides")
    subset_network   <- read_sf("subset_test.gpkg", layer = "network")
    
    # Flowlines
    expect_equal(nrow(subset_flowpaths), 1129)
    expect_equal(unique(subset_flowpaths$vpuid), "10L")
    expect_equal(min(subset_flowpaths$mainstemlp), 1113198)
    expect_true(all(st_geometry_type(subset_flowpaths) %in% c("MULTILINESTRING", "LINESTRING")))

    # Divides
    expect_equal(nrow(subset_divides), 1122)
    expect_equal(unique(subset_divides$vpuid), "10L")
    expect_true(all(st_geometry_type(subset_divides) == "POLYGON"))

    # Network
    expect_equal(nrow(subset_network), 1145)
    expect_equal(unique(subset_network$vpuid), "10L")
    
    unlink(outfile)
})

unlink(outfile)


