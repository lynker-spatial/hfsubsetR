# # Initialize constant variables
# source <- 's3://lynker-spatial/hydrofabric'
# #source <- '/Users/mjohnson/hydrofabric'
# type <- "reference"
# hf_version <- "2.2"
# lyrs = c('divides', 'flowlines', 'network')
# outfile <- 'subset_test.gpkg'
# library(sf)

# # --------------------- Reference fabric ---------------------------#

# # Test with an comid and network file 
# test_that("Terminal point check for comid passed", {

#     get_subset(outfile = outfile, 
#                comid = 718648, 
#                source = source, 
#                type = type, 
#                lyrs = lyrs,
#                hf_version = hf_version,
#                overwrite = TRUE)

#     subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowlines")
#     subset_divides   <- read_sf("subset_test.gpkg", layer = "divides")
#     subset_network   <- read_sf("subset_test.gpkg", layer = "network")
    
#     # Flowlines
#     expect_equal(nrow(subset_flowpaths), 5)
#     expect_equal(unique(subset_flowpaths$vpuid), "01")
#     expect_equal(min(subset_flowpaths$mainstemlp), 1923133)
#     expect_equal(max(subset_flowpaths$mainstemlp), 1923137) 

#     # Divides
#     expect_equal(nrow(subset_divides), 5)
#     expect_equal(unique(subset_divides$vpuid), "01")

#     # Network
#     expect_equal(nrow(subset_network), 6)
#     expect_equal(unique(subset_network$vpuid), "01")
    
#     unlink(outfile)
# })

# # Test with an id and network file 
# test_that("Terminal point check for id passed", {

#     get_subset(outfile = outfile, 
#                id = 923, 
#                source = source, 
#                type = type, 
#                lyrs = lyrs,
#                hf_version = hf_version,
#                overwrite = TRUE)

#     subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowlines")
#     subset_divides <- read_sf("subset_test.gpkg", layer = "divides")
#     subset_network <- read_sf("subset_test.gpkg", layer = "network")
    
#     # Flowlines
#     expect_equal(nrow(subset_flowpaths), 3)
#     expect_equal(unique(subset_flowpaths$vpuid), "01")


#     # Divides
#     expect_equal(nrow(subset_divides), 3)
#     expect_equal(unique(subset_divides$vpuid), "01")

#     # Network
#     expect_equal(nrow(subset_network), 3)
#     expect_equal(unique(subset_network$vpuid), "01")
 
#     unlink(outfile)
    
# })

# # Test with an nldi_feature
# test_that("Terminal point check for nldi_feature passed", {

#     get_subset(outfile = outfile, 
#                nldi_feature = list(featureSource = "nwissite", 
#                                    featureID = "USGS-08279500"), 
#                source = source, 
#                type = type,
#                lyrs = lyrs,
#                hf_version = hf_version,
#                overwrite = TRUE)

#     subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowlines")
#     subset_divides <- read_sf("subset_test.gpkg", layer = "divides")
#     subset_network <- read_sf("subset_test.gpkg", layer = "network")
    
#     # Flowlines
#     expect_equal(nrow(subset_flowpaths), 3375)
#     expect_equal(unique(subset_flowpaths$vpuid), "13")

#     # Divides
#     expect_equal(nrow(subset_divides), 3354)
#     expect_equal(unique(subset_divides$vpuid), "13")

#     # Network
#     expect_equal(nrow(subset_network), 3446)
#     expect_equal(unique(subset_network$vpuid), "13")
    
#     unlink(outfile)

# })

# # Test with an poi_id and network file 
# test_that("Terminal point check for poi_id passed", {

#     get_subset(outfile = outfile, 
#                poi_id = 111, 
#                source = source, 
#                type = type, 
#                lyrs = lyrs,
#                hf_version = hf_version,
#                overwrite = TRUE)

#     subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowlines")
#     subset_divides <- read_sf("subset_test.gpkg", layer = "divides")
#     subset_network <- read_sf("subset_test.gpkg", layer = "network")
    
#     # Flowlines
#     expect_equal(nrow(subset_flowpaths), 23)
#     expect_equal(unique(subset_flowpaths$vpuid), "01")

#     # Divides
#     expect_equal(nrow(subset_divides), 23)
#     expect_equal(unique(subset_divides$vpuid), "01")

#     # Network
#     expect_equal(nrow(subset_network), 23)
#     expect_equal(unique(subset_network$vpuid), "01")
    
#     unlink(outfile)
# })

# # Test with an hl_uri and network file 
# test_that("Terminal point check for hl_uri passed", {

#     get_subset(outfile = outfile, 
#                hl_uri = 'HUC12-010100070603', 
#                source = source, 
#                type = type, 
#                lyrs = lyrs,
#                hf_version = hf_version,
#                overwrite = TRUE)

#     subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowlines")
#     subset_divides <-  read_sf("subset_test.gpkg", layer = "divides")
#     subset_network <-  read_sf("subset_test.gpkg", layer = "network")
    
#     # Flowlines
#     expect_equal(nrow(subset_flowpaths), 130)
#     expect_equal(unique(subset_flowpaths$vpuid), "01")

#     # Divides
#     expect_equal(nrow(subset_divides), 127)
#     expect_equal(unique(subset_divides$vpuid), "01")

#     # Network
#     expect_equal(nrow(subset_network), 135)
#     expect_equal(unique(subset_network$vpuid), "01")
    
#     unlink(outfile)

# })

# unlink(outfile)