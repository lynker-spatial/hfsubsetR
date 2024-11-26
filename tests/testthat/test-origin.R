
# # library(dplyr)
# # library(arrow)
# # library(sf)
# # library(testthat)

# # devtools::load_all()

# #x network  <- '/Users/mjohnson/hydrofabric/v2.2/reference/conus_network'
# network  <- 's3://lynker-spatial/hydrofabric/v2.2/reference/conus_network'

# test =  arrow %>% 
#   filter(hl_uri == "gages-06752260") %>% 
#   collect()


# test_that("Test ID", {
#   ido = findOrigin(network, id = test$id)
#   expect_equal(ido$id, 2899997)
#   expect_equal(ido$vpuid, '10L')
# })

# test_that("Test COMID", {
#   idc = findOrigin(network, comid = test$hf_id)
#   expect_equal(idc$id, 2899997)
#   expect_equal(idc$vpuid, '10L')
# })

# test_that("Test hl", {
#   idhl = findOrigin(network, hl_uri = test$hl_uri)
#   expect_equal(idhl$id, 2899997)
#   expect_equal(idhl$vpuid, '10L')
# })

# test_that("Test poi", {
#   idpoi = findOrigin(network, poi_id = test$poi_id)
#   expect_equal(idpoi$id, 2899997)
#   expect_equal(idpoi$vpuid, '10L')
# })

# test_that("Test nldi", {
#   nldi = findOrigin(network, 
#                     nldi_feature = list(featureSource = "nwissite", 
#                                         featureID = "USGS-06752260"))
#   expect_equal(nldi$id, 2900003)
#   expect_equal(nldi$vpuid, '10L')
# })

# test_that("Test XY", {
#   xy = c( -105.0492, 40.58193)
#   xyid = findOrigin(network, xy = xy)
  
#   expect_equal(xyid$id, 2899997)
#   expect_equal(xyid$vpuid, '10L')
# })



# #x network  <- '/Users/mjohnson/hydrofabric/v2.2/reference/conus_network'
# gpkg  <- conus_gpkg

# test =  as_sqlite(gpkg, "network") %>% 
#   filter(hl_uri == "gages-06752260") %>% 
#   collect()

# test_that("Test ID", {
#   ido = findOriginGPKG(gpkg, id = test$id)
#   expect_equal(ido$id, 'wb-1569690')
#   expect_equal(ido$vpuid, '10L')
# })

# test_that("Test COMID", {
#   idc = findOriginGPKG(gpkg, comid = test$hf_id)
#   expect_equal(idc$id, 'wb-1569690')
#   expect_equal(idc$vpuid, '10L')
# })

# test_that("Test hl", {
#   idhl = findOriginGPKG(gpkg, hl_uri = test$hl_uri)
#   expect_equal(idhl$id, 'wb-1569690')
#   expect_equal(idhl$vpuid, '10L')
# })

# test_that("findOriginGPKG poi", {
#   idpoi = findOriginGPKG(gpkg, poi_id = test$poi_id)
#   expect_equal(idpoi$id, 'wb-1569690')
#   expect_equal(idpoi$vpuid, '10L')
# })

# test_that("Test nldi", {
#   nldi = findOriginGPKG(gpkg, 
#                     nldi_feature = list(featureSource = "nwissite", 
#                                         featureID = "USGS-06752260"))
#   expect_equal(nldi$id, 'wb-1569690')
#   expect_equal(nldi$vpuid, '10L')
# })

# test_that("Test XY", {
#   xy = c( -105.0492, 40.58193)
#   xyid = findOriginGPKG(gpkg, xy = xy)
  
#   expect_equal(xyid$id, 'wb-1569691')
#   expect_equal(xyid$vpuid, '10L')
# })
