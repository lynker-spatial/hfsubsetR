#x network  <- '/Users/mjohnson/hydrofabric/v2.2/reference/conus_network'
network  <- 's3://lynker-spatial/hydrofabric/v2.2/reference/conus_network'

test =  open_dataset(network) %>% 
  filter(hl_uri == "Gages-06752260") %>% 
  collect()

test_that("Test ID", {
  ido = findOrigin(network, id = test$id)
  expect_equal(ido$id, 2899997)
  expect_equal(ido$vpuid, '10L')
})

test_that("Test COMID", {
  idc = findOrigin(network, comid = test$hf_id)
  expect_equal(idc$id, 2899997)
  expect_equal(idc$vpuid, '10L')
})

test_that("Test hl", {
  idhl = findOrigin(network, hl_uri = test$hl_uri)
  expect_equal(idhl$id, 2899997)
  expect_equal(idhl$vpuid, '10L')
})

test_that("Test poi", {
  idpoi = findOrigin(network, poi_id = test$poi_id)
  expect_equal(idpoi$id, 2899997)
  expect_equal(idpoi$vpuid, '10L')
})

test_that("Test nldi", {
  nldi = findOrigin(network, 
                    nldi_feature = list(featureSource = "nwissite", 
                                        featureID = "USGS-06752260"))
  expect_equal(nldi$id, 2900003)
  expect_equal(nldi$vpuid, '10L')
})

test_that("Test XY", {
  xy = c( -105.0492, 40.58193)
  xyid = findOrigin(network, xy = xy)
  
  expect_equal(xyid$id, 2899997)
  expect_equal(xyid$vpuid, '10L')
})

