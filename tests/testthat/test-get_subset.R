# Initialize constant variables
source <- 's3://lynker-spatial/hydrofabric'
#source <- '/Users/mjohnson/hydrofabric'
type <- "reference"
hf_version <- "2.2"
lyrs = c('divides', 'flowlines', 'network')
outfile <- 'subset_test.gpkg'
library(sf)

network  <- 's3://lynker-spatial/hydrofabric/v2.2/reference/conus_network'

test =  open_dataset(network) %>% 
    filter(hl_uri == "Gages-06752260") %>% 
    collect()

# --------------------- Reference fabric ---------------------------#

# Test with an comid and network file 
test_that("Terminal point check for comid passed", {

    get_subset(outfile = outfile, 
               comid = 2899997, 
               source = source, 
               type = type, 
               lyrs = lyrs,
               hf_version = hf_version,
               overwrite = TRUE)

    subset_flowpaths <- read_sf("subset_test.gpkg", layer = "flowlines")
    subset_divides   <- read_sf("subset_test.gpkg", layer = "divides")
    subset_network   <- read_sf("subset_test.gpkg", layer = "network")
    
    # Flowlines
    expect_equal(nrow(subset_flowpaths), 1129)
    expect_equal(unique(subset_flowpaths$vpuid), "10L")
    expect_equal(min(subset_flowpaths$mainstemlp), 1113198)


    # Divides
    expect_equal(nrow(subset_divides), 1129)
    expect_equal(unique(subset_divides$vpuid), "10L")

    # Network
    expect_equal(nrow(subset_network), 1145)
    expect_equal(unique(subset_network$vpuid), "10L")
    
    unlink(outfile)
})
