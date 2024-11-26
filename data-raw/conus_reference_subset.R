## code to prepare `conus_reference_subset` dataset goes here
conus_ref_gpkg = "/Volumes/T7SSD/lynker-spatial/hydrofabric/v3.0/reference-features/conus_reference.gpkg"
output_gpkg    = "tests/testthat/testdata/conus_reference_subset.gpkg"

# sf::st_layers(conus_ref_gpkg)

if(file.exists(output_gpkg))  {
    file.remove(output_gpkg)
}

VPU_ID = "06"
HUC6 = "060400"

network = 
    conus_ref_gpkg  %>% 
    sf::read_sf(query = paste0("SELECT * FROM network WHERE vpuid = '", VPU_ID, "'"))  %>% 
    dplyr::filter(grepl(HUC6, reachcode))

divides = 
    conus_ref_gpkg  %>% 
    sf::read_sf(query = paste0("SELECT * FROM divides WHERE vpuid = '", VPU_ID, "'")) 
    # rmapshaper::ms_simplify(keep = 0.10) 

divides = 
    divides  %>% 
    dplyr::filter(divide_id %in% network$divide_id)

flowpaths = 
    conus_ref_gpkg  %>% 
    sf::read_sf(query = paste0("SELECT * FROM flowpaths WHERE vpuid = '", VPU_ID, "'")) 

flowpaths =
    flowpaths  %>% 
    dplyr::filter(id %in% network$id)
    # .$geom  %>% plot()


sf::write_sf(divides, dsn = output_gpkg, layer = "divides", driver = "GPKG")
sf::write_sf(flowpaths, dsn = output_gpkg, layer = "flowpaths", driver = "GPKG", append = TRUE)
sf::write_sf(network, dsn = output_gpkg, layer = "network", driver = "GPKG", append = TRUE)

# usethis::use_data(conus_reference_subset, overwrite = TRUE)
