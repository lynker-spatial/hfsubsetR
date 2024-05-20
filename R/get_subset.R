#' @title Build a hydrofabric subset
#' @param id hydrofabric id. datatype: string / vector of strings e.g., 'wb-10026' or c('wb-10026', 'wb-10355') 
#' @param comid NHDPlusV2 COMID. datatype: int / vector of int e.g., 61297116 or c(61297116 , 6129261) 
#' @param hl_uri hydrolocation URI. datatype: string / vector of string / a url e.g., HUC12-010100100101 or c(HUC12-010100100101 , HUC12-010100110104) 
#' @param poi_id POI identifier. datatype: int / vector of int e.g., 266387 or c(266387, 266745)
#' @param nldi_feature list with names 'featureSource' and 'featureID' where 'featureSource' is derived from the "source" column of the response of dataRetrieval::get_nldi_sources() and the 'featureID' is a known identifier from the specified 'featureSource'. datatype: a url e.g., 'https://labs.waterdata.usgs.gov/api/nldi/linked-data/census2020-nhdpv2'
#' @param xy Location given as vector of XY in EPSG:4326 (longitude, latitude, crs)
#' @param type hydrofabric type
#' @param hf_version hydrofabric version
#' @param source hydrofabric source (local root directory or s3 link)
#' @param outfile Data will be written to a file if supplied. 
#' @param outfile If gpkg file path is provided, data will be written to a file.
#' @export
#' @importFrom glue glue
#' @importFrom nhdplusTools discover_nhdplus_id get_sorted
#' @importFrom arrow open_dataset
#' @importFrom dplyr select filter collect `%>%` everything
#' @importFrom sf st_set_crs write_sf st_sfc st_point st_bbox

get_subset = function(id = NULL, 
                      comid = NULL,  
                      hl_uri = NULL, 
                      poi_id = NULL, 
                      nldi_feature = NULL, 
                      xy = NULL, 
                      type = "reference",
                      hf_version = "2.2", 
                      source = "s3://lynker-spatial/hydrofabric",
                      outfile = NULL) {
  
  hf_id <-  vpuid <- 
    topo <- hl_reference <- 
    hl_link <- hl_source <- 
    toid <- divide_id <- NULL
  
  hook      <- glue("{source}/v{hf_version}/{type}/conus")
  net_hook  <- glue("{hook}_network")
  hl_hook   <- glue("{source}/v{hf_version}/conus_hl")
  
  #xy = c(-105.0825, 40.58897)
  if(!is.null(xy)) {
    xy[1:2] <- as.numeric(xy[1:2])
    comid   <- discover_nhdplus_id(point = st_sfc(st_point(c(xy[1], xy[2])), crs = 4326))
    origin  <- open_dataset(net_hook) %>% 
      filter(hf_id == comid) %>% 
      select(id, vpuid, topo) %>% 
      collect()
  }
  
  # nldi_feature = list(featureSource = "nwissite", 
  #                     featureID = "USGS-08279500")
  if(!is.null(nldi_feature)){
    comid <- discover_nhdplus_id(nldi_feature = nldi_feature)
    origin <- open_dataset(net_hook) %>% 
      filter(hf_id == comid) %>% 
      select(id, vpuid, topo) %>% 
      collect()
  }
  
  #poi_id = 74719
  if(!is.null(poi_id)){
    hl <- open_dataset(hl_hook) %>% 
      filter(poi_id == !!poi_id) %>% 
      select(comid = hf_id, poi_id, vpuid) %>% 
      collect()
    
    origin <- open_dataset(net_hook) %>% 
      filter(vpuid == hl$vpuid, hf_id == hl$comid) %>% 
      select(id, vpuid, topo) %>% 
      collect()
  }
  
  #hl_uri = 'WBIn-120049871'
  if(!is.null(hl_uri)){
    
    input <- strsplit(hl_uri, "-")[[1]]
    
    hl <-  open_dataset(hl_hook) %>% 
      filter(hl_reference == input[1], hl_link == input[2]) %>% 
      select(comid = hf_id, poi_id, vpuid, hl_source, hl_reference, hl_link) %>%
      collect()
    
    origin <- open_dataset(net_hook) %>% 
      filter(vpuid == hl$vpuid, hf_id == hl$comid) %>% 
      select(id, vpuid, topo) %>% 
      collect()
  }
  
  #comid = 101
  if(!is.null(comid)){
    origin <- open_dataset(net_hook) %>% 
      filter(hf_id == comid) %>% 
      select(id, vpuid, topo) %>% 
      collect()
  }
  
  if(!is.null(id)){
    origin <- open_dataset(net_hook) %>% 
      filter(id == !!id) %>% 
      select(id, vpuid, topo) %>% 
      collect()
  }
  
  if(nrow(origin) == 0){
    stop("No origin found")
  } else if(nrow(origin) > 1){
    print(origin)
    stop("Multiple Origins Found")
  } 
  
  net = open_dataset(net_hook) %>% 
    filter(vpuid == origin$vpuid) %>% 
    select(id, toid, divide_id, everything()) %>% 
    collect()
  
  subset = suppressWarnings({
    nhdplusTools::get_sorted(net, outlets = origin$id)
  })
  
  if(origin$topo == "fl-fl"){
    fl_ids = unique(c(subset$id, subset$toid[-nrow(subset)]))
  } else {
    fl_ids = unique(c(subset$id, subset$toid))
  }
  
  fl = open_dataset(glue("{hook}_flowlines")) %>% 
    filter(vpuid == origin$vpuid) %>% 
    filter(id %in% fl_ids) %>% 
    read_sf_dataset() %>% 
    st_set_crs(5070)
  
  div = open_dataset(glue("{hook}_divides")) %>% 
    filter(vpuid == origin$vpuid) %>% 
    filter(divide_id %in% unique(subset$divide_id)) %>% 
    read_sf_dataset() %>% 
    st_set_crs(5070)
  
  if(is.null(outfile)){
    list(divides = div,
         flowpaths = fl,
         network = subset)
  } else {
    write_sf(fl, outfile, "flowpaths")
    write_sf(div, outfile, "divides")
    write_sf(net, outfile, "network")
    return(outfile)
  }
}

