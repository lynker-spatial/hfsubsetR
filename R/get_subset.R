na.omit = function(x){ x[!is.na(x)] }

extract_data = function(hook, vpu, ids, lyrs, outfile = NULL){
  
  hydrofabric = list()
  
  for(i in 1:length(lyrs)){
    dd = tryCatch({
      open_dataset(glue("{hook}_{lyrs[i]}")) 
    }, error = function(e){
      message(glue("{lyrs[i]} not found"))
      NULL
    })
    
    if(!is.null(dd)){
      dd = filter(dd, vpuid == vpu) %>% 
        filter(if_any(any_of(
          c('COMID',  'FEATUREID', 'divide_id', 'id', 'toid', "ID")
        ), ~ . %in% !!ids))
      
      if(any(c("geom", "geomtry") %in% names(dd))){
        tmp = read_sf_dataset(dd)
      } else {
        tmp = collect(dd)
      }
      
      if (!is.null(outfile)) {
        write_sf(tmp, outfile, lyrs[i])
      } else {
        hydrofabric[[lyrs[i]]] = t
      }
    }
  }
  
  if (!is.null(outfile)) {
   return(outfile) 
  } else {
    return(hydrofabric)
  }
}

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
#' @param outfile If gpkg file path is provided, data will be written to a file.
#' @param overwrite overwrite existing outfile file path. Default is FALSE
#' @export
#' @importFrom glue glue
#' @importFrom nhdplusTools discover_nhdplus_id get_sorted
#' @importFrom arrow open_dataset
#' @importFrom dplyr select filter collect `%>%` everything if_any any_of distinct
#' @importFrom sf st_set_crs write_sf st_sfc st_point st_bbox

get_subset = function(id = NULL, 
                      comid = NULL,  
                      hl_uri = NULL, 
                      poi_id = NULL, 
                      nldi_feature = NULL, 
                      xy = NULL, 
                      lyrs = c('cross_sections',
                               'divides',
                               'flowlines',
                               'flowpath-attributes',
                               'forcing-weights',
                               'hydrolocations',
                               'model-attributes',
                               'network',
                               'nexus'),
                      type = "reference",
                      domain = "conus",
                      hf_version = "2.2", 
                      source = "s3://lynker-spatial/hydrofabric",
                      outfile = NULL, 
                      overwrite = FALSE) {
  
  if (!is.null(outfile)) {
    if (file.exists(outfile) & overwrite) {
      unlink(outfile)
    } else if (file.exists(outfile)) {
      warning(glue("{outfile} already exists and overwrite is FALSE"))
      return(outfile)
    }
  }

  hf_id <-  vpuid <- 
    topo <- hl_reference <- 
    hl_link <- hl_source <- 
    toid <- divide_id <- NULL
  
  dim(net)
  dim(collect(open_dataset(net_hook)))
  
  hook      <- glue("{source}/v{hf_version}/{type}/{domain}")
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
      select(hl_source, comid = hf_id, poi_id, vpuid) %>% 
      collect()
    
    origin <- open_dataset(net_hook) %>% 
      filter(vpuid == hl$vpuid[1], hf_id == hl$comid[1]) %>% 
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
    select(id, toid) %>% 
    distinct() %>% 
    collect()
  
  subset = suppressWarnings({
    nhdplusTools::get_sorted(net, outlets = origin$id)
  })
  
  if(origin$topo == "fl-fl"){
    all_ids = na.omit(unique(c(subset$id, subset$toid[-nrow(subset)], subset$divide_id)))
  } else {
    all_ids = na.omit(unique(c(subset$id, subset$toid, subset$divide_id)))
  }
  
  extract_data(hook = hook, 
               vpu = origin$vpuid, 
               ids = ids,
               lyrs = lyrs, 
               outfile = outfile)  
  
}

