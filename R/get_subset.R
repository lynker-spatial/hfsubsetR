na.omit = function(x){ x[!is.na(x)] }

#' Extract Data from Arrow Stores
#' @inheritParams get_subset
#' @param hook a local or s3 hydrofabric direc
#' @return list or file path
#' @export
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

#' Find Origin from ID
#' @param network 
#' @inheritParams get_subset
#' @return data.frame
#' @export

findOrigin = function(network, 
                      id = NULL, 
                      comid = NULL,  
                      hl_uri = NULL, 
                      poi_id = NULL, 
                      nldi_feature = NULL, 
                      xy = NULL) {
  
  if(!is.null(xy)) {
    xy[1:2] <- as.numeric(xy[1:2])
    comid   <- discover_nhdplus_id(point = st_sfc(st_point(c(xy[1], xy[2])), crs = 4326))
  }
  
  # nldi_feature = list(featureSource = "nwissite", featureID = "USGS-08279500")
  if (!is.null(nldi_feature)) {
    comid <- discover_nhdplus_id(nldi_feature = nldi_feature)
  }
  
  con = open_dataset(network)
  #poi_id = 74719
  if (!is.null(poi_id)) {
    obj <- filter(con, poi_id == !!poi_id)
  }
  
  #hl_uri = 'WBIn-120049871'
  if (!is.null(hl_uri)) {
    obj <- filter(con, hl_uri == !!hl_uri) 
  }
  
  #comid = 101
  if (!is.null(comid)) {
    obj <- filter(con, hf_id == comid) 
  }
  
  if (!is.null(id)) {
    obj <- filter(con, id == !!id)
  }
  
  origin = select(obj, id, vpuid, topo) %>%
    distinct() %>% 
    collect()
  
  if (nrow(origin) == 0) {
    stop("No origin found")
  } else if (nrow(origin) > 1) {
    print(origin)
    stop("Multiple Origins Found")
  } else {
    return(origin)
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
#' @param lyrs layers to extract
#' @param domain hydrofabric domain
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
                      source = "s3://lynker-spatial/hydrofabric",
                      hf_version = "2.2", 
                      type = "reference",
                      domain = "conus",
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

  hook      <- glue("{source}/v{hf_version}/{type}/{domain}")
  net_hook  <- glue("{hook}_network")
  
  origin = findOrigin(net_hook,
                      id = id, 
                      comid = comid,  
                      hl_uri = hl_uri, 
                      poi_id = poi_id, 
                      nldi_feature = nldi_feature, 
                      xy = xy)
  
  net = open_dataset(net_hook) %>% 
    filter(vpuid == origin$vpuid) %>% 
    select(id, toid) %>% 
    distinct() %>% 
    collect()
  
  subset = suppressWarnings({ get_sorted(net, outlets = origin$id) })
  
  if(origin$topo == "fl-fl"){
    all_ids = na.omit(unique(c(subset$id, subset$toid[-nrow(subset)])))
  } else {
    all_ids = na.omit(unique(c(subset$id, subset$toid)))
  }
  
  extract_data(hook = hook, 
               vpu = origin$vpuid, 
               ids = all_ids,
               lyrs = lyrs, 
               outfile = outfile)  
  
}

