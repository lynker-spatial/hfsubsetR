na.omit = function(x){ x[!is.na(x)] }

#' Extract Data from Arrow Stores
#' @param hook a local or s3 hydrofabric directory
#' @param vpu an optional VPU to quicken search
#' @param ids all identifiers to extract
#' @param lyrs hydrofabric layers to subset
#' @param outfile a path to write resulting geopackage
#' @return list or file path
#' @export

extract_arrow_data = function(hook, vpu, ids, lyrs, outfile = NULL){
  
  vpuid <- poi_id <-  NULL
  hydrofabric = list()
  
  for(i in 1:length(lyrs)){
    dd = tryCatch({
      open_dataset(glue("{hook}_{lyrs[i]}")) 
    }, error = function(e){
      message(glue("{lyrs[i]} not found"))
      NULL
    })
    
    if(!is.null(dd)){
 
      dd   <- dd |>
        filter(vpuid == vpu)
      
      vars <- c('COMID',  'FEATUREID', 'divide_id', "link", "to", 'id', 'toid', "ID", "poi_id")
      
      if ("poi_id" %in% names(dd)) {
        dd = dd %>%
          dplyr::mutate(poi_id = as.character(poi_id)) |>
          dplyr::filter(if_any(any_of(!!vars), ~ . %in% !!ids))
      } else {
        dd = dd %>%
          filter(if_any(any_of(!!vars), ~ . %in% !!ids))
      }

      
      if(any(c("geom", "geomtry") %in% names(dd))){
        tmp = read_sf_dataset(dd)
      } else {
        tmp = collect(dd)
      }
      
      if (!is.null(outfile)) {
        write_sf(tmp, outfile, lyrs[i])
      } else {
        hydrofabric[[lyrs[i]]] = tmp
      }
    }
  }
  
  if (!is.null(outfile)) {
   return(outfile) 
  } else {
    return(hydrofabric)
  }
}


#' Extract Data from Arrow Stores
#' @param gpkg a local gpkg file
#' @param vpu an optional VPU to quicken search
#' @param ids all identifiers to extract
#' @param lyrs hydrofabric layers to subset
#' @param outfile a path to write resulting geopackage
#' @return list or file path
#' @export

extract_gpkg_data = function(gpkg, vpu, ids, lyrs, outfile = NULL){
  
  vpuid <- poi_id <-  NULL
  hydrofabric = list()
  
  for(i in 1:length(lyrs)){
    dd = tryCatch({
      as_sqlite(gpkg, lyrs[i])
    }, error = function(e){
      message(glue("{lyrs[i]} not found"))
      NULL
    })
    
    if(!is.null(dd)){
      
      dd   <- dd |>
        filter(vpuid == vpu)
      
      vars <- c('COMID',  'FEATUREID', 'divide_id', "link", "to", 'id', 'toid', "ID", "poi_id")
      
      if ("poi_id" %in% colnames(dd)) {
        dd = dd %>%
          dplyr::mutate(poi_id = as.character(poi_id)) |>
          dplyr::filter(if_any(any_of(!!vars), ~ . %in% !!ids))
      } else {
        dd = dd %>%
          filter(if_any(any_of(!!vars), ~ . %in% !!ids))
      }
      
      
      if(any(c("geom", "geomtry") %in% colnames(dd))){
        tmp = read_sf_dataset_sqlite(dd)
      } else {
        tmp = collect(dd)
      }
      
      if (!is.null(outfile)) {
        write_sf(tmp, outfile, lyrs[i])
      } else {
        hydrofabric[[lyrs[i]]] = tmp
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
#' @param gpkg a local gpkg file
#' @param outfile If gpkg file path is provided, data will be written to a file.
#' @param lyrs layers to extract
#' @param domain hydrofabric domain
#' @param overwrite overwrite existing outfile file path. Default is FALSE
#' @export
#' @importFrom glue glue
#' @importFrom nhdplusTools discover_nhdplus_id get_sorted
#' @importFrom arrow open_dataset
#' @importFrom dplyr select filter collect `%>%` everything if_any any_of distinct rename
#' @importFrom sf st_set_crs write_sf st_sfc st_point st_bbox

get_subset = function(id           = NULL, 
                      comid        = NULL,  
                      hl_uri       = NULL, 
                      poi_id       = NULL, 
                      nldi_feature = NULL, 
                      xy           = NULL, 
                      lyrs = c('divides',
                               'flowpaths',
                               'network',
                               'nexus'),
                      gpkg = NULL,
                      source = "s3://lynker-spatial/hydrofabric",
                      hf_version = "2.2", 
                      type = "nextgen",
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
  
  hf_id <- vpuid <-  NULL
  hook      <- glue("{source}/v{hf_version}/{type}/{domain}")

  if(!is.null(gpkg)){
    
    origin <- findOriginGPKG(gpkg,
                            id = id, 
                            comid = comid,  
                            hl_uri = hl_uri, 
                            poi_id = poi_id, 
                            nldi_feature = nldi_feature, 
                            xy = xy)
    
    net <- as_sqlite(gpkg, "network") %>% 
      filter(vpuid == origin$vpuid) %>% 
      select(any_of(c('id', 'toid', 'divide_id', "poi_id"))) %>% 
      distinct() %>% 
      collect()
    
  } else {
    
    origin <- findOrigin(network = glue("{hook}_network"),
                        id = id, 
                        comid = comid,  
                        hl_uri = hl_uri, 
                        poi_id = poi_id, 
                        nldi_feature = nldi_feature, 
                        xy = xy)
  
    net = open_dataset(glue("{hook}_network")) %>% 
      filter(vpuid == origin$vpuid) %>% 
      select(any_of(c('id', 'toid', 'divide_id', "poi_id"))) %>% 
      distinct() %>% 
      collect()
    
  }

  subset <- suppressWarnings({ get_sorted(net, outlets = origin$toid) })

  subset$toid[nrow(subset)] <- NA
  
  all_ids <- na.omit(unique(as.vector(as.matrix(subset))))
  
  if(!is.null(gpkg)){
    extract_gpkg_data(gpkg    = gpkg, 
                      vpu     = origin$vpuid, 
                      ids     = all_ids,
                      lyrs    = lyrs, 
                      outfile = outfile) 
  } else {
    extract_arrow_data(hook = hook, 
                       vpu = origin$vpuid, 
                       ids = all_ids,
                       lyrs = lyrs, 
                       outfile = outfile) 
  }
  
}

