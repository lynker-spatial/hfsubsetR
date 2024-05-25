#' Extract VPU GPKG from Lynker-Spatial
#' Data from a Parquet Dataset will be extracted for a VPU. 
#' Optionally this data can be written to a GPKG.
#' @param vpu VPU ID
#' @inheritParams get_subset
#' @return path or list 
#' @export

get_vpu_fabric = function(vpu = "01", 
                          type = "reference",
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
  
  vpuid <- NULL
  
  fl = open_dataset(glue('{source}/v{hf_version}/{type}/conus_flowlines/')) %>% 
    filter(vpuid == vpu) %>% 
    read_sf_dataset() %>% 
    st_set_crs(5070)
  
  div = open_dataset(glue('{source}/v{hf_version}/{type}/conus_divides/')) %>% 
    filter(vpuid == vpu) %>% 
    read_sf_dataset() %>% 
    st_set_crs(5070)
  
  
    
    open_dataset("s3://lynker-spatial/hydrofabric/v2.2/reference/conus_divides/") %>% 
    filter(vpuid == "10L") %>% 
    read_sf_dataset() %>% 
    st_set_crs(5070)
  
  net = open_dataset(glue('{source}/v{hf_version}/{type}/conus_network/')) %>% 
    filter(vpuid == vpu) %>% 
    collect() 
  
  if(is.null(outfile)){
    list(divides = div,
         flowpaths = fl,
         network = net)
  } else { 
    write_sf(fl, outfile,  "flowpaths")
    write_sf(div, outfile, "divides")
    write_sf(net, outfile, "network")
    return(outfile)
  }
}
