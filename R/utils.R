#' @title Get VPU Fabric
#' @description Retrieve and Process Vector Processing Unit (VPU) Hydrofabric Layers
#'
#' This function retrieves and optionally filters spatial data layers from a GeoPackage (GPKG) based on a specified 
#' Vector Processing Unit ID (VPU ID). The function can either return the filtered layers as a list or write them to an output file.
#'
#' @param gpkg A string specifying the path to the GeoPackage file.
#' @param vpuid A vector of VPU IDs to filter the layers. If `NULL`, no filtering is applied. Default is `NULL`.
#' @param outfile A string specifying the path to write the filtered layers to a new GeoPackage. If `NULL`, the layers 
#' are returned as a list. Default is `NULL`.
#'
#' @return If `outfile` is `NULL`, returns a list where each element is a filtered spatial layer (`sf` object). 
#' If `outfile` is provided, returns the path to the output GeoPackage.
#'
#' @details The function reads all layers from the provided GeoPackage, excluding the "error" layer. For each layer, 
#' the data is optionally filtered by the provided `vpuid` and then processed into `sf` objects. 
#' If an output file path is provided, the filtered layers are written to a new GeoPackage. Otherwise, 
#' the layers are stored in a list and returned.
#'
#' @examples
#' \dontrun{
#' # Example 1: Retrieve filtered layers as a list
#' fabric <- get_vpu_fabric("path/to/geopackage.gpkg", vpuid = c("01", "02"))
#'
#' # Example 2: Write filtered layers to a new GeoPackage
#' get_vpu_fabric("path/to/geopackage.gpkg", vpuid = c("01", "02"), outfile = "output.gpkg")
#' }
#' @export

get_vpu_fabric = function(gpkg, vpuid = NULL, outfile = NULL){
  
  lyrs <- sf::st_layers(gpkg)$name
  lyrs <- lyrs[lyrs != "error"]
  
  fabric = list()
  
  for(i in lyrs){
    layer_data = dplyr::filter(as_ogr(gpkg, i), vpuid %in% !!vpuid) |> 
      st_as_sf()
    
    if (!is.null(outfile)) {
      sf::write_sf(layer_data, outfile, i)
    } else {
      fabric[[i]] = layer_data
    }
  }

  if (!is.null(outfile)) {
    return(outfile)
  } else {
    return(fabric)
  }
}

#' @title Download a Hydrofabric Geopackage
#'
#' @description Downloads a hydrofabric Geopackage from a specified URL and saves it to a local file.
#'
#' @param url A character string specifying the base URL of the hydrofabric repository. Defaults to `'https://lynker-spatial.s3-us-west-2.amazonaws.com/hydrofabric'`.
#' @param version A character string indicating the version of the hydrofabric to download. Defaults to `'2.2'`.
#' @param domain A character string specifying the geographic domain of the hydrofabric. Defaults to `'conus'`.
#' @param type A character string indicating the type of hydrofabric. Defaults to `'nextgen'`.
#' @param outfile A character string specifying the path to save the downloaded file. If `NULL`, the file will not be saved. Defaults to `NULL`.
#' @param overwrite A logical value indicating whether to overwrite an existing file. Defaults to `FALSE`.
#' @return The function returns the path to the downloaded file (`outfile`).
#'
#' @examples
#' \dontrun{
#' # Download the default hydrofabric file
#' get_hydrofabric(outfile = "conus_nextgen.gpkg")
#'
#' # Specify a different domain and version
#' get_hydrofabric(
#'   version = "3.0",
#'   domain = "hawaii",
#'   outfile = "hawaii_nextgen.gpkg",
#'   overwrite = TRUE
#' )
#' }
#' @export

get_hydrofabric = function(url = 'https://lynker-spatial.s3-us-west-2.amazonaws.com/hydrofabric',
                           version = '2.2',
                           domain = 'conus',
                           type = 'nextgen',
                           outfile = NULL,
                           overwrite = FALSE){
  
  httr::GET(glue('{url}/v{version}/{domain}/{domain}_{type}.gpkg'), 
            httr::write_disk(outfile, overwrite = !!overwrite), 
            httr::progress())
  
  return(outfile)

}