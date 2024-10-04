#' @title Read gpkg as SQLite
#' @param gpkg path
#' @param lyr layer of GPKG. If NULL (default the database tables are printed)
#' @param ignore pattern for layers to be ignored description
#' @returns an S4 object that inherits from DBIConnection. This object is used to communicate with the database engine.
#' @importFrom DBI dbConnect dbListTables dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl
#' @export 

as_sqlite = function(gpkg, lyr= NULL, ignore = "gpkg_|rtree_|sqlite_"){
  
  db = dbConnect(RSQLite::SQLite(), gpkg)
  
  if(is.null(lyr)){
    tbls = dbListTables(db)
    tbls = tbls[!grepl(ignore, tbls)]
    print(tbls)
    dbDisconnect(db)
  } else {
    if(lyr %in% dbListTables(db)){
      db = tbl(db, lyr)
    } else {
      stop(lyr, ' not in gpkg.', call. = FALSE)
      dbDisconnect(db)
    }
  }
  db
}

#' @title Extract spatial data from an SQLite connection
#' @param tbl  remote temporary table
#' @returns an sf object (or data.frame is non spatial)
#' @importFrom DBI dbConnect dbListTables dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl collect slice_tail
#' @importFrom sf st_as_sf
#' @export 

read_sf_dataset_sqlite = function(tbl){
  
  con =  dbConnect(RSQLite::SQLite(), tbl$src$con@dbname)
  srs = con %>% 
    tbl("gpkg_spatial_ref_sys") %>% 
    collect() %>% 
    slice_tail(n = 1)
  
  dbDisconnect(con)
  
  d = collect(tbl)
  
  if(any(c("geom", "geometry") %in% names(d))){
    d = st_as_sf(d, crs = srs$definition)
  } else {
    warning("no simple features geometry column present")
    d
  }
  
  return(d)
}