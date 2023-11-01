
save_chelsa_biovar_geotiff<- function(var, chelsa_dir) {
  timeout_value = getOption('timeout')
  options(timeout=360)  # set time to 5 minutes
  # maybe add command to create chelsadir if it doesn't exist?
  
  chelsa_bio_var_url<- 'https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/'
  chelsa_filename <- paste0('CHELSA_', var, '_1981-2010_V.2.1.tif')
  chelsa_url <- paste0(chelsa_bio_var_url, chelsa_filename)
  local_filename = file.path(chelsa_dir,chelsa_filename)
  if (file.exists(local_filename)){
    warning('chelsa file exists, not re-downloading')
  } else {
    result<- utils::download.file(url=chelsa_url, destfile=local_filename)
  }
  
  # optional - check result to see if it actually worked
  
  # return the timeout option to original 
  options(timeout = timeout_value)
  
  return(local_filename) 
  
}


read_chelsa_geotiff<-function(var, chelsa_dir){
  chelsa_filename <- paste0('CHELSA_', var, '_1981-2010_V.2.1.tif')
  local_filename = file.path(chelsa_dir,chelsa_filename)
  if ( ! file.exists(local_filename) ) { 
    cmdstr <- paste0('save_chelsa_geotiff(',var,',',chelsa_dir,')')
    warning(paste("can't find chelsa file, download with ",cmdstr))
    return( NA)
  }
  
  # load raster into memory
  chelsa_raster <- terra::rast(local_file)
  return(chelsa_raster)
  
}


get_chelsa_value<- function(chelsa_raster,lat, lon){
  coord <- terra::vect(cbind(lon,lat), crs="EPSG:4326")
  chelsa_value <- terra::extract(bio10, coord)
  
}

#' test chelsa functions above 
#' 
#' 
test_chelsa <- function(climatevar = 'bio10') {
  
  chelsa_dir <- file.path(Sys.getenv('HOME'), 'tmp')
  chelsa_file <- save_chelsa_biovar_geotiff(climatevar, chelsa_dir)
  chelsa_raster <- read_chelsa_geotiff(climatevar, chelsa_file)
  lat=42.729660
  lon=-84.481534
  x <- get_chelsa_value(chelsa_raster, lat, lon)
  return(x)

}