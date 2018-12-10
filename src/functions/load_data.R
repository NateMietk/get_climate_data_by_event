
load_data <- function(url, dir, layer) {
  file <- paste0(dir, "/", layer, ".shp")
  
  if (!file.exists(file)) {
    download.file(url, destfile = paste0(dir, ".zip"))
    unzip(paste0(dir, ".zip"),
          exdir = dir)
    unlink(paste0(dir, ".zip"))
  }
  out_shp <- sf::st_read(dsn = dir, layer = layer)
  return(out_shp)
}
