# Now let's download the MODIS burned area data from my dropbox
fire_event_extracted <- NULL

if(!file.exists(file.path(extraction_dir, 'megafires_modis_extract.gpkg'))) {

  for(i in unique(megafires$fire_id)) {
    
    fire_event <- megafires %>%
      filter(fire_id == i)
    fire_year <- fire_event$discovery_year
    
    modis_files <- list.files(proc_dir_modis, pattern = paste0(fire_year), full.names = TRUE)
    modis_stack <- raster::stack(modis_files) %>%
      crop(fire_event)
    
    modis_extract <- velox(modis_stack)$extract(sp = fire_event, fun = function(x) max(x),
                                                small = TRUE) %>%
      as_tibble() 
    # Rename to the raster layer names
    colnames(modis_extract) <- c('first_burndate', 'fsr', 'last_burndate')
    
    fire_event_extracted[[i]] <- as.data.frame(modis_extract) %>%
      mutate(fire_id = as.data.frame(fire_event)$fire_id) %>%
      as_tibble() %>%
      dplyr::select(fire_id, first_burndate, last_burndate, fsr)
  }
  fire_event_extract <- do.call("rbind", fire_event_extracted) %>%
    left_join(megafires, ., by = 'fire_id') %>%
    st_sf(.) %>%
    mutate(first_burndate = case_when(discovery_doy <= first_burndate ~ discovery_doy,
                                      first_burndate <= discovery_doy ~ first_burndate))
  st_write(fire_event_extract, file.path(extraction_dir, 'megafires_modis_extract.gpkg'))
  } else {
    fire_event_extract <- st_read(file.path(extraction_dir, 'megafires_modis_extract.gpkg'))
  }