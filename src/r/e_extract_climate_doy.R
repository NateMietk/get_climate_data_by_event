
if(!file.exists(file.path(extraction_dir, 'climate_megafires.csv'))) {
  climate_vars <- c('pr', 'rmin', 'rmax', 'tmmx', 'tmmn', 'vpd', 'th', 'vs', 'pet', 'fm100', 'fm1000', 'fsr')
  climate_extracted <- NULL
  climate_per_mtbs_id <- NULL
  
  for(i in unique(fire_event_extract$fire_id)) {

    fire_event <- fire_event_extract %>%
      filter(fire_id == i) %>%
      st_transform('+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs')
    fire_year <- fire_event$discovery_year
    first_burn_date <- fire_event$first_burndate
    last_burn_date <- fire_event$last_burndate
    
    modis_files <- list.files(proc_dir_modis, pattern = paste0('USA_BurnDate_', fire_year, '.tif'), full.names = TRUE)
    modis_pts <- raster::stack(modis_files) %>%
      crop(fire_event) %>%
      mask(fire_event) %>%
      rasterToPoints(., spatial = TRUE) %>%
      st_as_sf(.) %>%
      mutate(fire_id = i)
    names(modis_pts) <- c('doy', 'fire_id', 'geometry')
    
    for(j in climate_vars) {
      if(j == 'fsr') {
        climate_files <- list.files(proc_dir_modis, pattern = paste0(fire_year, '_', j), full.names = TRUE)
        
        var <- climate_files[[1]] %>%
          basename() %>%
          str_split('\\.|_') %>%
          unlist()
        var <- var[4]
        
        fire_event <- fire_event %>%
          st_transform('+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs')
        
        # Import the netcdf files and crop to wildfire extent
        climate_stack <- raster::stack(climate_files) 
        proj4string(climate_stack) = CRS('+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs')
        climate_stack <- climate_stack %>%
          crop(fire_event) 
        
        modis_pts <- modis_pts %>%
          st_transform('+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs')
        
        climate_extract <- velox(raster::stack(climate_stack))$extract_points(sp = modis_pts) %>% as_tibble() 
        colnames(climate_extract) <- j
        
        col_name <- paste0('doy_', j)
        
        climate_extracted[[j]] <- as.data.frame(climate_extract) %>%
          mutate(fire_id = as.data.frame(modis_pts)$fire_id,
                 doy = as.data.frame(modis_pts)$doy) %>%
          as_tibble() %>% 
          mutate(!!ensym(col_name) := doy) %>%
          slice(which.max(!!ensym(j))) %>%
          dplyr::select(fire_id, !!ensym(col_name))
      } else {
        climate_files <- list.files(pattern = paste0(j, '_', fire_year, '.nc'), full.names = TRUE)
      
        var <- climate_files[[1]] %>%
          basename() %>%
          str_split('_') %>%
          unlist()
        var <- var[1]
        
        fire_event <- fire_event %>%
          st_transform('+proj=longlat +lon_0=0 +a=6378137 +rf=298.257223563')
        
        # Import the netcdf files and crop to wildfire extent
        climate_stack <- raster::stack(climate_files) 
        proj4string(climate_stack) = CRS('+proj=longlat +lon_0=0 +a=6378137 +rf=298.257223563')
        climate_stack <- climate_stack %>%
          crop(fire_event)
      
        # Set the date values and ranges
        start_date <- as.Date(paste(fire_year, "01", "01", sep = "/"))
        end_date <- as.Date(paste(fire_year, "12", "31", sep = "/"))
        idx = seq(as.Date(start_date), as.Date(end_date), by = "day")
        climate_stack = setZ(climate_stack, idx)
        
        # Subset the climate data so it falls within the days when the fire burned
        climate_stack = subset(climate_stack,
                               which(getZ(climate_stack) >= as.Date(first_burn_date, origin = start_date) &
                                       getZ(climate_stack) <= as.Date(last_burn_date, origin = start_date)))
        
        modis_pts <- modis_pts %>%
          st_transform('+proj=longlat +lon_0=0 +a=6378137 +rf=298.257223563')
        
        climate_extract <- velox(raster::stack(climate_stack))$extract_points(sp = modis_pts) %>% as_tibble() 
        colnames(climate_extract) <- c(paste0('x', seq(first_burn_date, last_burn_date)))
        
        col_name <- paste0('doy_', j)
        if(j == 'rmin' & j == 'tmmn' & j == 'vpd' & j == 'pet' & j == 'fm100' & j == 'fm1000') {
          climate_extracted[[j]] <- as.data.frame(climate_extract) %>%
            mutate(fire_id = as.data.frame(modis_pts)$fire_id) %>%
            as_tibble() %>% 
            gather(key, value, -fire_id) %>%
            mutate(key = as.factor(key)) %>%
            group_by(fire_id, key) %>%
            summarise(!!ensym(j) := min(value, na.rm = TRUE)) %>%
            mutate(!!ensym(col_name) := as.integer(stringr::str_replace(key, 'x', ''))) %>%
            slice(which.min(!!ensym(col_name))) %>%
            dplyr::select(fire_id, !!ensym(col_name))
        }  else {
          climate_extracted[[j]] <- as.data.frame(climate_extract) %>%
            mutate(fire_id = as.data.frame(modis_pts)$fire_id) %>%
            as_tibble() %>% 
            gather(key, value, -fire_id) %>%
            mutate(key = as.factor(key)) %>%
            group_by(fire_id, key) %>%
            summarise(!!ensym(j) := max(value, na.rm = TRUE)) %>%
            mutate(!!ensym(col_name) := as.integer(stringr::str_replace(key, 'x', ''))) %>%
            slice(which.max(!!ensym(col_name))) %>%
            dplyr::select(fire_id, !!ensym(col_name))
          }
      }
    }
    climate_per_mtbs_id[[i]] <- bind_cols(climate_extracted)
  }
  climate_megafires_doy <- bind_rows(climate_per_mtbs_id) %>%
    dplyr::select(-fire_id1, -fire_id2, -fire_id3, -fire_id4, -fire_id5, -fire_id6, -fire_id7, 
                  -fire_id8, -fire_id9, -fire_id10, -fire_id11) %>%
    left_join(as.data.frame(climate_megafires_event_statistics), ., by = 'fire_id')
  
  write_csv(climate_megafires_doy, file.path(extraction_dir, 'climate_megafires.csv'))
  system(paste0('aws s3 sync ', extraction_dir, ' ', s3_proc_extractions, ' --delete'))
  
} else {
  climate_megafires_doy <- read_csv(file.path(extraction_dir, 'climate_megafires.csv'))
}
