if(!file.exists(file.path(extraction_dir, 'climate_megafires.csv'))) {
  climate_vars <- c('pr', 'rmin', 'rmax', 'tmmx', 'tmmn', 'vpd', 'th', 'vs', 'pet', 'fm100', 'fm1000')
  climate_extracted <- NULL
  climate_per_mtbs_id <- NULL
  
  for(i in unique(fire_event_extract$fire_id)) {
    
    fire_event <- fire_event_extract %>%
      filter(fire_id == i) %>%
      st_transform('+proj=longlat +lon_0=0 +a=6378137 +rf=298.257223563')
    fire_year <- fire_event$discovery_year
    first_burn_date <- fire_event$first_burndate
    last_burn_date <- fire_event$last_burndate
    
    for(j in climate_vars) {
      
      climate_files <- list.files(pattern = paste0(j, '_', fire_year, '.nc'), full.names = TRUE)
      
      var <- climate_files[[1]] %>%
        basename() %>%
        str_split('_') %>%
        unlist()
      var <- var[1]
      
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
      if(j == "pr") {
        climate_mean <- calc(climate_stack, fun = mean, na.rm = TRUE)
        climate_min <- calc(climate_stack, fun = min)
        climate_max <- calc(climate_stack, fun = max)
        climate_sum <- calc(climate_stack, fun = sum)
        climate_stats <- raster::stack(climate_mean, climate_min, climate_max, climate_sum)
        
        climate_extract <- velox(climate_stats)$extract(sp = fire_event, fun = function(x) max(x),
                                                        small = TRUE) %>% as_tibble() 
        
        # Rename to the raster layer names
        colnames(climate_extract) <- c(paste0('mean_', var), paste0('min_', var), paste0('max_', var), paste0('sum_', var))
      } else {
        climate_mean <- calc(climate_stack, fun = mean, na.rm = TRUE)
        climate_min <- calc(climate_stack, fun = min)
        climate_max <- calc(climate_stack, fun = max)
        climate_stats <- raster::stack(climate_mean, climate_min, climate_max)
        
        climate_extract <- velox(climate_stats)$extract(sp = fire_event, fun = function(x) max(x),
                                                        small = TRUE) %>% as_tibble() 
        
        # Rename to the raster layer names
        colnames(climate_extract) <- c(paste0('mean_', var), paste0('min_', var), paste0('max_', var))
      }
      climate_extracted[[j]] <- as.data.frame(climate_extract) %>%
        mutate(fire_id = as.data.frame(fire_event)$fire_id) %>%
        as_tibble() 
    }
    climate_per_mtbs_id[[i]] <- bind_cols(climate_extracted)
  }
  climate_megafires <- bind_rows(climate_per_mtbs_id) %>%
    dplyr::select(-fire_id1, -fire_id2, -fire_id3, -fire_id4, -fire_id5, -fire_id6, -fire_id7, 
                  -fire_id8, -fire_id9, -fire_id10) %>%
    left_join(as.data.frame(fire_event_extract) %>% dplyr::select(-geom), ., by = 'fire_id')
  write_csv(climate_megafires, file.path(extraction_dir, 'climate_megafires.csv'))
  system(paste0('aws s3 sync ', extraction_dir, ' ', s3_proc_extractions))
  
} else {
  climate_megafires <- read_csv(file.path(extraction_dir, 'climate_megafires.csv'))
}
