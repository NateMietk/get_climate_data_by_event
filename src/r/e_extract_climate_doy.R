
if(!file.exists(file.path(extraction_dir, 'doy_megafires.csv'))) {
  climate_vars <- c('pr', 'rmin', 'rmax', 'tmmx', 'tmmn', 'vpd', 'th', 'vs', 'pet', 'fm100', 'fm1000', 'fsr')
  climate_extracted <- NULL
  climate_per_mtbs_id <- NULL
  
  for(i in unique(fire_event_extract$fire_id)) {
    fire_event <- fire_event_extract %>%
      filter(fire_id == i) %>%
      st_transform(laea)
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
      min_var_doy <- paste0(j, '_min_doy')
      max_var_doy <- paste0(j, '_max_doy')
      fsr_var <- paste0('USA_BurnDate_', fire_year, '_fsr')
      
      if(j == 'fsr') {
        climate_files <- list.files(proc_dir_modis, pattern = paste0(fire_year, '_', j), full.names = TRUE)
        var <- climate_files[[1]] %>%
          basename() %>%
          str_split('\\.|_') %>%
          unlist()
        var <- var[4]
        
        fire_event_buffer <- suppressWarnings(fire_event %>%
                                                st_buffer(12000))
        
        climate_stack <- raster::stack(climate_files) %>%
          crop(fire_event_buffer) 
        
        climate_extract <- velox(climate_stack)$extract_points(sp = modis_pts) %>% as_tibble()
        colnames(climate_extract) <- names(climate_stack)
        
        climate_extracted[[j]] <- as.data.frame(climate_extract) %>%
          mutate(fire_id = as.data.frame(modis_pts)$fire_id,
                 !!ensym(max_var_doy) := as.data.frame(modis_pts)$doy) %>%
          as_tibble() %>%
          slice(!!ensym(fsr_var) == max(!!ensym(fsr_var), na.rm = TRUE)) %>%
          dplyr::select(fire_id, !!ensym(max_var_doy)) %>%
          distinct(fire_id, .keep_all = TRUE)
        
      } else {
        climate_files <- list.files(pattern = paste0(j, '_', fire_year), full.names = TRUE)
        var <- climate_files[[1]] %>%
          basename() %>%
          str_split('\\.|_') %>%
          unlist()
        var <- var[1]
        
        fire_event_buffer <- suppressWarnings(fire_event %>%
                                                st_transform(ll) %>%
                                                st_buffer(0.12500001))
        
        climate_stack <- raster::stack(climate_files) %>%
          crop(fire_event_buffer) 
        projection(climate_stack) <- ll
        
        # Set the date values and ranges
        start_date <- as.Date(paste(fire_year, "01", "01", sep = "/"))
        end_date <- as.Date(paste(fire_year, "12", "31", sep = "/"))
        idx = seq(as.Date(start_date), as.Date(end_date), by = "day")
        climate_stack = setZ(climate_stack, idx)
        
        # Subset the climate data so it falls within the days when the fire burned
        climate_stack = subset(climate_stack,
                               which(getZ(climate_stack) >= as.Date(first_burn_date, origin = start_date) &
                                       getZ(climate_stack) <= as.Date(last_burn_date, origin = start_date))) 
        
        climate_stack <- climate_stack %>%
          projectRaster(., crs = laea, res = 4000, method = 'bilinear') 
        
        climate_extract <- velox(climate_stack)$extract_points(sp = modis_pts) %>% as_tibble()
        colnames(climate_extract) <- names(climate_stack)
        
        if(j == 'pr' & j == 'rmin' & j == 'tmmn' & j == 'vpd' & j == 'pet' & j == 'fm100' & j == 'fm1000') {
          climate_extracted[[j]] <- as.data.frame(climate_extract) %>%
            mutate(fire_id = as.data.frame(modis_pts)$fire_id,
                   !!ensym(min_var_doy) := as.data.frame(modis_pts)$doy) %>%
            as_tibble() %>% 
            gather(key = day_long, value = !!ensym(j), - fire_id, -!!ensym(min_var_doy)) %>%
            mutate(day_long = gsub('X', '', day_long),
                   day_long = yday(as_date(as.integer(day_long), origin = '1900-01-01'))) %>%
            filter(!!ensym(min_var_doy) == day_long) %>%
            slice(which(!!ensym(j) == min(!!ensym(j), na.rm = TRUE))) %>%
            dplyr::select(fire_id, !!ensym(min_var_doy)) %>%
            distinct(fire_id, .keep_all = TRUE)
          
        } else {
          climate_extracted[[j]] <- as.data.frame(climate_extract) %>%
            mutate(fire_id = as.data.frame(modis_pts)$fire_id,
                   !!ensym(max_var_doy) := as.data.frame(modis_pts)$doy) %>%
            as_tibble() %>% 
            gather(key = day_long, value = !!ensym(j), - fire_id, -!!ensym(max_var_doy)) %>%
            mutate(day_long = gsub('X', '', day_long),
                   day_long = yday(as_date(as.integer(day_long), origin = '1900-01-01'))) %>%
            filter(!!ensym(max_var_doy) == day_long) %>%
            slice(which(!!ensym(j) == max(!!ensym(j), na.rm = TRUE))) %>%
            dplyr::select(fire_id, !!ensym(max_var_doy)) %>%
            distinct(fire_id, .keep_all = TRUE)
        }
      }
    }
    climate_per_mtbs_id[[i]] <- bind_cols(climate_extracted)
  }
  doy_megafires <- bind_rows(climate_per_mtbs_id) %>%
    dplyr::select(-fire_id1, -fire_id2, -fire_id3, -fire_id4, -fire_id5, -fire_id6, -fire_id7, 
                  -fire_id8, -fire_id9, -fire_id10, -fire_id11) 
  
  write_csv(climate_megafires, file.path(extraction_dir, 'doy_megafires.csv'))
  system(paste0('aws s3 sync ', extraction_dir, ' ', s3_proc_extractions, ' --delete'))
  
} else {
  doy_megafires <- read_csv(file.path(extraction_dir, 'doy_megafires.csv'))
}

megafire_df <- climate_megafires_event_statistics %>%
  left_join(., doy_megafires, by = 'fire_id')

write_csv(megafire_df, file.path(extraction_dir, 'climate_megafires.csv'))
system(paste0('aws s3 sync ', extraction_dir, ' ', s3_proc_extractions, ' --delete'))
