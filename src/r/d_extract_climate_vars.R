if(!file.exists(file.path(extraction_dir, 'climate_megafires_event_statistics.csv'))) {
  climate_vars <- c('pr', 'rmin', 'rmax', 'tmmx', 'tmmn', 'vpd', 'th', 'vs', 'pet', 'fm100', 'fm1000')
  climate_extracted <- NULL
  climate_per_mtbs_id <- NULL
  
  for(i in unique(fire_event_extract$fire_id)) {
    fire_event <- fire_event_extract %>%
      filter(fire_id == i) %>%
      st_transform(laea)
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
      
      fire_event_buffer <- suppressWarnings(fire_event %>% st_transform(ll) %>%
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

        multiple_stats <- function(x, na.rm) c(mean = mean(x, na.rm=na.rm), min = min(x, na.rm=na.rm), max = max(x, na.rm=na.rm),
                                           sum = sum(x, na.rm=na.rm), sd = sd(x, na.rm=na.rm))

        mean_var <- paste0('mean_', j)
        min_var <- paste0('min_', j)
        max_var <- paste0('max_', j)
        sum_var <- paste0('sum_', j)
        sd_var <- paste0('sd_', j)
        
        climate_extract <- raster::extract(climate_stack, as(fire_event, 'Spatial'), 
                                           fun = multiple_stats, df = TRUE, small = TRUE) %>% 
          as_tibble() %>%
          add_rownames(., "stat") %>%
          dplyr::select(-ID) %>%
          gather(key = doy, value = !!ensym(j), -stat) %>%
          mutate(doy = gsub('X', '', doy),
                 doy = yday(as_date(as.integer(doy), origin = '1900-01-01'))) %>%
          spread(stat, !!ensym(j)) %>%
          dplyr::select(-doy) %>%
          summarize(!!ensym(mean_var) := mean(mean, na.rm = TRUE),
                    !!ensym(min_var) := min(min, na.rm = TRUE),
                    !!ensym(max_var) := max(max, na.rm = TRUE),
                    !!ensym(sum_var) := mean(sum, na.rm = TRUE),
                    !!ensym(sd_var) := mean(sd, na.rm = TRUE)) %>%
          mutate(fire_id = fire_event$fire_id)
        
      climate_extracted[[j]] <- as.data.frame(climate_extract) %>%
        mutate(fire_id = as.data.frame(fire_event)$fire_id) %>%
        as_tibble() 
    }
    climate_per_mtbs_id[[i]] <- bind_cols(climate_extracted)
  }
  climate_megafires_event_statistics <- bind_rows(climate_per_mtbs_id) %>%
    dplyr::select(-fire_id1, -fire_id2, -fire_id3, -fire_id4, -fire_id5, -fire_id6, -fire_id7, 
                  -fire_id8, -fire_id9, -fire_id10) %>%
    left_join(as.data.frame(fire_event_extract) %>% dplyr::select(-geom), ., by = 'fire_id')
  write_csv(climate_megafires_event_statistics, file.path(extraction_dir, 'climate_megafires_event_statistics.csv'))
  system(paste0('aws s3 sync ', extraction_dir, ' ', s3_proc_extractions))
  
} else {
  climate_megafires_event_statistics <- read_csv(file.path(extraction_dir, 'climate_megafires_event_statistics.csv'))
}
