# Run the bash script to download all netcdf files for the climate variable.  Takes a couple days
if(length(list.files(pattern = '.nc')) != 426) {
  system('bash ./src/bash/metdata_wget.sh')
} else {
  print('Already downloaded')
}

# Download and import CONUS states
# Download will only happen once as long as the file exists
if (!exists("states")){
  states <- load_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                      dir = raw_dir_us,
                      layer = "cb_2016_us_state_20m") %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  states$STUSPS <- droplevels(states$STUSPS)
}

# Import and clean the MTBS polygons
if(!file.exists(file.path(proc_dir_mtbs, 'mtbs.gpkg'))) {
  
  mtbs_shp <- file.path(raw_dir_mtbs, 'mtbs_perims_DD.shp')
  if (!file.exists(mtbs_shp)) {
    dest <- paste0(raw_dir_mtbs, ".zip")
    download.file("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip", dest)
    unzip(dest, exdir = raw_dir_mtbs)
    unlink(dest)
    assert_that(file.exists(mtbs_shp))
  }
  
  mtbs <- st_read(dsn = raw_dir_mtbs, layer = 'mtbs_perims_DD', quiet= TRUE) %>%
    st_transform(p4string_ea) %>%
    mutate(discovery_date = ymd(paste(Year, StartMonth, StartDay, sep="-")),
           discovery_year = year(discovery_date),
           discovery_day = day(discovery_date),
           discovery_month = month(discovery_date),
           discovery_doy = yday(discovery_date)) %>%
    st_intersection(., st_union(states)) %>%
    rename_all(tolower) %>%
    dplyr::select(fire_id, fire_name, discovery_date, discovery_year, 
                  discovery_day, discovery_month, discovery_doy, acres)
  st_write(mtbs, file.path(proc_dir_mtbs, 'mtbs.gpkg'))
  } else {
    mtbs <- st_read(file.path(proc_dir_mtbs, 'mtbs.gpkg'))
    }

  # Create the Megafire extract from the MTBS database
if(!file.exists(file.path(proc_dir_mtbs, 'megafires.gpkg'))) {
  megafires <- read_csv(file.path(proc_dir_mtbs, 'mefi_MTBS_ids.csv')) %>%
    mutate(fire_id = MTBS_id) %>%
    dplyr::select(fire_id) %>%
    left_join(., mtbs, by = 'fire_id') %>%
    st_sf(.)
  st_write(megafires, file.path(proc_dir_mtbs, 'megafires.gpkg'))

  } else {
    megafires <- st_read(file.path(proc_dir_mtbs, 'megafires.gpkg'))
  }
