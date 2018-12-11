# load all ibraries
packages <- c("raster", "ncdf4", "tidyverse", "sf", "lubridate", 'velox', 'assertthat')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  # automatically installs packages if not found
  install.packages(setdiff(packages, rownames(installed.packages())))  
  # loads the library once installed
  lapply(packages, library, character.only = TRUE, quietly = TRUE) 
} else {
  # if package is already install this loads it
  lapply(packages, library, character.only = TRUE, quietly = TRUE) 
}

# load all functions
file_sources <- list.files(file.path('src', 'functions'), pattern="*.R", 
                           full.names=TRUE, ignore.case=TRUE)
invisible(sapply(file_sources, source, .GlobalEnv))

# key projections
p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"   #http://spatialreference.org/ref/sr-org/6903/

# define the amount of cores st_par runs on
ncor <- parallel::detectCores()

# create main directories
data_dir <- ("data")
extraction_dir <- file.path(data_dir, "extractions")
processed_dir <- file.path(data_dir, 'processed')

# create main raw folder and all subfolders to hold raw/unprocessed data
raw_dir <- file.path(data_dir, "raw")
raw_dir_us <- file.path(raw_dir, "cb_2016_us_state_20m")
raw_dir_mtbs <- file.path(raw_dir, "mtbs")

# create processed directories
proc_dir_mtbs <- file.path(processed_dir, 'mtbs')
proc_dir_modis <- file.path(processed_dir, 'modis')

# for pushing and pulling to s3 using the system function
s3_base <- 's3://earthlab-natem/get_climate_data_megafires/'
s3_proc_prefix <- 's3://earthlab-natem/get_climate_data_megafires/processed/'
s3_proc_extractions <- 's3://earthlab-natem/get_climate_data_megafires/extractions/'
s3_proc_climate <- 's3://earthlab-natem/get_climate_data_megafires/fire/'

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(data_dir, raw_dir, extraction_dir, processed_dir, raw_dir_us, raw_dir_mtbs,
                proc_dir_mtbs, proc_dir_modis)

lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))
