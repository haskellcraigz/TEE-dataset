#########################################################
# Batch processing daily temp min, mean, and max 1979-2021
# Last Updated: Dec 13 2024
############################################################
## NOTES: 

# ---------------------------------------------------------------------------- #
# Set-up ----------------------------------------------------------------------
# ---------------------------------------------------------------------------- #

# --- set working directory
# set working directory to location of TEEFiles folder
# setwd()

# --- libraries

# raster packages
library(terra)
library(exactextractr)

# shapefile packages 
library(sf)

# df packages
library(tidyverse)

# --- shapefiles
# download nuts files located in data/01_rawdata/nuts/:
spdf <- st_read("")

# --- initalize values

# filepaths
base_path <- "~/data/01_rawdata/dailytemp/"
export_nuts2 <- "~/data/02_metrics/dailytemp_nuts2.csv"
export_nuts3 <- "~/data/02_metrics/dailytemp_nuts3.csv"

# year
startdate <- "1979-01-01"
enddate <- "2020-12-31"

# ---------------------------------------------------------------------------- #
# Step 1: Subset Shapefile Regions -------------------------------------------
# ---------------------------------------------------------------------------- #

# getting NUTS2 and NUTS3 polygons
nuts2.shp = spdf %>% 
  filter(LEVL_CODE == 2)

nuts3.shp = spdf %>% 
  filter(LEVL_CODE == 3)

# computing area of polygons
nuts2.shp$area <- st_area(nuts2.shp)

nuts3.shp$area <- st_area(nuts3.shp)

# ---------------------------------------------------------------------------- #
# Step 2: Raster processing (spatial means) -----------------------------------
# ---------------------------------------------------------------------------- #

# ---- Step 2a: Raster processing at NUTS 2 level

# ---- initalizing loop values

# load files into list and create empty list for processed files
nc_files.lst <- list.files(path = base_path, pattern = "\\.nc$", full.names = TRUE)
process_nuts2_files.lst <- list()

# index
i <- 1

# ---- begin loop taking spatial mean for each file

require(foreign)
library(lubridate)

# check that files loaded
keep
if (length(nc_files.lst) == 0) {
  cat("No NetCDF files found in: ", base_path, "\n")
  next
}

for (nc_file in nc_files.lst) {
  
  # loading .nc file
  raster <- rast(nc_file)
  
  basename(nc_file)
  
  # Create a sequence of dates for entire file period to feed to raster data
  dates <- seq(as.Date(startdate, "%Y-%m-%d"), as.Date(enddate, "%Y-%m-%d"), by = "day")
  
  # add the dates 
  names(raster) <- dates
  
  # matching crs
  spdf_crs <- st_transform(nuts2.shp, st_crs(raster))
  
  # Generate a unique filename for each .nc file's output
  nc_file_name <- basename(nc_file)
  new_file_name <- paste0(substr(nc_file_name, 1, 4), ".csv")
  
  # spatial mean
  extracted <- exact_extract(raster, spdf_crs, 'mean', 
                             append_cols = 'NUTS_ID',  progress = TRUE, default_value = NA_real_)
  
  # reshaping data
  (
    temp_long <- pivot_longer(
      extracted,  
      -c(NUTS_ID), 
      names_to = "date",
      values_to = paste0("m_", new_file_name)
    )  
  )
  
  temp_long <- temp_long %>%
    mutate(date = str_replace(date, "extracted\\.", ""))
  
  process_nuts2_files.lst[[i]] <- temp_long
  
  i <- i+1
  
  cat("Processed and saved data for: ", new_file_name, "\n")
  
  
}  

# combining datasets

nuts2_temp.df <- left_join(process_nuts2_files.lst[[1]], process_nuts2_files.lst[[2]],
                           by=c("NUTS_ID", "date"))

for (i in 3:length(process_nuts2_files.lst)){
  
  nuts2_temp.df <- left_join(nuts2_temp.df, process_nuts2_files.lst[[i]],
                             by=c("NUTS_ID", "date"))
  
}


# renaming temperature variables
nuts2_temp.df = nuts2_temp.df %>% 
  rename(temp_mean = m_mean.csv,
         temp_max = m_max_.csv,
         temp_min = m_min_.csv)

nuts2_temp.df = nuts2_temp.df %>% 
  mutate(date = str_replace_all(date, "mean.", ""))

# exporting
write.csv(nuts2_temp.df, export_nuts2)

# ---- Step 2b: Raster processing at NUTS 3 level

# ---- initalizing loop values

# load files into list and create empty list for processed files
nc_files.lst <- list.files(path = base_path, pattern = "\\.nc$", full.names = TRUE)
process_nuts3_files.lst <- list()

# index
i <- 1

# ---- begin loop taking spatial mean for each file

require(foreign)
library(lubridate)

# if (length(nc_files.lst) == 0) {
#   cat("No NetCDF files found in: ", base_path, "\n")
#   next
# }

for (nc_file in nc_files.lst) {
  
  # loading .nc file
  raster <- rast(nc_file)
  
  basename(nc_file)
  
  # Create a sequence of dates for entire file period to feed to raster data
  dates <- seq(as.Date(startdate, "%Y-%m-%d"), as.Date(enddate, "%Y-%m-%d"), by = "day")
  
  # add the dates 
  names(raster) <- dates
  
  # matching crs
  spdf_crs <- st_transform(nuts3.shp, st_crs(raster))
  
  # Generate a unique filename for each .nc file's output
  nc_file_name <- basename(nc_file)
  new_file_name <- paste0(substr(nc_file_name, 1, 3), ".csv")
  
  # spatial mean
  extracted <- exact_extract(raster, spdf_crs, 'mean', 
                             append_cols = 'NUTS_ID',  progress = TRUE, default_value = NA_real_)
  
  # reshaping data
  (
    temp_long <- pivot_longer(
      extracted,  
      -c(NUTS_ID), 
      names_to = "date",
      values_to = paste0("m_", new_file_name)
    )  
  )
  
  temp_long <- temp_long %>%
    mutate(date = str_replace(date, "extracted\\.", ""))
  
  process_nuts3_files.lst[[i]] <- temp_long
  
  i <- i+1
  
  cat("Processed and saved data for: ", new_file_name, "\n")
  
  
}  

# combining datasets

nuts3_temp.df <- left_join(process_nuts3_files.lst[[1]], process_nuts3_files.lst[[2]],
                           by=c("NUTS_ID", "date"))

for (i in 3:length(process_nuts3_files.lst)){
  
  nuts3_temp.df <- left_join(nuts3_temp.df, process_nuts3_files.lst[[i]],
                             by=c("NUTS_ID", "date"))
  
}


# renaming temperature variables
nuts3_temp.df = nuts3_temp.df %>% 
  rename(temp_mean = m_mea.csv,
         temp_max = m_max.csv,
         temp_min = m_min.csv)

nuts3_temp.df = nuts3_temp.df %>% 
  mutate(date = str_replace_all(date, "mean.", ""))

# exporting
write.csv(nuts3_temp.df, export_nuts3)

