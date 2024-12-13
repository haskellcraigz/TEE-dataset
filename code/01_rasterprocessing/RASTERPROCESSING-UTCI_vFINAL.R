############################################
## Batch processing daily UTCI min, mean, and max 1979-2021
## Last Updated: Dec 13 2024
#################################################
## NOTES: 

# ---------------------------------------------------------------------------- #
## Set-up ----------------------------------------------------------------------
# ---------------------------------------------------------------------------- #

# --- set working directory
# set working directory to location of TEEFiles folder
# setwd()

# -- libraries
# raster packages
library(terra)
library(exactextractr)

# shapefile packages 
library(sf)

# df packages
library(tidyverse)
require(foreign)
library(lubridate)

# -- setwd to code folder
# setwd("")


# -- load shapefile
# download nuts files located in data/01_rawdata/nuts/:
spdf <- st_read("")

# -- load raster data
nc_files.lst <- list.files(path = "~/data/rawdata/utci/", 
                           pattern = "\\.nc$", full.names = TRUE)


# -- initializing values 

# time frame
startdate <- "1979-01-01"
enddate <- "2020-12-31"

# filepaths
export_nuts2 <- "~/data/02_metrics/dailyutci_nuts2.csv"
export_nuts3 <- "~/data/02_metrics/dailyutci_nuts3.csv"

# ---------------------------------------------------------------------------- #
## Step 1: Subset Shapefile Regions -------------------------------------------
# ---------------------------------------------------------------------------- #

# NUTS2 region
nuts2.shp = spdf %>% 
  filter(LEVL_CODE == 2)

# NUTS3 region
nuts3.shp = spdf %>% 
  filter(LEVL_CODE == 3)

# ---------------------------------------------------------------------------- #
## Step 2: Raster processing (spatial means) -----------------------------------
# ---------------------------------------------------------------------------- #

## Step 2a: init lists to save output of loop --------------
process_nuts2_files.lst <- list()
process_nuts3_files.lst <- list()

## Step 2b: Compute NUTS2 Spatial Mean -------------

# -- starting index
i <- 1

# -- loop to process all raster files
for (nc_file in nc_files.lst) {
  
  # Step 1: loading .nc file
  raster <- rast(nc_file)
  
  basename(nc_file)
  
  # Step 2: Assign dates to raster layers

  # - Create a sequence of dates for entire file period to feed to raster data
  startdate <- if_else(str_sub(basename(nc_file), 16, 18) == "avg",
                       "1980-01-01", startdate)
  
  dates <- seq(as.Date(startdate, "%Y-%m-%d"), as.Date(enddate, "%Y-%m-%d"), by = "day")
  
  # - Add the dates to raster
  names(raster) <- dates
  
  # resetting start date
  startdate <- "1979-01-01" 
  
  # Step 3: Match NUTS and temp CRS
  spdf_crs <- st_transform(nuts2.shp, st_crs(raster))
  
  # Step 4: Generate a unique filename for each .nc file's output
  nc_file_name <- basename(nc_file)
  new_file_name <- paste0(substr(nc_file_name, 16, 18), ".csv")
  
  # Step 5: Compute spatial mean
  extracted <- exact_extract(raster, spdf_crs, 'mean', 
                             append_cols = 'NUTS_ID', default_value = NA_real_,  progress = TRUE)
  
  # Step 6: Reshape data into long format
  temp_long <- pivot_longer(
    extracted,  
    -c(NUTS_ID), 
    names_to = "date",
    values_to = "utci")
  
  temp_long <- temp_long %>%
    mutate(date = str_replace(date, "extracted\\.", ""))
  
  # Step 7: Save output
  process_nuts2_files.lst[[i]] <- temp_long
  
  # Step 8: Increase index
  i <- i+1
  
  # Step 9: Print update
  cat("Processed and saved data for: ", new_file_name, "\n")
  
  
}  

# -- Append UTCI measure type to sf
type <- c("avg", "max", "min")

process_nuts2_w_names.lst <- list()

for (i in 1:length(process_nuts2_files.lst)){
  
  file <- process_nuts2_files.lst[[i]]
  
  file$type <- type[i]
  
  process_nuts2_w_names.lst[[i]] <- file 
  
}


# -- Combine files in list into single dataframe

utci_nuts2 <- bind_rows(process_nuts2_w_names.lst)

# -- Reformat and export file

# Convert UTCI to Celsius & edit date variable 
utci_nuts2 = utci_nuts2 %>% 
  mutate(utci_c = utci - 273.15) %>% 
  mutate(date = str_replace_all(date, "mean.", ""))

# Reshaping data & reorganizing columns
utci_nuts2 = utci_nuts2 %>%
  select(-utci) %>% 
  pivot_wider(names_from = type,
              values_from = utci_c, 
              names_prefix = "utci_") %>%
  rename(utci_mean = utci_avg) %>%
  relocate(NUTS_ID, date, utci_mean, utci_min, utci_max)

# Export data
write.csv(utci_nuts2, file = export_nuts2)


## Step 2c: Compute NUTS3 Spatial Mean -------------

# -- starting index
i <- 1

# -- loop to process all raster files
for (nc_file in nc_files.lst) {
  
  # Step 1: loading .nc file
  raster <- rast(nc_file)
  
  basename(nc_file)
  
  # Step 2: Assign dates to raster layers
  
  # - Create a sequence of dates for entire file period to feed to raster data
  startdate <- if_else(str_sub(basename(nc_file), 16, 18) == "avg",
                       "1980-01-01", startdate)
  
  dates <- seq(as.Date(startdate, "%Y-%m-%d"), as.Date(enddate, "%Y-%m-%d"), by = "day")
  
  # - Add the dates to raster
  names(raster) <- dates
  
  # resetting start date
  startdate <- "1979-01-01" 
  
  # Step 3: Match NUTS and temp CRS
  spdf_crs <- st_transform(nuts3.shp, st_crs(raster))
  
  # Step 4: Generate a unique filename for each .nc file's output
  nc_file_name <- basename(nc_file)
  new_file_name <- paste0(substr(nc_file_name, 16, 18), ".csv")
  
  # Step 5: Compute spatial mean
  extracted <- exact_extract(raster, spdf_crs, 'mean', 
                             append_cols = 'NUTS_ID', default_value = NA_real_,  progress = TRUE)
  
  # Step 6: Reshape data into long format
  temp_long <- pivot_longer(
    extracted,  
    -c(NUTS_ID), 
    names_to = "date",
    values_to = "utci")
  
  temp_long <- temp_long %>%
    mutate(date = str_replace(date, "extracted\\.", ""))
  
  # Step 7: Save output
  process_nuts3_files.lst[[i]] <- temp_long
  
  # Step 8: Increase index
  i <- i+1
  
  # Step 9: Print update
  cat("Processed and saved data for: ", new_file_name, "\n")
  
  
}  

# -- Append UTCI measure type to sf
type <- c("avg", "max", "min")

process_nuts3_w_names.lst <- list()

for (i in 1:length(process_nuts3_files.lst)){
  
  file <- process_nuts3_files.lst[[i]]
  
  file$type <- type[i]
  
  process_nuts3_w_names.lst[[i]] <- file 
  
}


# -- Combine files in list into single dataframe

utci_nuts3 <- bind_rows(process_nuts3_w_names.lst)

# -- Reformat and export file

# Convert UTCI to Celsius & edit date variable 
utci_nuts3 = utci_nuts3 %>% 
  mutate(utci_c = utci - 273.15) %>% 
  mutate(date = str_replace_all(date, "mean.", ""))

# Reshaping data & reorganizing columns
utci_nuts3 = utci_nuts3 %>%
  select(-utci) %>% 
  pivot_wider(names_from = type,
              values_from = utci_c, 
              names_prefix = "utci_") %>%
  rename(utci_mean = utci_avg) %>%
  relocate(NUTS_ID, date, utci_mean, utci_min, utci_max)

# Export data
write.csv(utci_nuts3, file = export_nuts3)
