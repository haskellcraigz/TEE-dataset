#########################################################
# Pop. weighted batch processing daily temp min, mean, and max 1950-2023 
# Last Updated: May 16 2025
############################################################
## NOTE 1: see GPW_v4 contents file for explanation of each layer
## NOTE 2: If you use a different time frame, you will need to revise the
## ---------- script by removing code for years not included in your data

# ---------------------------------------------------------------------------- #
# Set-up ----------------------------------------------------------------------
# ---------------------------------------------------------------------------- #

# if needed, set working directory to TEE-dataset-main folder using below command
# setwd("")

# --- libraries

# raster packages
library(terra)
library(exactextractr)

# shapefile packages 
library(sf)

# df packages
library(tidyverse)


library(foreign)

#working with dates
library(lubridate)

# Init values ------------------------

# years of the temperature raster data (change dates if you pull different years of data)
startdate <- "1950-01-01"
enddate <- "2024-12-31"

# export file paths
export_nuts2 <- "data/02_metrics/dailypoptemp_nuts2.csv"
export_nuts3 <- "data/02_metrics/dailypoptemp_nuts3.csv"

# Load data and file names  -------------------

## load nuts shapefiles --------------
# download nuts files located in data/01_rawdata/nuts/:
spdf <- st_read("data/01_rawdata/nuts/NUTS_RG_10M_2021_4326.shp")

## load raster files -----------------

# temperature rasters
nc_files.lst <- list.files(path = "data/01_rawdata/dailytemp/", 
                           pattern = "\\.nc$", full.names = TRUE)

# population by year in data/rawdata/pop
pop_file_path <- list.files(path = "data/01_rawdata/pop/", 
                            pattern = "\\.nc$", full.names = TRUE)



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

## initalizing loop values --------------

process_nuts2_files.lst <- list()
process_nuts3_files.lst <- list()

# index
i <- 1


## NUTS2 begin loop taking spatial mean for each file ------------

for (nc_file in nc_files.lst) {
  
  # loading temperature .nc file
  raster <- rast(nc_file)
  
  basename(nc_file)
  
  
  # Create a sequence of dates for entire file period to feed to raster data
  dates <- seq(as.Date(startdate, "%Y-%m-%d"), as.Date(enddate, "%Y-%m-%d"), by = "day")
  
  # add the dates 
  names(raster) <- dates
  
  
  # load population .nc file
  pop <- rast(pop_file_path)
  
  # population years
  # NOTE: only first 5 layers of raster has yearly data, the rest is metadata
  years <- c(c(2000, 2005, 2010, 2015, 2020), rep(0, 15))
  
  # add these year-identifiers to pop raster
  names(pop) <- years
  
  # matching crs
  spdf_crs <- st_transform(nuts2.shp, st_crs(raster))
  
  #match pop raster to temp raster
  pop_crop_resampled <- resample(pop, raster, method="bilinear")
 
   ############################
  # split both temperature and population by years
  dates_2000_2005 <- seq(as.Date("2000-01-01"), as.Date("2004-12-31"), by = "day")
  temp_2000 <- subset(raster, names(raster) %in% dates_2000_2005) #subset
  pop_2000 <- pop_crop_resampled[["2000"]]
  
  dates_2005_2010 <- seq(as.Date("2005-01-01"), as.Date("2009-12-31"), by = "day")
  temp_2005 <- subset(raster, names(raster) %in% dates_2005_2010) #subset
  pop_2005 <- pop_crop_resampled[["2005"]]
  
  dates_2010_2015 <- seq(as.Date("2010-01-01"), as.Date("2014-12-31"), by = "day")
  temp_2010 <- subset(raster, names(raster) %in% dates_2010_2015) #subset
  pop_2010 <- pop_crop_resampled[["2010"]]
  
  dates_2015_2020 <- seq(as.Date("2015-01-01"), as.Date("2019-12-31"), by = "day")
  temp_2015 <- subset(raster, names(raster) %in% dates_2015_2020) #subset
  pop_2015 <- pop_crop_resampled[["2015"]]
  
  dates_2020_2021 <- seq(as.Date("2020-01-01"), as.Date(enddate, "%Y-%m-%d"), by = "day")
  temp_2020 <- subset(raster, names(raster) %in% dates_2020_2021) #subset
  pop_2020 <- pop_crop_resampled[["2020"]]
  
  
  ##################################
  # Extract pop-weighted temperature for NUTS2 boundary
  weighted_extract_2000 <- exact_extract(temp_2000, spdf_crs, 'weighted_mean',
                            append_cols = 'NUTS_ID', weights = pop_2000,
                            default_value = NA_real_, default_weight = 0, 
                            progress = TRUE)
                                   
  
  weighted_extract_2005 <- exact_extract(temp_2005, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2005,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  weighted_extract_2010 <- exact_extract(temp_2010, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2010,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  weighted_extract_2015 <- exact_extract(temp_2015, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2015,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  weighted_extract_2020 <- exact_extract(temp_2020, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2020,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  
  
  
  
  # Generate a unique filename for each .nc file's output (min, max, mean)
  nc_file_name <- basename(nc_file)
  new_file_name <- paste0(substr(nc_file_name, 28, 30), "_pop_weighted.csv")


  # pivot into long-form tables
  long_2000 <- weighted_extract_2000 %>%
    pivot_longer(-c(NUTS_ID),
                            names_to = "date",
                            values_to = paste0(substr(nc_file_name, 28, 30),
                                               "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  long_2005 <- weighted_extract_2005 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  long_2010 <- weighted_extract_2010 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  long_2015 <- weighted_extract_2015 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  long_2020 <- weighted_extract_2020 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  
  
  #combine
  temperature_all <- rbind(long_2000, long_2005)
  temperature_all <- rbind(temperature_all, long_2010)
  temperature_all <- rbind(temperature_all, long_2015)
  temperature_all <- rbind(temperature_all, long_2020)
  
  # save to output list
  process_nuts2_files.lst[[i]] <- temperature_all
  
  i <- i+1
  
  cat("Processed and saved data for: ", new_file_name, "\n")
  
  
}  





## combine datasets from loop --------------------

nuts2_temp.df <- left_join(process_nuts2_files.lst[[1]], process_nuts2_files.lst[[2]],
                           by=c("NUTS_ID", "date"))

nuts2_temp.df <- left_join(nuts2_temp.df, process_nuts2_files.lst[[3]],
                           by=c("NUTS_ID", "date") )


## clean and rename any variables -------------
# renaming temperature variables
nuts2_temp.df = nuts2_temp.df %>% 
  rename(temp_max_pop_weighted = max_pop_weighted,
         temp_mean_pop_weighted = mea_pop_weighted,
         temp_min_pop_weighted = min_pop_weighted)

# convert to celcius
nuts2_temp.df = nuts2_temp.df %>% 
  mutate(temp_max_pop_weighted = temp_max_pop_weighted - 273.15,
         temp_mean_pop_weighted = temp_mean_pop_weighted - 273.15,
         temp_min_pop_weighted = temp_min_pop_weighted - 273.15)

## save NUTS2 -------------------------
write.csv(nuts2_temp.df, file = export_nuts2)




## NUTS3 begin loop taking spatial mean for each file ------------

i <- 1

for (nc_file in nc_files.lst) {
  
  # loading temperature .nc file
  raster <- rast(nc_file)
  
  basename(nc_file)
  
  
  # Create a sequence of dates for entire file period to feed to raster data
  dates <- seq(as.Date(startdate, "%Y-%m-%d"), as.Date(enddate, "%Y-%m-%d"), by = "day")
  
  # add the dates 
  names(raster) <- dates
  
  
  # load population .nc file
  pop <- rast(pop_file_path)
  
  # population years
  # NOTE: only first 5 layers of raster has yearly data, the rest is metadata
  years <- c(c(2000, 2005, 2010, 2015, 2020), rep(0, 15))
  
  # add these year-identifiers to pop raster
  names(pop) <- years
  
  # matching crs
  spdf_crs <- st_transform(nuts3.shp, st_crs(raster))
  
  #match pop raster to temp raster
  pop_crop_resampled <- resample(pop, raster, method="bilinear")
  
  ############################
  # split both temperature and population by years
  dates_2000_2005 <- seq(as.Date("2000-01-01"), as.Date("2004-12-31"), by = "day")
  temp_2000 <- subset(raster, names(raster) %in% dates_2000_2005) #subset
  pop_2000 <- pop_crop_resampled[["2000"]]
  
  dates_2005_2010 <- seq(as.Date("2005-01-01"), as.Date("2009-12-31"), by = "day")
  temp_2005 <- subset(raster, names(raster) %in% dates_2005_2010) #subset
  pop_2005 <- pop_crop_resampled[["2005"]]
  
  dates_2010_2015 <- seq(as.Date("2010-01-01"), as.Date("2014-12-31"), by = "day")
  temp_2010 <- subset(raster, names(raster) %in% dates_2010_2015) #subset
  pop_2010 <- pop_crop_resampled[["2010"]]
  
  dates_2015_2020 <- seq(as.Date("2015-01-01"), as.Date("2019-12-31"), by = "day")
  temp_2015 <- subset(raster, names(raster) %in% dates_2015_2020) #subset
  pop_2015 <- pop_crop_resampled[["2015"]]
  
  dates_2020_2021 <- seq(as.Date("2020-01-01"), as.Date(enddate, "%Y-%m-%d"), by = "day")
  temp_2020 <- subset(raster, names(raster) %in% dates_2020_2021) #subset
  pop_2020 <- pop_crop_resampled[["2020"]]
  
  
  ##################################
  # Extract pop-weighted temperature for NUTS2 boundary
  weighted_extract_2000 <- exact_extract(temp_2000, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2000,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  
  weighted_extract_2005 <- exact_extract(temp_2005, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2005,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  weighted_extract_2010 <- exact_extract(temp_2010, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2010,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  weighted_extract_2015 <- exact_extract(temp_2015, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2015,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  weighted_extract_2020 <- exact_extract(temp_2020, spdf_crs, 'weighted_mean',
                                         append_cols = 'NUTS_ID', weights = pop_2020,
                                         default_value = NA_real_, default_weight = 0, 
                                         progress = TRUE)
  
  
  
  
  
  # Generate a unique filename for each .nc file's output (min, max, mean)
  nc_file_name <- basename(nc_file)
  new_file_name <- paste0(substr(nc_file_name, 28, 30), "_pop_weighted.csv")
  
  
  # pivot into long-form tables
  long_2000 <- weighted_extract_2000 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  long_2005 <- weighted_extract_2005 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  long_2010 <- weighted_extract_2010 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  long_2015 <- weighted_extract_2015 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  long_2020 <- weighted_extract_2020 %>%
    pivot_longer(-c(NUTS_ID),
                 names_to = "date",
                 values_to = paste0(substr(nc_file_name, 28, 30),
                                    "_pop_weighted")) %>%
    mutate(date = str_replace(date, "weighted_mean\\.", ""))
  
  
  
  #combine
  temperature_all <- rbind(long_2000, long_2005)
  temperature_all <- rbind(temperature_all, long_2010)
  temperature_all <- rbind(temperature_all, long_2015)
  temperature_all <- rbind(temperature_all, long_2020)
  
  # save to output list
  process_nuts3_files.lst[[i]] <- temperature_all
  
  i <- i+1
  
  cat("Processed and saved data for: ", new_file_name, "\n")
  
  
}  





## combine datasets from loop --------------------

nuts3_temp.df <- left_join(process_nuts3_files.lst[[1]], process_nuts3_files.lst[[2]],
                           by=c("NUTS_ID", "date"))

nuts3_temp.df <- left_join(nuts3_temp.df, process_nuts3_files.lst[[3]],
                           by=c("NUTS_ID", "date") )


## clean and rename any variables -------------
# renaming temperature variables
nuts3_temp.df = nuts3_temp.df %>% 
  rename(temp_max_pop_weighted = max_pop_weighted,
         temp_mean_pop_weighted = mea_pop_weighted,
         temp_min_pop_weighted = min_pop_weighted)

# convert to celcius
nuts3_temp.df = nuts3_temp.df %>% 
  mutate(temp_max_pop_weighted = temp_max_pop_weighted - 273.15,
         temp_mean_pop_weighted = temp_mean_pop_weighted - 273.15,
         temp_min_pop_weighted = temp_min_pop_weighted - 273.15)

## save NUTS3 [PATH TO BE UPDATED] -------------------------
write.csv(nuts3_temp.df, file = export_nuts3)






