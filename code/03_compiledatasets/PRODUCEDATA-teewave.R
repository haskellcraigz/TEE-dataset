#########################################
## Produce teewave: heatwaves/coldsnaps 1980-2021
## Last Modified: May 16 2025
########################################
## NOTES:

# Packages ---------------

library(tidyverse)

# set working directory to TEE-dataset-main folder
setwd("")

# Load data --------------

wave_local_nuts2 <- read.csv("data/02_metrics/wave_local_nuts2.csv")
wave_local_nuts3 <- read.csv("data/02_metrics/wave_local_nuts3.csv")

wave_global_nuts2 <- read.csv("data/02_metrics/wave_global_nuts2.csv")
wave_global_nuts3 <- read.csv("data/02_metrics/wave_global_nuts3.csv")

# Clean up variable names --------------

## NUTS 2 --------------

# renaming variables to reflect temp measurement
wave_local_nuts2 = wave_local_nuts2 %>% 
  rename(nuts_id = NUTS_ID) %>% 
  mutate(type = paste0(above_below, as.character(percentile), "percentile")) %>% 
  mutate(scale = "NUTS2") %>% 
  select(scale, nuts_id, year, type, temperature_threshold, startdate, enddate, wavelength)

wave_global_nuts2 = wave_global_nuts2 %>% 
  rename(nuts_id = NUTS_ID) %>% 
  mutate(type = paste0(above_below, as.character(temperature_threshold), "C")) %>% 
  mutate(scale = "NUTS2") %>% 
  select(scale, nuts_id, year, type, temperature_threshold, startdate, enddate, wavelength)

wave_nuts2 <- rbind(wave_local_nuts2, wave_global_nuts2)

wave_nuts2 = wave_nuts2 %>% 
  filter(wavelength > 1)

write.csv(wave_nuts2, "data/03_fulldatasets/teewave_nuts2.csv")

## NUTS 3 --------------

# renaming variables to reflect temp measurement
wave_local_nuts3 = wave_local_nuts3 %>% 
  rename(nuts_id = NUTS_ID) %>% 
  mutate(type = paste0(above_below, as.character(percentile), "percentile")) %>% 
  mutate(scale = "NUTS3") %>% 
  select(scale, nuts_id, year, type, temperature_threshold, startdate, enddate, wavelength) %>% 
  mutate(temperature_threshold = temperature_threshold - 273.15)

wave_global_nuts3 = wave_global_nuts3 %>% 
  rename(nuts_id = NUTS_ID) %>% 
  mutate(type = paste0(above_below, as.character(temperature_threshold), "C")) %>% 
  mutate(scale = "NUTS3") %>% 
  select(scale, nuts_id, year, type, temperature_threshold, startdate, enddate, wavelength) %>% 
  mutate(temperature_threshold = temperature_threshold - 273.15)

wave_nuts3 <- rbind(wave_local_nuts3, wave_global_nuts3)

wave_nuts3 = wave_nuts3 %>% 
  filter(wavelength > 1)

write.csv(wave_nuts3, "data/03_fulldatasets/teewave_nuts3.csv")
