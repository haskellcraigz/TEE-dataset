###############################################
# Constructing Cooling and Heating Degree Days (CDD/HDD)
# Date Last Updated: May 16 2025
################################################
## NOTES:
## Following Carr (2024), De Rosa (2015) and the US national weather service 
## definition of cooling/heating degree days - using Carr's threshold of 24C
## for CDD and (one of) De Rosa's threshold of 20C for HDD
## Note - this metric is used particularly when considering the impact of
## temperature on energy use/the electrical grid

# if needed, set working directory to TEE-dataset-main folder using below command
# setwd("")

# Packages [FILE PATH TO BE UPDATED IN FINAL REPO] -------------------
library(tidyverse)

# Load data [FILE PATH TO BE UPDATED IN FINAL REPO] --------------------
## NUTS3
data_nuts3 <- read_csv("data/02_metrics/dailytemp_nuts3.csv")
# making year variable
data_nuts3 <- data_nuts3 %>% 
  mutate(year = str_sub(date, 1, 4))


##NUTS2
data_nuts2 <- read_csv("data/02_metrics/dailytemp_nuts2.csv")
# making year variable
data_nuts2 <- data_nuts2 %>% 
  mutate(year = str_sub(date, 1, 4))

# Set hardcoded thresholds ----------


# CDD/HDD thresholds
cdd_thresh <- 24
hdd_thresh <- 20

# first year of data period
first_year <- 1980

# export file paths
export_nuts2 <- "data/02_metrics/cdd_nuts2.csv"
export_nuts3 <- "data/02_metrics/cdd_nuts3.csv"

# Compute metric -------------
# difference between mean temperature and threshold
cdd <- function(df, threshold){
  # cdd = sum of (temp_mid - 24C if temp_mid > 24, 0 otw)
  #
  #
  df = df %>% 
    mutate(temp_mid = (temp_min + temp_max)/2,
           degrees = if_else(temp_mid > threshold, 
                             temp_mid - threshold, 0)) %>% 
    group_by(NUTS_ID, year) %>% 
    mutate(cdd = sum(degrees, na.rm=TRUE)) %>% 
    ungroup()
  
  return(df)
}

hdd <- function(df, threshold){
  
  df = df %>% 
    mutate(temp_mid = (temp_min + temp_max)/2,
           degrees = if_else(temp_mid < threshold, 
                             threshold - temp_mid, 0)) %>% 
    group_by(NUTS_ID, year) %>% 
    mutate(hdd = sum(degrees, na.rm=TRUE)) %>% 
    ungroup()
  
  return(df)
}

## Clean data -----------------

### filter to study period ------------
data_nuts2 <- data_nuts2 %>%
  filter(year >= first_year)

data_nuts3 <- data_nuts3 %>%
  filter(year >= first_year)

## run function ----------------
# note: as the functions add to the input dataframe, we can chain these together
## to produce one dataframe with tropical nights and polar days as separate columns
cdd_nuts2 <- cdd(data_nuts2, cdd_thresh)
all_nuts2 <- hdd(cdd_nuts2, hdd_thresh)

cdd_nuts3 <- cdd(data_nuts3, cdd_thresh)
all_nuts3 <- hdd(cdd_nuts3, hdd_thresh)


## keep only yearly counts ----------------
yearly_nuts2 <- all_nuts2 %>%
  select(NUTS_ID, year, 
         cdd,
         hdd) %>% #drop daily variables
  unique() #keep only one row per NUTS_ID, year

yearly_nuts3 <- all_nuts3 %>%
  select(NUTS_ID, year, 
         cdd,
         hdd) %>% #drop daily variables
  unique() #keep only one row per NUTS_ID, year

## join and save [UPDATED FILE PATH FOR FINAL REPO] -----------------------

write_csv(yearly_nuts2, file = export_nuts2)


write_csv(yearly_nuts3, file = export_nuts3)





