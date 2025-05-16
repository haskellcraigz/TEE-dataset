###############################################
# Data Paper - Tropical Nights/Polar Days
# Date Last Modified: May 16 2025
################################################
# NOTES: Following Medina-Ramon (2007), Mills(2015) and the EEA publication on extreme 
# heat indices, we define "Tropical Nights" as a minimum temperature above 20*C 
# [and polar days as the maximum temperature below 10* C]
# Note that EEA suggest this index as relevant mainly for the health sector

# Packages -------------------
library(tidyverse)
library(zoo)

# set working directory to TEE-dataset-main folder
setwd("")

# Load data [FILE PATH TO BE UPDATED IN FINAL REPO] --------------------
## NUTS3
data_nuts3 <- read_csv("data/02_metrics/dailytemp_nuts3.csv")
data_nuts3 <- data_nuts3 %>% 
  mutate(year = str_sub(date, 1, 4))


##NUTS2
data_nuts2 <- read_csv("data/02_metrics/dailytemp_nuts2.csv")
# making year variable
data_nuts2 <- data_nuts2 %>% 
  mutate(year = str_sub(date, 1, 4))

# Set hardcoded thresholds ----------
# tropical nights/polar days
tn_thresh <- 20
pd_thresh <- 10

# first year of data period
first_year <- 1980

# export paths
export_nuts2 <- "data/02_metrics/tropicalnights_nuts2.csv"
export_nuts3 <- "data/02_metrics/tropicalnights_nuts3.csv"


# Compute metric -------------

## functions -----------------

# function for tropical nights per year using temp_min variable
tropical_nights <- function(df, threshold){
  
  
  for (i in 1:length(threshold)){
    
    
    df = df %>% 
      mutate(varname = if_else(temp_min >= threshold[i], 1, 0)) %>% 
      group_by(NUTS_ID, year) %>% 
      mutate(varname_count = sum(varname, na.rm=TRUE)) %>% 
      ungroup()
    
    names(df)[names(df) == "varname"] <- paste0("over", as.character(threshold[i]))
    names(df)[names(df) == "varname_count"] <- paste0("daysover", as.character(threshold[i]), "_yr")
    
  }
  
  return(df)
  
}


# function for cold days per year using temp_max value
polar_days <- function(df, threshold){
  
  
  for (i in 1:length(threshold)){
    
    df = df %>% 
      mutate(varname = if_else(temp_max <= threshold[i], 1, 0)) %>% 
      group_by(NUTS_ID, year) %>% 
      mutate(varname_count = sum(varname, na.rm=TRUE)) %>% 
      ungroup()
    
    names(df)[names(df) == "varname"] <- paste0("below", as.character(threshold[i]))
    names(df)[names(df) == "varname_count"] <- paste0("daysbelow", as.character(threshold[i]), "_yr")
    
  }
  
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
tropical_nights_nuts2 <- tropical_nights(data_nuts2, tn_thresh)
all_nuts2 <- polar_days(tropical_nights_nuts2, pd_thresh)

tropical_nights_nuts3 <- tropical_nights(data_nuts3, tn_thresh)
all_nuts3 <- polar_days(tropical_nights_nuts3, pd_thresh)


## keep only yearly counts ----------------
yearly_nuts2 <- all_nuts2 %>%
  select(NUTS_ID, year, 
         daysover20_yr,
         daysbelow10_yr) %>% #drop daily variables
  unique() #keep only one row per NUTS_ID, year

yearly_nuts3 <- all_nuts3 %>%
  select(NUTS_ID, year, 
         daysover20_yr,
         daysbelow10_yr) %>% #drop daily variables
  unique() #keep only one row per NUTS_ID, year

## join and save [UPDATED FILE PATH FOR FINAL REPO] -----------------------

write_csv(yearly_nuts2, file = export_nuts2)


write_csv(yearly_nuts3, file = export_nuts3)



