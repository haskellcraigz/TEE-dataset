###############################################
# Constructing Days over local threshold and that temp. threshold
# Date Last Modified: May 16 2025
################################################
## NOTES:

# Packages] -------------------
library(tidyverse)

# set working directory to TEE-dataset-main folder
setwd("")


# Load data  --------------------
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
# temperature percentiles
extreme_heat <- c(0.90, 0.95, 0.99)
extreme_cold <- c(0.01, 0.05, 0.10)

# last year of reference period
ref_year <- 1990
# first year of reference period 
ref_year_first <- 1961
# first year of data period
first_year <- 1979

# export file paths
export_nuts2 <- "data/02_metrics/percentilethreshold_nuts2.csv"
export_nuts3 <- "data/02_metrics/percentilethreshold_nuts3.csv"

# Compute metric -------------

## functions -----------------
# function to count days over threshold
hot.days.yr <- function(df, percentile, ref_year){
  
  
  for (i in 1:length(percentile)){
    
    df_ref = df %>% 
      filter(year <= ref_year) %>% 
      group_by(NUTS_ID) %>% 
      mutate(threshold =  quantile(temp_mean, percentile[i], na.rm=TRUE)) %>% 
      ungroup()  %>% 
      select(NUTS_ID, threshold) %>% 
      distinct()
    
    df <- left_join(df, df_ref, by=c("NUTS_ID"))
    
    df = df %>% 
      mutate(varname = if_else(temp_mean >= threshold, 1, 0)) %>% 
      group_by(NUTS_ID, year) %>% 
      mutate(varname_count = sum(varname, na.rm=TRUE)) %>% 
      ungroup()
    
    names(df)[names(df) == "threshold"] <- paste0("threshold", as.character((percentile[i])))
    names(df)[names(df) == "varname"] <- paste0("over", as.character(percentile[i]))
    names(df)[names(df) == "varname_count"] <- paste0("daysover", as.character(percentile[i]), "_yr")
    
  }
  
  return(df)
  
}


# function for cold days per year using temp_mean value

cold.days.yr <- function(df, percentile, ref_year){
  
  
  for (i in 1:length(percentile)){
    
    df_ref = df %>% 
      filter(year <= ref_year) %>% 
      group_by(NUTS_ID) %>% 
      mutate(threshold =  quantile(temp_mean, percentile[i], na.rm=TRUE)) %>% 
      ungroup() %>% 
      select(NUTS_ID, threshold) %>% 
      distinct()
    
    df <- left_join(df, df_ref, by=c("NUTS_ID"))
    
    df = df %>% 
      mutate(varname = if_else(temp_mean <= threshold, 1, 0)) %>% 
      group_by(NUTS_ID, year) %>% 
      mutate(varname_count = sum(varname, na.rm=TRUE)) %>% 
      ungroup()
    
    names(df)[names(df) == "threshold"] <- paste0("threshold", as.character((percentile[i])))
    names(df)[names(df) == "varname"] <- paste0("below", as.character(percentile[i]))
    names(df)[names(df) == "varname_count"] <- paste0("daysbelow", as.character(percentile[i]), "_yr")
    
  }
  
  return(df)
  
}


# Run Functions  --------------

## Clean data -----------------

## filter data to first year of reference period  ------------
data_nuts2 <- data_nuts2 %>%
  filter(year >= ref_year_first)

data_nuts3 <- data_nuts3 %>%
  filter(year >= ref_year_first)

## NUTS2 calculate percentile temperature and days over threshold ------------
# note: as the functions add to the input dataframe, we can chain these together
## to produce one dataframe with separate columns
data_nuts2 <- hot.days.yr(data_nuts2, extreme_heat, ref_year)
data_nuts2 <- cold.days.yr(data_nuts2, extreme_cold, ref_year)

### keep only years of study period & clean up cols and names -------------
data_nuts2 <- data_nuts2 %>%
  filter(year >= first_year) %>%
  select(NUTS_ID, year, starts_with("threshold"), starts_with("days")) %>%
  unique()

### save  ----------------

write_csv(data_nuts2, file = export_nuts2)



## NUTS3 calculate percentile temperature and days over threshold ------------
# note: as the functions add to the input dataframe, we can chain these together
## to produce one dataframe with separate columns
data_nuts3 <- hot.days.yr(data_nuts3, extreme_heat, ref_year)
data_nuts3 <- cold.days.yr(data_nuts3, extreme_cold, ref_year)


### keep only years of study period & clean up cols and names -------------
data_nuts3 <- data_nuts3 %>%
  filter(year >= first_year) %>%
  select(NUTS_ID, year, starts_with("threshold"), starts_with("days")) %>%
  unique()

### save ----------------

write_csv(data_nuts3, file = export_nuts3)