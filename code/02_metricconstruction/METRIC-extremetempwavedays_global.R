###############################################
# Constructing Extreme Temperature Waves using Global Threshold
# Date Last Modified: May 16 2025
################################################
# NOTES: Reference period can be adjusted by user. Last year of reference period
# can be set manually under `Set hardcoded thresholds`. If the first year of the 
# reference period needs to be adjusted, user can filter the dataset before
# applying the functions

# if needed, set working directory to TEE-dataset-main folder using below command
# setwd("")

# Packages -------------------
library(tidyverse)
library(zoo)

# Load data --------------------
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
# global temperature cut-offs
hottemp_val <- c(20, 30, 40)
coldtemp_val <- c(-10, 0, 10)


# last year of reference period
ref_year <- 1990

#first year of data period
first_year <- 1980

# export filepaths
export_nuts2 <- "data/02_metrics/wave_global_nuts2.csv"
export_nuts3 <- "data/02_metrics/wave_global_nuts3.csv"

# Filter data to only observations after reference period -----------

data_nuts2 <- data_nuts2 %>%
  filter(year >= first_year)


data_nuts3 <- data_nuts3 %>%
  filter(year >= first_year)


# Compute metrics -----------------------------------

## functions -----------------

# function to count days over threshold
hot.days.yr <- function(df, threshold_val){
  
  
  for (i in threshold_val){
    
    # computing days ina year over a threshold
    df = df %>% 
      mutate(varname = if_else(temp_max >= i, 1, 0)) %>% 
      group_by(NUTS_ID, year) %>% 
      mutate(varname_count = sum(varname, na.rm=TRUE)) %>% 
      ungroup()
    
    # renaming variables to names that include threshold
    names(df)[names(df) == "threshold"] <- paste0("threshold", as.character(i))
    names(df)[names(df) == "varname"] <- paste0("over", as.character(i))
    names(df)[names(df) == "varname_count"] <- paste0("daysover", as.character(i), "_yr")
    
  }
  
  return(df)
  
}


# function for cold days per year using temp_max value

cold.days.yr <- function(df, threshold_val){
  
  
  for (i in threshold_val){
    
    # computing number of days in a year below threshold
    df = df %>% 
      mutate(varname = if_else(temp_max <= i, 1, 0)) %>% 
      group_by(NUTS_ID, year) %>% 
      mutate(varname_count = sum(varname, na.rm=TRUE)) %>% 
      ungroup()
    
    # renaming variables to names that include threshold
    names(df)[names(df) == "threshold"] <- paste0("threshold", as.character(i))
    names(df)[names(df) == "varname"] <- paste0("below", as.character(i))
    names(df)[names(df) == "varname_count"] <- paste0("daysbelow", as.character(i), "_yr")
    
  }
  
  return(df)
  
}


# building function that identifies heat waves
extremewaves.yr <- function(df){
  
  temperature <- unique(df$temperature) #which percentile threshold is used in calculation
  thresh <- gsub("[^0-9.-]", "",
                 temperature) #drop characters from percentile to get the threshold number
  
  length <- 2

  #remove columns from df not being used to save space
  df <- df %>%
    select(NUTS_ID, date, temperature_bin) #note `temperature_bin` is indicator of if date is over threshold
  
  df = df %>%
    group_by(NUTS_ID) %>%
    mutate(count = zoo::rollsum(temperature_bin, length, align="right", fill = NA)) %>%
    ungroup()
  
  # generating periods of extreme temperature waves
  df = df %>%
    mutate(date = as.Date(date)) %>%
    mutate(count = duration(count, "day")) %>% 
    arrange(NUTS_ID, date) %>%
    group_by(NUTS_ID) %>%
    mutate(consecdate = if_else((date - duration(1, "day")) == lag(date), 1, 0)) %>% 
    mutate(startdate = if_else(consecdate == 1 & lag(temperature_bin) == 1, date - 1, date)) %>%
    mutate(enddate = if_else(lead(temperature_bin) == 1 & lead(consecdate) == 1, lead(date), date)) %>%
    ungroup() %>%
    mutate(startdate = if_else(temperature_bin == 0 | consecdate == 0, NA, startdate)) %>%
    mutate(enddate = if_else(temperature_bin == 0 | consecdate == 0, NA, enddate))
  
  # collapsing overlapping periods
  wave = df %>%
    filter(is.na(startdate) == FALSE) %>%
    select(NUTS_ID, startdate, enddate) %>%
    group_by(NUTS_ID) %>%
    mutate(indx = c(0, cumsum(as.numeric(lead(startdate)) > cummax(as.numeric(enddate)))[-n()])) %>%
    group_by(NUTS_ID, indx) %>%
    summarise(startdate = min(startdate),
              enddate = max(enddate)) %>%
    ungroup()
  
  # constructing year and length variables
  wave = wave %>%
    mutate(year = year(startdate)) %>%
    mutate(wavelength = enddate - startdate) %>%
    select(-indx) #remove index as not needed any more
  
  
  
  # add temperature threshold as a column 
  wave$temperature_threshold <- as.numeric(thresh)
  
  #add above/below indicator
  wave <- wave %>%
    mutate(above_below = gsub("[^a-zA-Z]", "",
                              temperature)) %>%
    relocate(NUTS_ID, above_below, temperature_threshold, 
             year, startdate, enddate, wavelength)
  
  
  return(wave)
  
}





## run functions (nuts2) -----------------------

### calculate days over threshold -----------

# note: as the functions add to the input dataframe, we can chain these together
## to produce one dataframe with separate columns
data_nuts2 <- hot.days.yr(data_nuts2, hottemp_val)
data_nuts2 <- cold.days.yr(data_nuts2, coldtemp_val)



### count of consecutive days over threshold nuts 2 -------------------
# ordering dataset
data_nuts2 = data_nuts2 %>%
  arrange(NUTS_ID, date)


# reshape data to split day functions into their own dataset in a list
binaryvar = data_nuts2 %>%
  select(starts_with("over"), starts_with("below")) #this finds the col names,
#since identical nuts2/nuts3 column names, we only need to do this once on one of them

binaryvar <- as.vector(names(binaryvar))

data_nuts2 <- pivot_longer(data_nuts2, cols=binaryvar, names_to = "temperature",
                           values_to = "temperature_bin")




data_nuts2.lst <- split(data_nuts2, data_nuts2$temperature)

rm(data_nuts2) #remove to save space in working environment



# # implement function
data_nuts2_wave.lst <- lapply(data_nuts2.lst, extremewaves.yr)

# binding rows and dropping extra variables
wave_nuts2 <- do.call("rbind", data_nuts2_wave.lst)

#remove the row names
wave_nuts2 <- remove_rownames(wave_nuts2)

### save ----------------------------
write_csv(wave_nuts2, file = export_nuts2)

### remove objects to save space  --------------
rm(data_nuts2.lst, data_nuts2_wave.lst)

## run functions (nuts3) ------------------

### calculate days over threshold -----------

# note: as the functions add to the input dataframe, we can chain these together
## to produce one dataframe with separate columns
data_nuts3 <- hot.days.yr(data_nuts3, hottemp_val)
data_nuts3 <- cold.days.yr(data_nuts3, coldtemp_val)



### count of consecutive days over threshold nuts 3 -------------------
# ordering dataset
data_nuts3 = data_nuts3 %>%
  arrange(NUTS_ID, date)


# reshape data to split day functions into their own dataset in a list
binaryvar = data_nuts3 %>%
  select(starts_with("over"), starts_with("below")) #this finds the col names,
#since identical nuts2/nuts3 column names, we only need to do this once on one of them

binaryvar <- as.vector(names(binaryvar))

data_nuts3 <- pivot_longer(data_nuts3, cols=binaryvar, names_to = "temperature",
                           values_to = "temperature_bin")




data_nuts3.lst <- split(data_nuts3, data_nuts3$temperature)

rm(data_nuts3) #remove to save space in working environment



# # implement function
data_nuts3_wave.lst <- lapply(data_nuts3.lst, extremewaves.yr)

# binding rows and dropping extra variables
wave_nuts3 <- do.call("rbind", data_nuts3_wave.lst)

#remove the row names
wave_nuts3 <- remove_rownames(wave_nuts3)

### save ----------------------------
write_csv(wave_nuts3, file = export_nuts3)
 


