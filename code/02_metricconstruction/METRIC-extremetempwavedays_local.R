###############################################
# Constructing Extreme Temperature Waves using Local Threshold
# Date Last Modified: May 16 2025
################################################
# NOTES: (1) Following Chambers (2020) definition of heatwave days in a year
# (2) Reference period can be adjusted by user. Last year of reference period
# can be set manually under `Set hardcoded thresholds`. If the first year of the 
# reference period needs to be adjusted, user can filter the dataset before
# applying the functions

# Packages -------------------
library(tidyverse)
library(zoo)

# if needed, set working directory to TEE-dataset-main folder using below command
# setwd("")

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
# percentile cutoffs
hottemp_val <- c(.90, .95, .99)
coldtemp_val <- c(.10, .05, .01)


# last year of reference period
ref_year <- 1990
# first year of reference period 
ref_year_first <- 1961
# first year of data period
first_year <- 1979

# export filepaths
export_nuts2 <- "data/02_metrics/wave_local_nuts2.csv"
export_nuts3 <- "data/02_metrics/wave_local_nuts3.csv"

# Limit earliest year of data to calculate reference from ------------

data_nuts2 <- data_nuts2 %>%
  filter(year >= ref_year_first)

data_nuts3 <- data_nuts3 %>%
  filter(year >= ref_year_first)

# Compute metrics -----------------------------------

## functions -----------------

# function to count days over threshold
hot.days.yr <- function(df, percentile, ref_year){
  
  
  for (i in 1:length(percentile)){
    
    df_ref = df %>% 
      filter(year <= ref_year) %>% 
      group_by(NUTS_ID) %>% 
      mutate(threshold =  quantile(temp_max, percentile[i], na.rm=TRUE)) %>% 
      ungroup()  %>% 
      select(NUTS_ID, threshold) %>% 
      distinct()
    
    df <- left_join(df, df_ref, by=c("NUTS_ID"))
    
    df = df %>% 
      mutate(varname = if_else(temp_max >= threshold, 1, 0)) %>% 
      group_by(NUTS_ID, year) %>% 
      mutate(varname_count = sum(varname, na.rm=TRUE)) %>% 
      ungroup()
    
    names(df)[names(df) == "threshold"] <- paste0("threshold", as.character((percentile[i])))
    names(df)[names(df) == "varname"] <- paste0("over", as.character(percentile[i]))
    names(df)[names(df) == "varname_count"] <- paste0("daysover", as.character(percentile[i]), "_yr")
    
  }
  
  return(df)
  
}


# function for cold days per year using temp_max value

cold.days.yr <- function(df, percentile, ref_year){
  
  
  for (i in 1:length(percentile)){
    
    df_ref = df %>% 
      filter(year <= ref_year) %>% 
      group_by(NUTS_ID) %>% 
      mutate(threshold =  quantile(temp_max, percentile[i], na.rm=TRUE)) %>% 
      ungroup() %>% 
      select(NUTS_ID, threshold) %>% 
      distinct()
    
    df <- left_join(df, df_ref, by=c("NUTS_ID"))
    
    df = df %>% 
      mutate(varname = if_else(temp_max <= threshold, 1, 0)) %>% 
      group_by(NUTS_ID, year) %>% 
      mutate(varname_count = sum(varname, na.rm=TRUE)) %>% 
      ungroup()
    
    names(df)[names(df) == "threshold"] <- paste0("threshold", as.character((percentile[i])))
    names(df)[names(df) == "varname"] <- paste0("below", as.character(percentile[i]))
    names(df)[names(df) == "varname_count"] <- paste0("daysbelow", as.character(percentile[i]), "_yr")
    
  }
  
  return(df)
  
}






# building function that identifies heat waves
extremewaves.yr <- function(df){

  percentile <- unique(df$percentile) #which percentile threshold is used in calculation
  thresh <- gsub("[^0-9.]", "",
                   percentile) #drop characters from percentile to get the threshold number

  length <- 2
  
  # extract the temperature for the NUTS at this threshold
  ## FIX for threshold of 0.9 as `0.9` matches multiple columns
  if (thresh == "0.9"){
    temp_table <- df %>%
      select(NUTS_ID, threshold0.9) %>%
      unique()
  } else{
    temp_table <- df %>%
      select(NUTS_ID, contains(thresh)) %>%
      select(-contains("days")) %>% #remove the yearly count of days above/below threshold
      unique()
  }
  
  
  #rename 
  names(temp_table) <- c("NUTS_ID", "temperature_threshold")
  
  #remove columns from df not being used to save space
  df <- df %>%
    select(NUTS_ID, date, percentile_bin) #note `percentile_bin` is indicator of if date is over threshold

  df = df %>%
    group_by(NUTS_ID) %>%
    mutate(count = zoo::rollsum(percentile_bin, length, align="right", fill = NA)) %>%
    ungroup()

  df = df %>%
    mutate(date = as.Date(date)) %>%
    mutate(count = duration(count, "day")) %>% 
    arrange(NUTS_ID, date) %>%
    group_by(NUTS_ID) %>%
    mutate(consecdate = if_else((date - duration(1, "day")) == lag(date), 1, 0)) %>% 
    mutate(startdate = if_else(consecdate == 1 & lag(percentile_bin) == 1, date - 1, date)) %>%
    mutate(enddate = if_else(lead(percentile_bin) == 1 & lead(consecdate) == 1, lead(date), date)) %>%
    ungroup() %>%
    mutate(startdate = if_else(percentile_bin == 0 | consecdate == 0, NA, startdate)) %>%
    mutate(enddate = if_else(percentile_bin == 0 | consecdate == 0, NA, enddate))

  wave = df %>%
    filter(is.na(startdate) == FALSE) %>%
    select(NUTS_ID, startdate, enddate) %>%
    group_by(NUTS_ID) %>%
    mutate(indx = c(0, cumsum(as.numeric(lead(startdate)) > cummax(as.numeric(enddate)))[-n()])) %>%
    group_by(NUTS_ID, indx) %>%
    summarise(startdate = min(startdate),
              enddate = max(enddate)) %>%
    ungroup()

  wave = wave %>%
    mutate(year = year(startdate)) %>%
    mutate(wavelength = enddate - startdate) %>%
    select(-indx) #remove index as not needed any more

  
  
  # add percentile threshold as a column 
  wave$threshold <- percentile
  
  # add info on the threshold
  wave <- left_join(wave, temp_table)
  
  #split threshold column into two cols: above/below and percentile
  wave <- wave %>%
    mutate(percentile = gsub("[^0-9.]", "",
                             threshold),
           above_below = gsub("[^a-zA-Z]", "",
                              threshold)) %>%
    select(-threshold) %>%
    relocate(NUTS_ID, above_below, percentile, temperature_threshold, 
             year, startdate, enddate, wavelength)


  return(wave)

}





## run functions (nuts2) -----------------------

### calculate threshold and days over threshold -----------

# note: as the functions add to the input dataframe, we can chain these together
## to produce one dataframe with separate columns
data_nuts2 <- hot.days.yr(data_nuts2, hottemp_val, ref_year)
data_nuts2 <- cold.days.yr(data_nuts2, coldtemp_val, ref_year)

### keep only years to calculate waves for -------------
data_nuts2 <- data_nuts2 %>%
  filter(year >= first_year)


### count of consecutive days over threshold nuts 2 -------------------
# ordering dataset
data_nuts2 = data_nuts2 %>%
  arrange(NUTS_ID, date)


# reshape data to split day functions into their own dataset in a list
binaryvar = data_nuts2 %>%
  select(starts_with("over"), starts_with("below")) #this finds the col names,
  #since identical nuts2/nuts3 column names, we only need to do this once on one of them

binaryvar <- as.vector(names(binaryvar))

data_nuts2 <- pivot_longer(data_nuts2, cols=binaryvar, names_to = "percentile",
                           values_to = "percentile_bin")




data_nuts2.lst <- split(data_nuts2, data_nuts2$percentile)

rm(data_nuts2) #remove to save space in working environment



# # implement function
data_nuts2_wave.lst <- lapply(data_nuts2.lst, extremewaves.yr)

# binding rows and dropping extra variables
wave_nuts2 <- do.call("rbind", data_nuts2_wave.lst)

#remove the row names
wave_nuts2 <- remove_rownames(wave_nuts2)

### save file ----------------------------
write_csv(wave_nuts2, file = export_nuts2)

### remove objects to save space  --------------
rm(data_nuts2.lst, data_nuts2_wave.lst)

## run functions (nuts3) ------------------


### calculate threshold and days over threshold -----------
data_nuts3 <- hot.days.yr(data_nuts3, hottemp_val, ref_year) # these take aprox. 4 mins
data_nuts3 <- cold.days.yr(data_nuts3, coldtemp_val, ref_year)


### keep only years after reference period to calculate waves for -------------
data_nuts3 <- data_nuts3 %>%
  filter(year >= first_year)



### count of consecutive days over threshold nuts 3 -------------------
# ordering dataset
data_nuts3 = data_nuts3 %>%
  arrange(NUTS_ID, date)

# reshape data to split day functions into their own dataset in a list
binaryvar = data_nuts3 %>%
  select(starts_with("over"), starts_with("below")) #this finds the col names,
#since identical nuts2/nuts3 column names, we only need to do this once on one of them

binaryvar <- as.vector(names(binaryvar))
# reshape data to split day functions into their own dataset in a list
data_nuts3 <- pivot_longer(data_nuts3, cols=binaryvar, names_to = "percentile",
                           values_to = "percentile_bin")



data_nuts3.lst <- split(data_nuts3, data_nuts3$percentile)

rm(data_nuts3) #remove to save space in working environment



# # implement function
data_nuts3_wave.lst <- lapply(data_nuts3.lst, extremewaves.yr)

#remove to save space
rm(data_nuts3.lst)

# binding rows and dropping extra variables
wave_nuts3 <- do.call("rbind", data_nuts3_wave.lst)

#remove the row names
wave_nuts3 <- remove_rownames(wave_nuts3)

### save [UPDATE PATHS FOR FINAL REPO] ----------------------------
write_csv(wave_nuts3, file = export_nuts3)


