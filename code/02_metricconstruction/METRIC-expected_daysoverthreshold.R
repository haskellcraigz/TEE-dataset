###############################################
# Data Paper - (Expected) Days over threshold
# Date Last Updated: May 16 2025
################################################
# Notes:
# 

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
# temperature
extreme_heat <- seq(20,50, by = 5)
extreme_cold <- seq(-20,10, by = 5)

# first year of data period
first_year <- 1980

# export filepaths
export_nuts3 <- "data/02_metrics/expected_threshold_nuts3.csv"
export_nuts2 <- "data/02_metrics/expected_threshold_nuts2.csv"

# Compute metric -------------

## functions -----------------
## Uses the empirical cumulative distribution function to 
## calculate probability of experiencing day hotter (colder)
## than cut-off threshold
empirical_temp <- function(data, temp_hot, temp_cold){
  # Input: data - dataframe of daily temperature data (must be called
  #               temp_mean)
  # Input: temp_hot - vector of temperature thresholds at which to calculate 
  #                   probably >= hot temperature
  # Input: temp_cold - vector of temperature thresholds at which to calculate 
  #                   probably <= cold temperature
  #
  # Uses the empirical ecdf function, Fn = ecdf(x) --> F(x <= T)
  #
  # Output: vector of probabilities of temperature occurrence at each value in prob_temps
  
  #init output vector of temperature thresholds and probabilities
  out_thresholds <- c(temp_hot, temp_cold) # hot first then cold
  out_probs <- rep(NA, length(out_thresholds))
  
  # catch missign data
  if (sum(is.na(data$temp_mean)) == length(data$temp_mean)){
    print("No temperature data for:")
    print(unique(data$NUTS_ID))
    print(min(year(data$date)))
    return(out_probs)
  }
  
  #calculate empirical function Fn
  Fn <- ecdf(data$temp_mean)
  
  # init counter for out_probs
  i <- 1
  
  for (t in temp_hot){ #iterate through hot temps first
    prob <- 1 - Fn(t) # P[X>x]
    out_probs[i] <- prob
    
    i <- i + 1 #update counter
  }
  
  for (t in temp_cold){ #iterate through cold temps
    prob <- Fn(t) # P[X<=x]
    out_probs[i] <- prob
    
    i <- i + 1 #update counter
  }
  
  return(out_probs)
}



threshold_days <- function(data, temp_hot, temp_cold){
  # Input: data - dataframe of daily temperature data (must be called
  #               temp_mean)
  # Input: temp_hot - vector of temperature thresholds at which to calculate 
  #                   probably >= hot temperature
  # Input: temp_cold - vector of temperature thresholds at which to calculate 
  #                   probably <= cold temperature
  #
  # Output: vector of number of days over/below temp threshold
  
  #init output vector of temperature thresholds and number of days
  out_thresholds <- c(temp_hot, temp_cold) # hot first then cold
  out_num <- rep(NA, length(out_thresholds))
  
  # catch missign data
  if (sum(is.na(data$temp_mean)) == length(data$temp_mean)){
    print("No temperature data for:")
    print(unique(data$NUTS_ID))
    print(min(year(data$date)))
    return(out_num)
  }
  
  
  # init counter for out_probs
  i <- 1
  
  for (t in temp_hot){ #iterate through hot temps first
    
    out_num[i] <- sum(data$temp_mean > t)
    
    i <- i + 1 #update counter
  }
  
  for (t in temp_cold){ #iterate through cold temps
    out_num[i] <- sum(data$temp_mean < t)
    
    i <- i + 1 #update counter
  }
  
  return(out_num)
}

# Run Functions  --------------

## Clean data -----------------

## filter to study period ------------
data_nuts2 <- data_nuts2 %>%
  filter(year >= first_year)

data_nuts3 <- data_nuts3 %>%
  filter(year >= first_year)

## split to list by NUTS, year 
nuts2_ls <- split(data_nuts2, f ~ NUTS_ID + year)
nuts3_ls <- split(data_nuts3, f ~ NUTS_ID + year)


## Empirical probability ------------------



## Calculate probabilities 
## Apply functions
nuts2_empirical_ls <- lapply(X = nuts2_ls,
                          FUN = empirical_temp,
                          temp_hot = extreme_heat,
                          temp_cold = extreme_cold) #apply function to each df in list

nuts3_empirical_ls <- lapply(X = nuts3_ls,
                             FUN = empirical_temp,
                             temp_hot = extreme_heat,
                             temp_cold = extreme_cold) #apply function to each df in list


## Re-bind to table format 

# function for going from vector to wide-format tibble
tibblize_prob_vector <- function(probs, id, temp_thresholds){
  # Input: probs - vector of probabilities at each threshold
  # Input: temp_thresholds - the vector of threshold we are considering, in order
  # Input: id - the region/era combination ID for this dataframe
  #
  # Output: tibble with columns region, era, temp_threshold, probability
  
  #split name into region vs year
  region <- str_split(id, pattern = "\\.")[[1]][1]
  year <- str_split(id, pattern = "\\.")[[1]][2]
  
  ## create column names
  loc <- ifelse(temp_thresholds <= 10, "below", "above") #hot/cold is above/below
  
  nm <- str_c("prob", loc, sep ="_")
  nm <- str_c(nm, temp_thresholds, sep = "_")
  
  row1 <- as_tibble(rbind(nm, probs)) #init tibble
  names(row1) <- nm #update column names
  tb <- row1[-1,] #remove column names in first row
  
  
  # create tibble
  tb$NUTS_ID <- region #add region
  tb$year <- year #add era

  
  
  return(tb)
}

# vector of temperature thresholds, in order from '[x]_temp' functions
thresholds <- c(extreme_heat, extreme_cold)

# convert to list of tibbles
nuts2_tbls <- mapply(FUN = tibblize_prob_vector,
                               nuts2_empirical_ls, names(nuts2_empirical_ls),
                               MoreArgs = list(temp_thresholds = thresholds),
                               SIMPLIFY = F)

nuts3_tbls <- mapply(FUN = tibblize_prob_vector,
                     nuts3_empirical_ls, names(nuts3_empirical_ls),
                     MoreArgs = list(temp_thresholds = thresholds),
                     SIMPLIFY = F)

# bind into single dataframe
nuts2_df <- do.call("rbind", nuts2_tbls)
nuts3_df <- do.call("rbind", nuts3_tbls)


## Days over threshold ----------------------------

## Apply functions
nuts2_number_ls <- lapply(X = nuts2_ls,
                             FUN = threshold_days,
                             temp_hot = extreme_heat,
                             temp_cold = extreme_cold) #apply function to each df in list

nuts3_number_ls <- lapply(X = nuts3_ls,
                             FUN = threshold_days,
                             temp_hot = extreme_heat,
                             temp_cold = extreme_cold) #apply function to each df in list


## Re-bind to table format 

# function for going from vector to wide-format tibble
tibblize_num_vector <- function(num, id, temp_thresholds){
  # Input: num - vector of number of days at each threshold
  # Input: temp_thresholds - the vector of threshold we are considering, in order
  # Input: id - the region/era combination ID for this dataframe
  #
  # Output: tibble with columns region, era, temp_threshold, probability
  
  #split name into region vs year
  region <- str_split(id, pattern = "\\.")[[1]][1]
  year <- str_split(id, pattern = "\\.")[[1]][2]
  
  ## create column names
  loc <- ifelse(temp_thresholds <= 10, "below", "above") #hot/cold is above/below
  
  nm <- str_c("num", loc, sep ="_")
  nm <- str_c(nm, temp_thresholds, sep = "_")
  
  row1 <- as_tibble(rbind(nm, num)) #init tibble
  names(row1) <- nm #update column names
  tb <- row1[-1,] #remove column names in first row
  
  
  # create tibble
  tb$NUTS_ID <- region #add region
  tb$year <- year #add era
  
  
  
  return(tb)
}

# vector of temperature thresholds, in order from '[x]_temp' functions
thresholds <- c(extreme_heat, extreme_cold)

# convert to list of tibbles
nuts2_tbls <- mapply(FUN = tibblize_num_vector,
                     nuts2_number_ls, names(nuts2_number_ls),
                     MoreArgs = list(temp_thresholds = thresholds),
                     SIMPLIFY = F)

nuts3_tbls <- mapply(FUN = tibblize_num_vector,
                     nuts3_number_ls, names(nuts3_number_ls),
                     MoreArgs = list(temp_thresholds = thresholds),
                     SIMPLIFY = F)

# bind into single dataframe
nuts2_num_df <- do.call("rbind", nuts2_tbls)
nuts3_num_df <- do.call("rbind", nuts3_tbls)




## join and save -----------------------

all_nuts2 <- left_join(nuts2_df, nuts2_num_df)
all_nuts3 <- left_join(nuts3_df, nuts3_num_df)

write_csv(all_nuts2, file = export_nuts2)


write_csv(all_nuts3, file = export_nuts3)


