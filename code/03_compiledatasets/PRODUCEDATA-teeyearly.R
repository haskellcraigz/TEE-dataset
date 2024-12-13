#########################################
## Produce teeyearly: yearly metrics 1980-2021
## Last Modified: Dec 13 2024
########################################

# Packages ---------------

library(tidyverse)

# Load data [UPDATE FILE PATH] --------------

files <- list.files(path = "~/TEEDataset/data/02_metrics/", full.names = TRUE)

metrics_c <- c("cdd", "utciyearly", "expected_threshold", "percentilethreshold",
               "tropical_nights")


## init dataframes to save to -------------
df_nuts2 <- tibble()

df_nuts3 <- tibble()

## load files ---------------
for (f in files){
  
  if (any(str_detect(f, metrics_c))){ #csv file is of yearly metrics
    
    if (str_detect(f, "nuts2")){ #nuts 2
      
      #check if output dataframes are empty
      if (dim(df_nuts2)[1] == 0){
        df_nuts2 <- read_csv(f) #load csv file and replace df_nuts2 with this data
      } else{
        temp <- read_csv(f) #load data
        df_nuts2 <- left_join(df_nuts2, temp,
                              by = c("NUTS_ID", "year")) #join to existing data
      }
    }
    
    if(str_detect(f, "nuts3")){ #nuts 3
      
      #check if output dataframes are empty
      if (dim(df_nuts3)[1] == 0){
        df_nuts3 <- read_csv(f) #load csv file and replace df_nuts2 with this data
      } else{
        temp <- read_csv(f) #load data
        df_nuts3 <- left_join(df_nuts3, temp,
                              by = c("NUTS_ID", "year")) #join to existing data
      }
    }
    

    
  }
}



# Clean up and standardize column names ---------------------------


df_nuts2 <- df_nuts2 %>%
  # add scale as variable
  mutate(scale = "NUTS2") %>%
  relocate(scale, .before = NUTS_ID) %>%
  #rename columns for consistency
  rename(nuts_id = NUTS_ID,
         prob_below_neg20 = `prob_below_-20`,
         prob_below_neg15 = `prob_below_-15`,
         prob_below_neg10 = `prob_below_-10`,
         prob_below_neg5 = `prob_below_-5`,
         
         days_below_neg20 = `num_below_-20`,
         days_below_neg15 = `num_below_-15`,
         days_below_neg10 = `num_below_-10`,
         days_below_neg5 = `num_below_-5`,
         days_below_0 = `num_below_0`,
         days_below_5 = `num_below_5`,
         days_below_10 = `num_below_10`,
         
         days_above_20 = num_above_20,
         days_above_25 = num_above_25,
         days_above_30 = num_above_30,
         days_above_35 = num_above_35,
         days_above_40 = num_above_40,
         days_above_45 = num_above_45,
         days_above_50 = num_above_50,
         
         temp_threshold_0.90 = threshold0.9,
         temp_threshold_0.95 = threshold0.95,
         temp_threshold_0.99 = threshold0.99,
         temp_threshold_0.01 = threshold0.01,
         temp_threshold_0.05 = threshold0.05,
         temp_threshold_0.10 = threshold0.1,
         
         days_above_0.90 = daysover0.9_yr,
         days_above_0.95 = daysover0.95_yr,
         days_above_0.99 = daysover0.99_yr,
         days_below_0.01 = daysbelow0.01_yr,
         days_below_0.05 = daysbelow0.05_yr,
         days_below_0.10 = daysbelow0.1_yr) %>%
  
  #filter to only 1980 and onwards
  filter(year >= 1980) %>%
  #order dataset
  arrange(nuts_id, year)



df_nuts3 <- df_nuts3 %>%
  # add scale as variable
  mutate(scale = "NUTS3") %>%
  relocate(scale, .before = NUTS_ID) %>%
  #rename columns for consistency
  rename(nuts_id = NUTS_ID,
         prob_below_neg20 = `prob_below_-20`,
         prob_below_neg15 = `prob_below_-15`,
         prob_below_neg10 = `prob_below_-10`,
         prob_below_neg5 = `prob_below_-5`,
         
         days_below_neg20 = `num_below_-20`,
         days_below_neg15 = `num_below_-15`,
         days_below_neg10 = `num_below_-10`,
         days_below_neg5 = `num_below_-5`,
         days_below_0 = `num_below_0`,
         days_below_5 = `num_below_5`,
         days_below_10 = `num_below_10`,
         
         days_above_20 = num_above_20,
         days_above_25 = num_above_25,
         days_above_30 = num_above_30,
         days_above_35 = num_above_35,
         days_above_40 = num_above_40,
         days_above_45 = num_above_45,
         days_above_50 = num_above_50,
         
         temp_threshold_0.90 = threshold0.9,
         temp_threshold_0.95 = threshold0.95,
         temp_threshold_0.99 = threshold0.99,
         temp_threshold_0.01 = threshold0.01,
         temp_threshold_0.05 = threshold0.05,
         temp_threshold_0.10 = threshold0.1,
         
         days_above_0.90 = daysover0.9_yr,
         days_above_0.95 = daysover0.95_yr,
         days_above_0.99 = daysover0.99_yr,
         days_below_0.01 = daysbelow0.01_yr,
         days_below_0.05 = daysbelow0.05_yr,
         days_below_0.10 = daysbelow0.1_yr) %>%
  
  #filter to only 1980 and onwards
  filter(year >= 1980) %>%
  #order dataset
  arrange(nuts_id, year)






# save [FILE PATHS TO BE UPDATED] ----------------------------

write_csv(df_nuts2, 
          "~/TEEDataset/data/03_fulldatasets/teeyearly_nuts2.csv")
write_csv(df_nuts3, 
          "~/TEEDataset/data/03_fulldatasets/teeyearly_nuts3.csv")
