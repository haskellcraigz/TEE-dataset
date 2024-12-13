################################################
## Constructing UTCI heat stress days 
## Date Last Modified: Dec 13 2024 
###############################################
## NOTES: (1) UTCI data only available 1980 - 2020
## (2) UTCI of at least 26 C is moderate heat stress while a UTCI
# above 32 is severe heat stress (see https://cds.climate.copernicus.eu/cdsapp#!/dataset/derived-utci-historical?tab=overview)



# Packages [FILE PATH UPDATED] ----------------
library(tidyverse)

# Load data [FILE PATH UPDATED] ---------------
data_nuts2 <- read.csv(file = "~/TEEDataset/data/02_metrics/dailyutci_nuts2.csv")
data_nuts3 <- read_csv(file = "~/TEEDataset/data/02_metrics/dailyutci_nuts3.csv")


# Reshape data frames and add heat stress column ----------------------------------

# Nuts 2 
data_nuts2_processed <- data_nuts2 %>% 
  #add thresholds
  mutate(heatstress_mod26 = if_else(utci_mean >= 26, 1, 0),
         heatstress_sev32 = if_else(utci_mean >= 32, 1, 0),
         coldstress_mod0 = if_else(utci_mean <= 0, 1, 0),
         coldstress_sevm13 = if_else(utci_mean <= -13, 1, 0)) %>%
  # compute number of days in a year
  mutate(year = lubridate::year(date)) %>% 
  group_by(NUTS_ID, year) %>% 
  mutate(utci_above_26= sum(heatstress_mod26)) %>% 
  mutate(utci_above_32 = sum(heatstress_sev32)) %>% 
  mutate(utci_below_0 = sum(coldstress_mod0)) %>% 
  mutate(utci_below_neg13 = sum(coldstress_sevm13)) %>% 
  ungroup() %>%
  #keep only yearly data
  select(NUTS_ID, year, starts_with("utci_below"), starts_with("utci_above")) %>%
  unique()


# NUTS 3
data_nuts3_processed <- data_nuts3 %>% 
  mutate(heatstress_mod26 = if_else(utci_mean >= 26, 1, 0),
         heatstress_sev32 = if_else(utci_mean >= 32, 1, 0),
         coldstress_mod0 = if_else(utci_mean <= 0, 1, 0),
         coldstress_sevm13 = if_else(utci_mean <= -13, 1, 0)) %>%
  # compute number of days in a year
  mutate(year = lubridate::year(date)) %>% 
  group_by(NUTS_ID, year) %>% 
  mutate(utci_above_26= sum(heatstress_mod26)) %>% 
  mutate(utci_above_32 = sum(heatstress_sev32)) %>% 
  mutate(utci_below_0 = sum(coldstress_mod0)) %>% 
  mutate(utci_below_neg13 = sum(coldstress_sevm13)) %>% 
  ungroup() %>%
  #keep only yearly data
  select(NUTS_ID, year, starts_with("utci_below"), starts_with("utci_above")) %>%
  unique()

# Save [UPDATE FILE PATHS] ---------------------

write_csv(data_nuts2_processed, 
          file = "~/TEEDataset/data/02_metrics/utciyearly_nuts2.csv")


write_csv(data_nuts3_processed, 
          file = "~/TEEDataset/data/02_metrics/utciyearly_nuts3.csv")



