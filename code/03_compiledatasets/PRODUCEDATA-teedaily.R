#########################################
## Produce teedaily: daily min, max, mean temperature 1950-2021
## Last Modified: Dec 13 2024
########################################
## NOTES: 

# Packages ---------------

library(tidyverse)

# Load data [UPDATE FILE PATH] --------------
## daily temperature for entire period
data_nuts3 <- read_csv("~/TEEDataset/data/02_metrics/dailytemp_nuts3.csv")
data_nuts2 <- read_csv("~/TEEDataset/data/02_metrics/dailytemp_nuts2.csv")

# daily utci
utci_nuts2 <- read_csv("~/TEEDataset/data/02_metrics/dailyutci_nuts2.csv")
utci_nuts3 <- read_csv("~/TEEDataset/data/02_metrics/dailyutci_nuts3.csv")

#daily pop. weighted temperature
popw_nuts2 <- read_csv("~/TEEDataset/data/02_metrics/dailypoptemp_nuts2.csv")
popw_nuts3 <- read_csv("~/TEEDataset/data/02_metrics/dailypoptemp_nuts3.csv")


# Clean -----------------------
data_nuts3 <- data_nuts3[, -1] %>%
  mutate(scale = "NUTS3") %>%
  relocate(scale, NUTS_ID, date, temp_min, temp_max, temp_mean)

data_nuts2 <- data_nuts2[, -1] %>%
  mutate(scale = "NUTS2") %>%
  relocate(scale, NUTS_ID, date, temp_min, temp_max, temp_mean)

## Filter to last day of 2021 -------------------

data_nuts2 <- data_nuts2 %>%
  filter(date < lubridate::ymd("2022-01-01"))

data_nuts3 <- data_nuts3 %>%
  filter(date < lubridate::ymd("2022-01-01"))


# Combine with UTCI and pop. weighted temperature -------------
## NUTS2
data_nuts2 <- left_join(data_nuts2,
                        popw_nuts2)

data_nuts2 <- select(data_nuts2, -`...1`) #remove excess row id col

data_nuts2 <- left_join(data_nuts2,
                        utci_nuts2)

data_nuts2 <- select(data_nuts2, -`...1`) #remove excess row id col


## NUTS3
data_nuts3 <- left_join(data_nuts3,
                        popw_nuts3)

data_nuts3 <- select(data_nuts3, -`...1`) #remove excess row id col

data_nuts3 <- left_join(data_nuts3,
                        utci_nuts3)

data_nuts3 <- select(data_nuts3, -`...1`) #remove excess row id col

## arrange rows and columns ----------------

data_nuts2 <- data_nuts2 %>%
  rename(nuts_id = NUTS_ID) %>%
  relocate(scale, nuts_id, date, temp_mean, temp_min, temp_max,
           temp_mean_pop_weighted, temp_min_pop_weighted, temp_max_pop_weighted,
           utci_mean, utci_min, utci_max) %>%
  arrange(nuts_id, date)


data_nuts3 <- data_nuts3 %>%
  rename(nuts_id = NUTS_ID) %>%
  relocate(scale, nuts_id, date, temp_mean, temp_min, temp_max,
           temp_mean_pop_weighted, temp_min_pop_weighted, temp_max_pop_weighted,
           utci_mean, utci_min, utci_max) %>%
  arrange(nuts_id, date)

# Save [UPDATE FILE PATH] ----------------

write_csv(data_nuts2, file = "~/TEEDataset/data/03_fulldatasets/teedaily_nuts2.csv")
write_csv(data_nuts3, file = "~/TEEDataset/data/03_fulldatasets/teedaily_nuts2.csv")









