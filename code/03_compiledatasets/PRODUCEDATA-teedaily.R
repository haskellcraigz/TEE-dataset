#########################################
## Produce teedaily: daily min, max, mean temperature 1950-2021
## Last Modified: May 16 2025
########################################
## NOTES: 

# Packages ---------------

library(tidyverse)

# set working directory to TEE-dataset-main folder
setwd("")

# Load data --------------
## daily temperature for entire period
data_nuts3 <- read_csv("data/02_metrics/dailytemp_nuts3.csv")
data_nuts2 <- read_csv("data/02_metrics/dailytemp_nuts2.csv")

## daily utci
utci_nuts2 <- read_csv("data/02_metrics/dailyutci_nuts2.csv")
utci_nuts3 <- read_csv("data/02_metrics/dailyutci_nuts3.csv")

## daily pop. weighted temperature
popw_nuts2 <- read_csv("data/02_metrics/dailypoptemp_nuts2.csv")
popw_nuts3 <- read_csv("data/02_metrics/dailypoptemp_nuts3.csv")

# Initialize Values --------------
p1 <- c(1980:1989)
p2 <- c(1990:1999)
p3 <- c(2000:2009)
p4 <- c(2010:2019)
p5 <- c(2020:2024)


# Clean -----------------------
data_nuts3 <- data_nuts3[, -1] %>%
  mutate(scale = "NUTS3") %>%
  relocate(scale, NUTS_ID, date, temp_min, temp_max, temp_mean) 

data_nuts2 <- data_nuts2[, -1] %>%
  mutate(scale = "NUTS2") %>%
  relocate(scale, NUTS_ID, date, temp_min, temp_max, temp_mean) 

## Filter to last day of 2024 -------------------

data_nuts2 <- data_nuts2 %>%
  filter(date < lubridate::ymd("2025-01-01"))

data_nuts3 <- data_nuts3 %>%
  filter(date < lubridate::ymd("2025-01-01"))


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

# dropping far away islands
data_nuts2 = data_nuts2 %>% 
  filter(is.na(temp_mean) == FALSE)

data_nuts3 <- data_nuts3 %>%
  rename(nuts_id = NUTS_ID) %>%
  relocate(scale, nuts_id, date, temp_mean, temp_min, temp_max,
           temp_mean_pop_weighted, temp_min_pop_weighted, temp_max_pop_weighted,
           utci_mean, utci_min, utci_max) %>%
  arrange(nuts_id, date)

# dropping far away islands
data_nuts3 = data_nuts3 %>% 
  filter(is.na(temp_mean) == FALSE)

# Save [UPDATE FILE PATH] ----------------

## break data into smaller periods and export
# 1950-1979 (daily min/mean/max only)
data_nuts2_1950 = data_nuts2 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year < 1980) %>% 
  select(scale:temp_max)

write.csv(data_nuts2_1950, file = "data/03_fulldatasets/teedaily_nuts2_1950-1979.csv")

rm(data_nuts2_1950)

data_nuts3_1950 = data_nuts3 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year < 1980) %>% 
  select(scale:temp_max)

write.csv(data_nuts3_1950, file = "data/03_fulldatasets/teedaily_nuts3_1950-1979.csv")

rm(data_nuts3_1950)


# 1980s
data_nuts2_1980 = data_nuts2 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p1) %>% 
  select(-c(temp_mean_pop_weighted, temp_min_pop_weighted, temp_max_pop_weighted))

write.csv(data_nuts2_1980, file = "data/03_fulldatasets/teedaily_nuts2_1980-1989.csv")

rm(data_nuts2_1980)

data_nuts3_1980 = data_nuts3 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p1) %>% 
  select(-c(temp_mean_pop_weighted, temp_min_pop_weighted, temp_max_pop_weighted))

write.csv(data_nuts3_1980, file = "data/03_fulldatasets/teedaily_nuts3_1980-1989.csv")

rm(data_nuts3_1980)

# 1990s
data_nuts2_1990 = data_nuts2 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p2) %>% 
  select(-c(temp_mean_pop_weighted, temp_min_pop_weighted, temp_max_pop_weighted))

write.csv(data_nuts2_1990, file = "data/03_fulldatasets/teedaily_nuts2_1990-1999.csv")

rm(data_nuts2_1990)

data_nuts3_1990 = data_nuts3 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p2) %>% 
  select(-c(temp_mean_pop_weighted, temp_min_pop_weighted, temp_max_pop_weighted))

write.csv(data_nuts3_1990, file = "data/03_fulldatasets/teedaily_nuts3_1990-1999.csv")

rm(data_nuts3_1990)

# 2000s
data_nuts2_2000 = data_nuts2 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p3)

write.csv(data_nuts2_2000, file = "data/03_fulldatasets/teedaily_nuts2_2000-2009.csv")

rm(data_nuts2_2000)

data_nuts3_2000 = data_nuts3 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p3)

write.csv(data_nuts3_2000, file = "data/03_fulldatasets/teedaily_nuts3_2000-2009.csv")

rm(data_nuts3_2000)

# 2010s
data_nuts2_2010 = data_nuts2 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p4)

write.csv(data_nuts2_2010, file = "data/03_fulldatasets/teedaily_nuts2_2010-2019.csv")

rm(data_nuts2_2010)

data_nuts3_2010 = data_nuts3 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p4)

write.csv(data_nuts3_2010, file = "data/03_fulldatasets/teedaily_nuts3_2010-2019.csv")

rm(data_nuts3_2010)

# 2020s
data_nuts2_2020 = data_nuts2 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p5)

write.csv(data_nuts2_2020, file = "data/03_fulldatasets/teedaily_nuts2_2020-2024.csv")

rm(data_nuts2_2020)

data_nuts3_2020 = data_nuts3 %>% 
  mutate(year = year(date)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% p5)

write.csv(data_nuts3_2020, file = "data/03_fulldatasets/teedaily_nuts3_2020-2024.csv")

rm(data_nuts3_2020)









