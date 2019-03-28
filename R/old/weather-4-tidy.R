setwd("weather")

library(tidyverse)
library(lubridate)


data <- readRDS("data/3-data-import.RDS")
stations <- readRDS("data/2-stations-filtered.RDS")
locations <- readRDS("data/2-locations-filtered.RDS")
stations_to_locs <- readRDS("data/2-stations-to-locs.RDS")



# Stattion check ----------------------------------------------------------

stations_ok <- data %>%
  mutate(year = year(date)) %>% 
  select(usaf,wban,year,date) %>% 
  distinct() %>% 
  group_by(usaf,wban,year) %>% 
  count() %>% 
  filter(n > 365 * 0.8)


data_filtered <- data %>% 
  mutate(year = year(date)) %>% 
  semi_join(stations_ok, by = c("usaf", "wban", "year"))

data_summed <- stations_to_locs %>% 
  filter(match == "exact") %>% 
  
  ##this appends a list of stations again, but now all are called "buffer" now including both precise and buffer
  union_all(stations_to_locs %>% mutate(match = "approximate")) %>% 
  inner_join(data_filtered, by = c("usaf", "wban")) %>% 
  select(-c(usaf, wban, csafp, year)) %>% 
  group_by(cbsafp, match, date) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  arrange(cbsafp, date, desc(match)) %>% 
  ungroup() 


data_gather <- data_summed %>%
  ##using summarise_all(funs(first(na.omit(.)))) took too long, therefore I gather/stread/mutate/spread
  gather(metric, value, -c(cbsafp:date)) %>% 
  spread(match, value) %>% 
  mutate(approximate = if_else(is.nan(approximate) | is.infinite(approximate) | is.na(approximate), NA_real_, approximate),
         value = if_else(is.nan(exact) | is.infinite(exact) | is.na(exact), approximate, exact)) %>% 
  select(-exact, -approximate)
  

data_spread <- data_gather %>% 
  spread(metric, value) %>% 
  replace_na(list(snow = 0, precip = 0)) %>% 
  mutate(temp_max = if_else(is.na(temp_max) & is.na(temp_min), temp_mean,
                            if_else(is.na(temp_max), 2*temp_mean - temp_min, temp_max)),
         temp_min = if_else(is.na(temp_max) & is.na(temp_min), temp_mean, 
                            if_else(is.na(temp_min), 2*temp_mean - temp_max, temp_min))) %>% 
  #mutate(is_na = if_else(rowSums(is.na(.)) > 0, 1, 0)) %>% 
                            {.}

saveRDS(data_spread, file = "data/4-data-tidied.RDS")
