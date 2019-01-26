setwd("weather")

library(RANN)
library(caret)

library(lubridate)
library(tidyverse)

load("data/locations.RData")
load("data/stations.RData")
load("data/data.RData")

weather_by_cbsa <- data %>% 
  inner_join(stations %>% select(usaf, wban, cbsafp), by = c("usaf", "wban")) %>% 
  group_by(cbsafp, date) %>% 
  summarise(temp_mean = mean(temp_mean, na.rm = TRUE),
            temp_min = mean(temp_min, na.rm = TRUE),
            temp_max = mean(temp_max, na.rm = TRUE),
            precip = max(precip, na.rm = TRUE),
            snow = max(snow, na.rm = TRUE),
            is_rain = max(is_rain, na.rm = TRUE),
            is_snow = max(is_snow, na.rm = TRUE)) %>% 
  ungroup() %>% 
  purrr::map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', "is_rain", "is_snow"), ~ifelse(is.nan(.x) | is.infinite(.x), NA, .x)) %>% 
  bind_rows() %>% 
  replace_na(list(snow = 0, precip = 0)) %>% 
  mutate(temp_max = if_else(is.na(temp_max) & is.na(temp_min), temp_mean,
                            if_else(is.na(temp_max), 2*temp_mean - temp_min, temp_max)),
         temp_min = if_else(is.na(temp_max) & is.na(temp_min), temp_mean, 
                            if_else(is.na(temp_min), 2*temp_mean - temp_max, temp_min))) %>% 
  #mutate(is_na = if_else(rowSums(is.na(.)) > 0, 1, 0)) %>% 
  {.}
  
  
  
dummy <- stations %>% 
  select(cbsafp) %>% 
  distinct() %>% 
  merge(tibble(date = seq.Date(date("2013-01-01"), date("2017-12-31"), by = "day")), all = TRUE) 


missing <- dummy %>% 
  left_join(weather_by_cbsa, by = c("cbsafp", "date")) %>% 
  filter(is.na(temp_mean)) %>% 
  group_by(cbsafp) %>% 
  count()



locations_missing <- locations %>% inner_join(missing, by = "cbsafp")




data <- weather_by_cbsa


## Checkpoint
save(data, file = "data/2-tidy-data.RData")
save(locations, file = "data/locations.RData")
