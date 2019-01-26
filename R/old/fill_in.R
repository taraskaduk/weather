# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(scales)

setwd("weather")

# Import ------------------------------------------------------------------

load("data/1-import.RData")

# Data --------------------------------------------------------------------
stations_join <- stations_us %>% 
  select(usaf, wban, lat, lon)

weather <- weather_data %>% 
  inner_join(stations_join, by = c('stn' = 'usaf', 'wban'))

w_mutated <- weather %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0),
         
         year = year(date),
         month = month(date),
         day = day(date),
         day = if_else(day == 29 & month == 2, 28, as.numeric(day)),
         date = date(ISOdate(year,month,day))
         ) %>% 
  select(-c(lat, lon, stn, wban)) %>% 
  group_by(lat.0, lon.0, date, year, month, day) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  purrr::map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', 'pressure', 'wind', 'gust'), ~ifelse(is.nan(.x), NA, .x)) %>% 
  bind_rows()


check_completion <- w_mutated %>% 
  group_by(lat.0, lon.0, year) %>% 
  summarise(n = n()) %>% 
  filter(n >= 365*0.9)

coords <- check_completion %>% 
  select(lat.0, lon.0, year) %>% 
  distinct()

dummy <- tibble(date = seq.Date(date("2012-01-01"), date("2017-12-31"), by = "day")) %>% 
  filter(!(day(date)==29 & month(date)==2)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  merge(coords, all = TRUE)

w_expanded <- w_mutated %>% 
  semi_join(check_completion, by = c("lat.0", "lon.0", "year")) %>% 
  full_join(dummy, by = c("date", "lat.0", "lon.0", "year", "month", "day")) %>% 
  arrange(lat.0, lon.0, date)

w_missing <- w_expanded %>% 
  filter(is.na(temp_mean))

w_filldown <- w_expanded %>% 
  fill(temp_mean, temp_min, temp_max, precip, snow, wind, gust, 
       is_rain, is_tornado, is_hail, is_snow,  .direction = "down") %>% 
  semi_join(w_missing, by = c("date", "lat.0", "lon.0"))

w_fillup <- w_expanded %>% 
  fill(temp_mean, temp_min, temp_max, precip, snow, wind, gust, 
       is_rain, is_tornado, is_hail, is_snow, .direction = "up") %>% 
  semi_join(w_missing, by = c("date", "lat.0", "lon.0"))

w_fill_mean <- union_all(w_filldown, w_fillup) %>% 
  group_by(lat.0, lon.0, date, year, month, day) %>% 
  summarise_all(mean, na.rm=TRUE)

w_filled <- w_expanded %>% 
  anti_join(w_missing, by = c("date", "lat.0", "lon.0")) %>% 
  union_all(w_fill_mean) %>% 
  purrr::map_at(c('is_rain', 'is_snow', 'is_hail', 'is_tornado', 'is_fog', 'is_thunder'), ~if_else(.x < 0.1, 0, 1)) %>% 
  bind_rows()
  

save(w_filled, file = "data/2-tidy.RData")
