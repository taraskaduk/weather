# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)

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
         flag = "existing",
         day = if_else(day(date) == 29 & month(date) == 2, 28, as.numeric(day(date))),
         date = date(ISOdate(year(date),month(date),day))
         ) %>% 
  select(-c(lat, lon, stn, wban, day)) %>% 
  group_by(lat.0, lon.0, flag, date) %>% 
  summarise_all(funs(mean, max), na.rm = TRUE) %>% 
  select(lat.0:date,
         temp_mean = temp_mean_mean,
         temp_min = temp_min_mean,
         temp_max = temp_max_mean,
         precip = precip_max,
         snow = snow_max,
         pressure = pressure_mean,
         wind = wind_mean,
         gust = gust_mean,
         is_fog = is_fog_max,
         is_rain = is_rain_max,
         is_snow = is_snow_max,
         is_hail = is_hail_max,
         is_thunder = is_thunder_max,
         is_tornado = is_tornado_max) %>% 
  ungroup() %>% 
  purrr::map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', 'pressure', 'wind', 'gust'), ~ifelse(is.nan(.x) | is.infinite(.x), NA, .x)) %>% 
  bind_rows()


coords <- w_mutated %>% 
  select(lat.0, lon.0) %>% 
  distinct()

dummy <- tibble(date = seq.Date(date("2012-01-01"), date("2017-12-31"), by = "day")) %>% 
  filter(!(day(date)==29 & month(date)==2)) %>% 
  mutate(flag = "missing") %>% 
  merge(coords, all = TRUE)

data_missing <- dummy %>% 
  anti_join(w_mutated, by = c("lat.0","lon.0","date"))

data_all <- w_mutated %>% 
  union_all(data_missing) %>% 
  mutate(yday = if_else(leap_year(date) & yday(date) > 60, yday(date) - 1, yday(date)),
         year = year(date))

data_all %>% group_by(flag) %>% count()

save(data_all, file = "data/2-tidy.RData")
