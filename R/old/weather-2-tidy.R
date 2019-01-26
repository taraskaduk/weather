setwd("weather")

library(RANN)
library(caret)
library(lubridate)
library(tidyverse)

load("data/1-import.RData")

# 2.1 Tidy -----------------------------------------------------------------

# stations_join <- stations_us %>% 
#   select(usaf, wban, lat, lon)

# data_coords <- data_weather %>% 
#   inner_join(stations_join, by = c('stn' = 'usaf', 'wban'))



## Previously, I was trying to get average readings for 3-5 closest stations to each city, and was using RANN's ANN algo to find the nearest neighbors,
## but also to predict the missing values.
## I was getting some weird results, particularly for Philly, where my outcome was ZERO rainy or snowy days in 5 years.
## (I was isolating Philly early - I wasn't getting the same results)

## I am thinking I should do something else now - treat each metric as an observation, and for each day find




# add st index - we'll need it later for joining

locations <- locations %>% 
  filter(type == "Metro Area") %>% 
  ##filter(str_detect(name, "San Francisco") == TRUE) %>% 
  mutate(index_loc = as.integer(rownames(.)))

data_weather$date <- as.Date(data_weather$date)


stations_stable <- data_weather %>% 
  group_by(stn, wban, year(date)) %>% 
  count() %>% 
  group_by(stn, wban) %>% 
  summarise(min = min(n)) %>% 
  filter(min > 365*0.7) %>% 
  ungroup()


stations_us <- stations_us %>% 
  semi_join(stations_stable, by = c('usaf'='stn', 'wban')) %>% 
  mutate(index_st = as.integer(rownames(.)))


stations_join <- stations_us %>% 
  select(usaf, wban, index_st)

data_weather <- data_weather %>% 
  inner_join(stations_join, by = c('stn' = 'usaf', 'wban')) %>% 
  select(-stn, -wban)


data_tidy <- data_weather %>% 
  mutate(precip = if_else(is.na(precip) & is_rain == 0, 0, precip),
         is_element = if_else(is_rain + is_hail + is_snow > 0, 1, 0)) %>% 
  select(index_st, date:snow, is_element)

data_gather <- data_tidy %>% 
  gather(metric, value, -c(index_st:date)) %>% 
  filter(!is.na(value))
















# Find the closest neighbors for each city among stations
nn <- nn2(data = stations_us %>% select(lat,lon),
          query = locations %>% select(lat,lon),
          k = 8)

##So, radius var in nn2 foesn't seem to filter out stations far away. PR ends up pulling Miami stations. The next sequence addresses it.
nn1_df <- as_data_frame(nn[[1]]) %>% 
  mutate(index_loc = as.integer(rownames(.))) %>% 
  gather(V, index_st, V1:V8)
  
nn2_df <- as_data_frame(nn[[2]]) %>% 
  mutate(index_loc = as.integer(rownames(.))) %>% 
  gather(V, dist, V1:V8)
  
nn_df <- nn1_df %>% 
  inner_join(nn2_df, by = c("index_loc", "V")) %>% 
  #and this is where I filter out remote locations
  filter(dist <= 2) %>% 
  mutate(rank = str_remove(V, "V") %>% as.integer()) %>% 
  select(-c(V,dist))



##rm(nn1_df, nn2_df, nn, data_weather, data_gather)


## Now to the big join.
## First, take locations.
## Then, find 5 closest stations for each
## Then, join the actual data from each station
## Lastly, filter to return the top reading


locations_tojoin <- locations %>% 
  select(index_loc) %>% 
  # join dates - daily grain from 2013 to 2017
  merge(tibble(date = seq.Date(date("2013-01-01"), date("2017-12-31"), by = "day")), all = TRUE) %>% 
  merge(tibble(metric = c("temp_mean", "temp_max", "temp_min", "precip", "snow", "is_element")), all = TRUE)

data_final <- NULL

for(i in 1:7) {
  nn_sub <- nn_df %>% 
    filter(rank == i)
  
  locations_missing <- locations_tojoin %>% 
    left_join(nn_sub, by = "index_loc")
    
  data_joined <- locations_missing %>% 
    left_join(data_gather, by = c("date", "index_st", "metric"))
  
  locations_tojoin <- data_joined %>% 
    filter(is.na(value)) %>% 
    select(-rank, -value, -index_st)
    
  data_final <- rbind(data_final, data_joined %>% filter(!is.na(value)))
}
  
  ##merge(tibble(metric = c("temp_mean", "temp_max", "temp_min", "precip", "snow", "is_element")), all = TRUE)


data_spread <- data_final %>% 
  select(-rank, -index_st) %>% 
  spread(metric, value)

data <- data_spread %>% 
  left_join(locations %>% select(geoid, index_loc, name, pop17, lat, lon), by = "index_loc") %>% 
  replace_na(list(snow = 0, precip = 0)) %>% 
  mutate(temp_max = if_else(is.na(temp_max), 2*temp_mean - temp_min, temp_max),
         temp_min = if_else(is.na(temp_max), 2*temp_mean - temp_max, temp_min))



## Checkpoint
save(data, locations, file = "data/2-tidy.RData")

