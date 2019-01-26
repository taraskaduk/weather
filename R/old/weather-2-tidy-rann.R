setwd("weather")

library(RANN)
library(caret)
library(lubridate)
library(tidyverse)

load("data/1-import.RData")

# 2.1 Tidy -----------------------------------------------------------------

stations_join <- stations_us %>% 
  select(usaf, wban, lat, lon)

data_coords <- data_weather %>% 
  inner_join(stations_join, by = c('stn' = 'usaf', 'wban'))

#This creates a list of stations with consistent observations over time
stations_stable <- data_coords %>% 
  group_by(stn, wban, lat, lon, year(date)) %>% 
  count() %>% 
  group_by(stn, wban, lat, lon) %>% 
  summarise(min = min(n)) %>% 
  filter(min > 365*0.8) %>% 
  ungroup()


nn <- nn2(data = stations_stable %>% select(lat,lon),
          query = locations %>% select(lat,lon),
          k = 5)


##So, radius var in nn2 foesn't seem to filter out stations far away. PR ends up pulling Miami stations. The next sequence addresses it.
nn1_df <- as_data_frame(nn[[1]]) %>% 
  mutate(i = rownames(.)) %>% 
  gather(V, index, V1:V5)
  
nn2_df <- as_data_frame(nn[[2]]) %>% 
  mutate(i = rownames(.)) %>% 
  gather(V, dist, V1:V5)
  
nn_df <- nn1_df %>% 
  inner_join(nn2_df, by = c("i", "V")) %>% 
  #and this is where I filter out remote locations
  mutate(index = if_else(dist <= 2, index, NA_integer_))

nn_df_spread <- nn_df %>% 
  select(-dist) %>% 
  spread(V, index)

index_leave <- nn_df$index %>% as.vector() %>%  unique()


## This one create a column rather than filters out. Uncomment - Imma filter out this time
#data_stable$nn <- rownames(data_stable) %in% index_leave

stations_nn <- stations_stable %>% 
  mutate(index = rownames(.)) %>% 
  .[index_leave, ]


data_nn <- data_coords %>% 
  inner_join(stations_nn %>% select(stn, wban, index), by = c("stn", "wban"))


loc_nn <- locations %>% 
  mutate(i = rownames(.)) %>% 
  inner_join(nn_df_spread, by = "i")



result_dummy <- loc_nn %>% 
  select(geoid, name, lat, lon, V1:V3) %>% 
  distinct() %>% 
  merge(tibble(date = seq.Date(date("2013-01-01"), date("2017-12-31"), by = "day")), all = TRUE)

result_gathered <- result_dummy %>% 
  mutate(year = year(date),
         yday = yday(date)) %>% 
  gather(key = "neighbor", value = "index", c(V1:V3)) %>% 
  mutate(index = as.character(index)) %>% 
  filter(!is.na(index))

data_tojoin <- data_nn %>% 
  mutate(date = as.Date(date)) %>% 
  select(index, date, temp_mean:snow, is_rain, is_snow)

result_joined <- result_gathered %>% 
  left_join(data_tojoin, by=c("index", "date"))



result_summed <- result_joined %>% 
  group_by(geoid, name, lat, lon, date, year, yday) %>% 
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
  mutate(is_na = if_else(rowSums(is.na(.)) > 0, 1, 0))






knn_train <- data_nn %>% 
  select(lat, lon, date:snow, is_rain, is_snow) %>% 
  mutate(year = year(date),
         yday = yday(date))



f_knn <- function(df_train = knn_train, 
                  df_pred, col){
  
  nn <- nn2(data = df_train %>% select(lat,lon,yday,year),
            query = df_pred %>% select(lat,lon,yday,year),
            k = 5)
  i_nn <- nn[[1]] %>% as.vector() %>% unique()
  
  df_train_nn <- df_train[i_nn, ]
  
  formula <- as.formula(substitute(col ~ lat + lon + yday + year, list(col = as.name(col))))
  knn_model <- knnreg(formula = formula, 
                      data = df_train_nn,
                      k = 5)
  col <- quo_name(enquo(col))
  
  predict(knn_model, newdata = df_pred)
  
}


## This is where it will probably choke...

result_predicted <- result_summed %>%
  filter(is_na == 1) %>% 
  ##I just can't work this into the function... I give up
  mutate(temp_mean = if_else(is.na(temp_mean), f_knn(df_pred = ., col = "temp_mean"), temp_mean),
         temp_max = if_else(is.na(temp_max), f_knn(df_pred = ., col = "temp_max"), temp_max),
         temp_min = if_else(is.na(temp_min), f_knn(df_pred = ., col = "temp_min"), temp_min),
         precip = if_else(is.na(precip), f_knn(df_pred = ., col = "precip"), precip),
         snow = if_else(is.na(snow), f_knn(df_pred = ., col = "snow"), snow),
         is_rain = if_else(is.na(is_rain), f_knn(df_pred = ., col = "is_rain"), is_rain),
         is_snow = if_else(is.na(is_snow), f_knn(df_pred = ., col = "is_snow"), is_snow)) %>% 
  union_all(result_summed %>% filter(is_na == 0)) %>% 
  select(-is_na)


data <- result_predicted

## Checkpoint
save(data, locations, file = "data/2-tidy.RData")

