# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(scales)
library(rsample)
library(caret)
library(recipes)
library(yardstick)
library(RANN)

setwd("weather")

# Import ------------------------------------------------------------------

load("data/2-tidy.RData")


data_all <- data_all %>% 
  mutate(is_element = if_else(is_rain + is_snow + is_hail > 0, 1, 0))

data_select <- data_all %>% 
  select(lat.0, lon.0, yday, year, date, flag, temp_mean, temp_max, temp_min, precip, snow, is_element)


data_prep <- data_select %>% 
  mutate(flag = if_else((is.na(temp_max) | is.na(temp_min) | is.na(precip)) & !is.na(temp_mean), "partial", flag))
         

nn <- nn2(data = data_prep %>% filter(flag == "existing") %>% select(lat.0:year),
          query = data_prep %>% filter(flag == "partial") %>% select(lat.0:year),
          k = 5)
index_leave <- nn[[1]] %>% as.vector() %>% unique()
data_prep$flag_train_prep <- rownames(data_prep) %in% index_leave



# 2018-07-28 --------------------------------------------------------------


f_knn <- function(df_train = data_prep %>% filter(flag_train_prep == TRUE), 
                  df_pred = data_prep, col){
  
  formula <- as.formula(substitute(col ~ lat.0 + lon.0 + yday + year, list(col = as.name(col))))
  
  knn_model <- knnreg(formula = formula, 
                          data = df_train,
                          k = 5)
  col <- quo_name(enquo(col))
  
  
  predict(knn_model, newdata = df_pred)
  
}


data_prepped <- data_prep %>%
  filter(flag == 'partial') %>% 
  mutate(temp_min =  f_knn(df_pred = ., col = "temp_min"),
         temp_max =  f_knn(df_pred = ., col = "temp_max"),
         precip =    f_knn(df_pred = ., col = "precip")) %>% 
  union_all(data_prep %>% filter(flag != "partial"))



# Final prediction --------------------------------------------------------

nn <- nn2(data = data_prepped %>% filter(flag != "missing") %>% select(lat.0:year),
          query = data_prepped %>% filter(flag == "missing") %>% select(lat.0:year),
          k = 5)

index_leave <- nn[[1]] %>% as.vector() %>% unique()

data_prepped$flag_train <- rownames(data_prepped) %in% index_leave



data_predicted <- data_prepped %>% 
  filter(flag == 'missing') %>% 
  mutate(temp_mean = f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "temp_mean"),
         temp_min =  f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "temp_min"),
         temp_max =  f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "temp_max"),
         precip =    f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "precip"),
         snow =      f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "snow"),
         is_element= f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "is_element")) %>% 
  union_all(data_prep %>% filter(flag != "missing"))













## -------------

save(data_predicted, file = "data/3-predict.RData")

