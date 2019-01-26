# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(maps)
library(scales)
library(opencage)
library(ggmap)
library(ggthemes)


# Import ------------------------------------------------------------------

load("data/data-world.RData")


# Transform ---------------------------------------------------------------

stations_world <- stations %>% 
  select(usaf, wban, country = ctry, state = st, lat, lon) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% 
  distinct() %>% 
  group_by(usaf, wban) %>% 
  summarise(state = max(state, na.rm = TRUE),
            lat = mean(lat),
            lon = mean(lon))

ggplot(stations_world, aes(x=lon, y =lat)) + geom_point(size = 0.1, alpha = 0.1)


# Data --------------------------------------------------------------------

weather_data_coord <- weather_data %>% 
  inner_join(stations, by =c("stn"="usaf", "wban"))

weather_daily <- weather_data_coord %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0),
         year = year(date),
         month = month(date),
         day = day(date),
         yday = if_else(year %% 4 == 0 & yday(date) > 60,
                        yday(date) - 1,
                        yday(date)),
         day = if_else(day == 29 & month == 2, 28, as.numeric(day))) %>% 
  select(-c(lat, lon, stn, wban, date)) %>% 
  group_by(lat.0, lon.0, month, day, yday) %>% 
  summarise_if(is.numeric,mean, na.rm=TRUE) %>% 
  ungroup()

weather_daily %>%   
  group_by(lat.0, lon.0) %>% 
  summarise(n = n()) %>% 
  mutate(complete = if_else(n==365, 1, 0)) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=complete)) + geom_point()

weather_daily %>%   
  group_by(lat.0, lon.0) %>% 
  summarise(n = n()) %>% 
  mutate(complete = if_else(n==365, 1, 0)) %>% 
  ggplot(aes(x=n)) + geom_histogram()


weather_daily %>%   
  group_by(lat.0, lon.0) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(n) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


weather_daily %>%   
  ggplot(aes(x=prcp)) + geom_histogram()

weather_daily_pleasant <- weather_daily %>% 
  mutate(pleasant = if_else(temp_min >= 45 & #was 45
                              temp_max <= 85 &  #was 85
                              (temp_mean >= 55 | temp_mean <=75) & #was 55
                              sndp < 5 & # how to determine this?
                              prcp < 0.2,
                            1,
                            0))


# Visualize ---------------------------------------------------------------

weather_daily_pleasant %>% 
  filter(lat.0 == 48 & lon.0 == -122) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

weather_daily_pleasant %>% 
  filter(lat.0 == 30 & lon.0 == -81) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()



# Generate a data frame with all dots -----------------------------------------------

lat <- data_frame(lat.0 = seq(-90, 90, by = 1))
lon <- data_frame(lon.0 = seq(-180, 180, by = 1))
dots <- lat %>% 
  merge(lon, all = TRUE)

## Only include dots that are within borders. Also, exclude lakes.
dots <- dots %>% 
  mutate(country = map.where('world', lon.0, lat.0),
         lakes = map.where('lakes', lon.0, lat.0)) %>% 
  filter(!is.na(country) & is.na(lakes) & str_detect(country, "Antarctica") == FALSE) %>% 
  select(-lakes)



weather_summary <- weather_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  semi_join(dots, by = c("lat.0", "lon.0"))
  
  ggplot() + 
  geom_point(data = dots, aes(x=lon.0, y = lat.0), col = "grey90", size = 1) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant), size = 1, alpha = 0.4) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant, alpha = pleasant*2), size = 1) + 
  
  scale_colour_gradient2(low = "red", mid = "yellow",
                         high = "darkblue", midpoint = 182, space = "Lab",
                         na.value = "grey95") +
  theme_fivethirtyeight() +
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most and least pleasant days in a year",
       caption = "Blue - most pleasant days, yellow - midpoint at 182 pleasant days, red - least pleasant days, grey - no data available\n
A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2014 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")

  
  
  
  
  

# reverse geocode ---------------------------------------------------------


  cat("OPENCAGE_KEY=2c40d268e5b0427ba678df1b5900af0a\n",
      file=file.path(normalizePath("~/"), ".Renviron"),
      append=TRUE)
  Sys.getenv("OPENCAGE_KEY")
  
  # Now testing it:
  test <- opencage_reverse(latitude = 30.3016961, 
                           longitude = -81.6528217)
  test$results$components.city

weather_top100 <- weather_summary %>% 
  arrange(desc(pleasant)) %>% 
  head(100) %>% 
  mutate(city = purrr::map2(lat.0, lon.0, opencage_reverse) %>% .$results$components.city)

reverse <- purrr::map2(weather_top100$lat.0, 
                       weather_top100$lon.0, 
                       opencage_reverse, 
                       min_confidence = 1, 
                       no_annotation = TRUE) %>% 
  purrr::map_df("results") 

reverse_loc <- reverse %>% 
  mutate(city = case_when(!is.na(components.city) ~ components.city,
                          !is.na(components.town) ~ components.town,
                          !is.na(components.village) ~ components.village,
                          !is.na(components.suburb) ~ components.suburb,
                          !is.na(components.neighbourhood) ~ components.neighbourhood)) %>% 
  select(city, country = components.country)

weather_top100 <- weather_top100 %>% 
  bind_cols(reverse_loc)

weather_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  mutate(pleasant_group = as.factor(case_when(pleasant > 250 ~ 3,
                                              pleasant > 150 ~ 2,
                                              TRUE ~ 1))) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=pleasant_group)) + 
  geom_point(size = 4) + 
  theme_void() +
  coord_map(projection = "albers", lat_0=45, lon_0=-100)


transposed <- weather_daily_pleasant %>% 
  mutate(lat = if_else(lon.0 < -125 & lat.0 >48, lat.0 -50, lat.0),
         lon = if_else(lon.0 < -125 & lat.0 >48, lon.0 + 50, lon.0))

transposed %>% 
  group_by(lon, lat) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ggplot(aes(x=lon, y=lat, col=pleasant)) + 
  geom_point(size = 3) + 
  scale_color_continuous(low='white', high='darkblue') +
  theme_void() +
  coord_map(projection = "albers", lat_0=45, lon_0=-100)

