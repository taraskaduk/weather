# Libraries ---------------------------------------------------------
library(tidyverse)
library(lubridate)
# library(readr)
# library(rvest)
# library(tools)
# library(stringr)
# library(maps)
# library(jsonlite)
# library(geosphere)



# Stations ----------------------------------------------------------------

# #Scrape all this!!!
# stations <- read_csv("stations_final.csv")
# stations %>% filter(city == 'Jacksonville' & state == 'Florida')
# 
# stations_usa <- stations %>% 
#   semi_join(cities, by = c('city', 'state'))
# 
# n_distinct(stations_usa$city)


# this file looks like a tab separated file, however readr doesn't take the \t. 
# Whitespace does the job, but oblviously breaks things on the city column. 
# I therefore skip everything after the altitude column, as that data is not important to me
# stations_import <- read_table2("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 
#                               col_names = c("station", "lat", "lon", "alt"), 
#                               col_types = cols(X5 = col_skip(), 
#                                                X6 = col_skip(), 
#                                                X7 = col_skip(), 
#                                                X8 = col_skip(), 
#                                                X9 = col_skip()
#                               ))
# 
# # Back up
# write_csv(stations_import, 'stations_import.csv')
# 
# ggplot() + 
#   geom_point(data = stations_import, aes(x = lon, y = lat), alpha = 0.1, size = 0.1, col = 'red') +
#   geom_point(data = cities, aes(x = longitude, y = latitude), alpha = 0.2, size = 0.1, col = 'blue') +
#   theme_light()
# 




stations <- read_table("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt", 
                       skip = 20,
                       na = c('999999', '99999', '-999.0', ''))

colnames(stations) <- tolower(colnames(stations))

# # Old
# stations_us <- stations %>% 
#   select(usaf, state = st, lat, lon) %>% 
#   filter(!is.na(lat) & !is.na(lon)) %>% 
#   mutate(country = map.where('world', lon, lat)) %>% #USA doesn't include AK, HI, PR
#   filter((str_detect(country, "USA") == TRUE | str_detect(country, "Puerto Rico") == TRUE) & lon < 0 ) %>% 
#   distinct() %>% 
#   group_by(usaf) %>% 
#   summarise(state = max(state, na.rm = TRUE),
#             lat = mean(lat),
#             lon = mean(lon))

stations_us <- stations %>% 
  select(usaf, country = ctry, state = st, lat, lon) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% 
  filter(country %in% c("US", "RQ")) %>%  #USA + territories
  distinct() %>% 
  group_by(usaf) %>% 
  summarise(state = max(state, na.rm = TRUE),
            lat = mean(lat),
            lon = mean(lon)) %>% 
  filter(!is.na(state) & lat > 10 & lon < 0)

ggplot(stations_us, aes(x=lon, y =lat)) + geom_point(size = 1)


# Data --------------------------------------------------------------------





# # Initial import. run and comment out - why tease the ftp with multiple requests?
# for (year in 2014:2017) {
#   
#   file <- paste0('gsod_',year)
#   
#   ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
#   destfile <- paste0(file,'.op.gz')
#   
#   curl::curl_download(ftp, destfile)
#   untar(destfile, exdir = file)
#   
#   data_all <- map_df(list.files(file, full.names = TRUE), 
#                      read_table, 
#                      col_types = cols(.default = "c",
#                                       YEARMODA = col_date(format = "%Y%m%d")),
#                      na = c("9999.9", '99999', '99.99', '999.9', '0.00I')
#                      )
#   
#   # write_csv(data_all, paste0(file, '.csv'))
#   # data_all <- read_csv("gsod_2017.csv", 
#   #                      col_types = cols(FRSHTT = col_character(), 
#   #                                       WBAN = col_character(),
#   #                                       YEARMODA = col_date(format = "%Y%m%d")),
#   #                      na = c("9999.9", '99999', '99.99', '999.9', '0.00I'))
#   
#   colnames(data_all) <- tolower(colnames(data_all))
#   
#   data_clean <- data_all %>% 
#     # rename columns
#     select(stn = `stn---`, wban, date = yearmoda, temp_mean = temp, temp_min = min, temp_max = max, prcp, sndp) %>% 
#     # clean up asterisks and flags
#     # (see data description for details)
#     map_df(~ str_replace_all(.,'A|B|C|D|E|F|G|H|I|\\*', '')) %>% 
#     # convert some columns to numeric after cleaning up
#     map_at(c('temp_mean', 'temp_min', 'temp_max', 'prcp', 'sndp'), as.numeric) %>% 
#     dplyr::bind_rows() %>% 
#     # precipitation and snowfall NAs can be converted to 0 for this project 
#     # (see data description for details)
#     replace_na(replace = list(prcp = 0, sndp = 0))
#   
#   
#   
#   
#   data_us <- data_clean %>% 
#     inner_join(stations_us, by = c('stn' = 'usaf'))
#   
#   data_us %>% 
#     select(lat, lon, stn) %>%
#     distinct() %>% 
#     ggplot(aes(x=lon, y = lat)) + geom_point(size = 0.25, alpha = 0.1)
#   
#   data_us %>% 
#     mutate(latr = round(lat*4,0)/4,
#            lonr = round(lon*4,0)/4) %>% 
#     select(latr, lonr, stn) %>% 
#     distinct() %>% 
#     ggplot(aes(x=lonr, y = latr)) + geom_point(size = 0.25, alpha = 0.8)
#   
#   
#   write_csv(data_us, paste0("data/data_us_", year, ".csv"))
#   
#   file.remove(destfile)
#   unlink(file, recursive = TRUE) 
# 
# }

# Subsequent import
# CSV ---------------------------------------------------------------------
data_us <- map_df(list.files("data", full.names = TRUE), 
                  read_csv,
                  col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                                   state = col_character(), stn = col_character(), 
                                   wban = col_character()))

weather_daily <- data_us %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0),
         year = year(date),
         month = month(date),
         day = day(date),
         yday = if_else(year %% 4 == 0 & yday(date) > 60,
                        yday(date) - 1,
                        yday(date))) %>% 
  select(-c(state, lat, lon, stn, wban, date)) %>% 
  group_by(lat.0, lon.0, month, day, yday) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
  filter(!(day == 29 & month == 2)) %>% 
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
  mutate(pleasant = if_else(temp_min >= 40 & #was 45
                              temp_max <= 80 &  #was 85
                              (temp_mean >= 50 | temp_mean <=75) & #was 55
                              sndp < 5 & # how to determine this?
                              prcp < 0.2,
                            1,
                            0))

weather_daily_pleasant %>% 
  filter(lat.0 == 48 & lon.0 == -122) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

weather_daily_pleasant %>% 
  filter(lat.0 == 30 & lon.0 == -81) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()


weather_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
ggplot(aes(x=lon.0, y=lat.0, col=pleasant)) + 
  geom_point(size = 3) + 
  scale_color_continuous(low='white', high='red') +
  theme_void()


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

# Cities ------------------------------------------------------------------

url_cities <- 'https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv'

cities <- read_csv(url_cities) %>% 
  rename(latitude = lat,
         longitude = lng)

write_csv(cities, "data/cities.csv")

ggplot(cities, aes(x = longitude, y = latitude)) + 
  geom_point(alpha = 0.2, size = 0.1) +
  theme_light()