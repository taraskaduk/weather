# Libraries ---------------------------------------------------------
library(tidyverse)
library(readr)
library(rvest)
library(tools)
library(stringr)
library(maps)
library(jsonlite)
library(geosphere)
library(rnoaa)


options("noaakey" = Sys.getenv("noaakey"))

# Cities ------------------------------------------------------------------

url_cities <- 'https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv'

cities <- read_csv(url_cities) %>% 
  rename(latitude = lat,
         longitude = lng)

ggplot(cities, aes(x = longitude, y = latitude)) + 
  geom_point(alpha = 0.2, size = 0.1) +
  theme_light()

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
stations_import <- read_table2("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 
                              col_names = c("station", "lat", "lon", "alt"), 
                              col_types = cols(X5 = col_skip(), 
                                               X6 = col_skip(), 
                                               X7 = col_skip(), 
                                               X8 = col_skip(), 
                                               X9 = col_skip()
                              ))

# Back up
write_csv(stations_import, 'stations_import.csv')

ggplot() + 
  geom_point(data = stations_import, aes(x = lon, y = lat), alpha = 0.1, size = 0.1, col = 'red') +
  geom_point(data = cities, aes(x = longitude, y = latitude), alpha = 0.2, size = 0.1, col = 'blue') +
  theme_light()


# Get stations data -------------------------------------------------------


## Initial download - uncomment
 url_gsoy <- "https://www.ncei.noaa.gov/data/gsoy/archive/gsoy-latest.tar.gz"
 destfile <- "gsoy.tar.gz"
 curl::curl_download(url_gsoy, destfile)
 untar(destfile, exdir = "gsoy")

files_all <- list.files(path = "gsoy")

# Filter stations by coordinates ------------------------------------------

stations_usa <- stations_import %>% 
  mutate(country = map.where('world', lon, lat)) %>% #USA doesn't include AK, HI, PR
  filter((str_detect(country, "USA") == TRUE | str_detect(country, "Puerto Rico") == TRUE) & lon < 0 )

ggplot(stations_usa, aes(x=lon, y= lat)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  coord_fixed()

# stations_usa <- stations_import %>% 
#   filter(lat <= max(cities$latitude) &
#            lat >= min(cities$latitude) &
#            lon <= max(cities$longitude) &
#            lon >= min(cities$longitude)
#   )



# Subset leaving only stations for which we have the data
stations_usa_avail <- stations_usa %>% 
  mutate(file = paste0(station,".csv")) %>% 
  filter(file %in% files_all)

files_keep <- stations_usa_avail$file
files_remove <- subset(files_all, !(files_all %in% files_keep))

file.remove(paste0("gsoy/",files_remove))


# Subset leaving only stations with fresh data ----------------------------

stations_year <- map_df(paste0("gsoy/",files_keep), read_csv, col_types = cols_only(STATION = col_character(),
                                                                                   DATE = col_integer()
                                                                                   ))
stations_year_sum <- stations_year %>% 
  rename(station = STATION,
         date = DATE) %>% 
  filter(date >= 2008) %>% 
  group_by(station) %>% 
  summarise(date_max = max(date),
            n = n()) %>% 
  filter(n > 4) %>% 
  mutate(file = paste0(station, ".csv"))

files_all2 <- list.files(path = "gsoy")
files_keep2 <- stations_year_sum$file
files_remove2 <- subset(files_all2, !(files_all2 %in% files_keep2))
file.remove(paste0("gsoy/",files_remove2))

stations_avail <- stations_usa_avail %>% 
  filter(file %in% files_keep2)



# Round up ----------------------------------------------------------------

stations <- stations_avail %>% 
  mutate(lat_round = round(lat*4,0)/4,
         lon_round = round(lon*4,0)/4)

ggplot(stations, aes(x=lon_round, y= lat_round)) + 
  geom_point(size = 0.1, alpha = 0.2) +
  coord_fixed()





# Import files ------------------------------------------------------------

weather_data_raw <- map_df(paste0("gsoy/",files_keep2), 
                           read_csv,
                           col_types = cols_only(STATION = col_character(),
                                                 NAME = col_character(), 
                                                 DATE = col_character(), 
                                                 LATITUDE = col_double(), 
                                                 LONGITUDE = col_double(), 
                                                 ELEVATION = col_double(),
                                                 HTDD = col_double(),
                                                 CLDD = col_double(),
                                                 DP10 = col_integer(),
                                                 DSND = col_integer(),
                                                 DT00 = col_integer(),
                                                 DT32 = col_integer(),
                                                 DX70 = col_integer(),
                                                 DX90 = col_integer(),
                                                 EMNT = col_double(),
                                                 EMXT = col_double(),
                                                 EMXP = col_double(),
                                                 PRCP = col_double(),
                                                 PSUN = col_double(), #%
                                                 SNOW = col_double(),
                                                 TAVG = col_double(),
                                                 TMAX = col_double(),
                                                 TMIN = col_double(),
                                                 TSUN = col_double()))





write_csv(weather_data_raw[1:200000,], "weather_data_raw1.csv")
write_csv(weather_data_raw[200001:nrow(weather_data_raw),], "weather_data_raw2.csv")


## Guide: https://www.ncei.noaa.gov/data/gsoy/doc/GSOYReadme.txt

weather_data <- weather_data_raw %>% 
  select(station = STATION,
         date = DATE,
         lat = LATITUDE,
         lon = LONGITUDE,
         alt = ELEVATION,
         name = NAME,
         heating_days = HTDD,
         cooling_days = CLDD,
         precip_days = DP10,
         snow_days = DSND,
         days_xcold = DT00,
         days_cold = DT32,
         days_hot = DX70,
         days_xhot = DX90,
         temp_extreme_min = EMNT,
         temp_extreme_max = EMXT,
         precip_extreme = EMXP,
         precip_total = PRCP,
         sunshine_total_chance = PSUN, #%
         snow_total = SNOW,
         temp_avg = TAVG,
         temp_max_avg = TMAX,
         temp_min_avg = TMIN,
         sunshine_total = TSUN #in minutes
  )

write_csv(weather_data, "weather_data.csv")
# weather_data <- read_csv("weather_data.csv")




# Group by coord ----------------------------------------------------------

weather_by_coord <- weather_data %>% 
  mutate(lat_round = round(lat*4,0)/4,
         lon_round = round(lon*4,0)/4) %>% 
  select(-c(lat, lon, alt)) %>% 
  group_by(lat_round, lon_round) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)


ggplot(weather_by_coord, aes(x=lon_round, y= lat_round, col = precip_total)) + 
  geom_point(size = 1) +
  coord_fixed() +
  theme_void() +
  scale_color_continuous(high = "red", low = "blue", na.value = "grey90")




# Test before creating a map of purrr -------------------------------------

radius <- 0.3

cities_test <- cities %>% 
  filter(id == "1840022312")

cities_merge_test <- cities_test %>% 
  select(id, latitude, longitude)

stations_needed_test <- cities_merge_test %>% 
  merge(stations %>% select(-alt) %>% 
          filter(lat <= cities_test$latitude + radius &
                   lat >= cities_test$latitude - radius &
                   lon <= cities_test$longitude + radius &
                   lon >= cities_test$longitude - radius), all = TRUE) %>% 
  mutate(distance = distHaversine(cbind(longitude, latitude), cbind(lon, lat))) %>% 
  arrange(distance) %>% 
  head(1)



stations_needed <- stations_needed_test[1,]

cities_merge <- cities %>% 
  select(id, latitude, longitude)

# for (i in 1:nrow(cities_merge)) {
#   stations_needed[i, ] <- cities_merge[i, ] %>% 
#     select(id, latitude, longitude) %>% 
#     merge(stations %>% select(-alt) %>% 
#             filter(lat <= cities_merge[i,]$latitude + radius &
#                      lat >= cities_merge[i,]$latitude - radius &
#                      lon <= cities_merge[i,]$longitude + radius &
#                      lon >= cities_merge[i, ]$longitude - radius), all = TRUE) %>% 
#     mutate(distance = distHaversine(cbind(longitude, latitude), cbind(lon, lat))) %>% 
#     arrange(distance) %>% 
#     head(1)
# }


ggplot() + 
  geom_point(data = stations, aes(x = lon, y = lat), alpha = 0.1, size = 0.4, col = 'red') +
  geom_point(data = cities, aes(x = longitude, y = latitude), alpha = 0.2, size = 0.1, col = 'blue') +
  theme_light()



## Far far away
for (i in 1:nrow(cities_merge)) {
  for (radius in seq(0.1, 10, by = 1)) {
    stations_merge <- stations %>% select(-alt) %>% 
      filter(lat <= cities_merge[i,]$latitude + radius &
               lat >= cities_merge[i,]$latitude - radius &
               lon <= cities_merge[i,]$longitude + radius &
               lon >= cities_merge[i, ]$longitude - radius)
    if(nrow(stations_merge) != 0) break
  }

  
  stations_needed[i, ] <- cities_merge[i, ] %>% 
    merge(stations_merge, all = TRUE) %>% 
    mutate(distance = distHaversine(cbind(longitude, latitude), cbind(lon, lat))) %>% 
    arrange(distance) %>% 
    head(1)
}






ggplot(stations_needed) + 
  geom_point(aes(x = lon, y = lat), alpha = 0.1, col = 'red', size = 3) +
  geom_point(aes(x = longitude, y = latitude), alpha = 0.2, col = 'blue') +
  theme_light()


stations_needed_dist <- stations_needed %>% 
  select(station) %>% 
  mutate(station = paste0(station,".csv")) %>% 
  distinct() %>% .[ , "station"]


files_remove <- subset(files_all, !(files_all %in% stations_needed_dist))
files_missing <- subset(stations_needed_dist, !(stations_needed_dist %in% files_all))

setwd("gsoy")
file.remove(files_remove)
setwd("..")

file_left <-  list.files(path = "gsoy")
file.remove(destfile)
rm(list= ls()[!(ls() %in% c('cities','stations_needed', 'stations_needed_dist'))])


##Attention!!! May blow uP!
options(warn=-1)
weather_data_raw <- map_df(paste0("gsoy/",stations_needed_dist), read_csv, col_types = cols(.default = "c"))
options(warn=0)


write_csv(weather_data_raw[1:200000,], "weather_data_raw1.csv")
write_csv(weather_data_raw[200001:400000,], "weather_data_raw2.csv")
write_csv(weather_data_raw[400001:nrow(weather_data),], "weather_data_raw3.csv")

file.remove("gsoy")


## Guide: https://www.ncei.noaa.gov/data/gsoy/doc/GSOYReadme.txt

weather_data <- weather_data_raw %>% 
  select(station = STATION,
         date = DATE,
         lat = LATITUDE,
         lon = LONGITUDE,
         alt = ELEVATION,
         name = NAME,
         heating_days = HTDD,
         cooling_days = CLDD,
         precip_days = DP10,
         snow_days = DSND,
         days_xcold = DT00,
         days_cold = DT32,
         days_hot = DX70,
         days_xhot = DX90,
         temp_extreme_min = EMNT,
         temp_extreme_max = EMXT,
         precip_extreme = EMXP,
         precip_total = PRCP,
         sunshine_total_chance = PSUN, #%
         snow_total = SNOW,
         temp_avg = TAVG,
         temp_max_avg = TMAX,
         temp_min_avg = TMIN,
         sunshine_total = TSUN #in minutes
  )

write_csv(weather_data, "weather_data.csv")


weather_data <- read_csv("weather_data.csv")











# Get the weather data ----------------------------------------------------
## SO, this isn't working... Bummer

year <- "2016"

stations_neeed <- cities_stations %>% 
  select(usaf, wban, station) %>% 
  distinct() %>% 
  mutate(ftp = paste0("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/", year, "/", usaf, "-", wban, "-", year, ".op.gz")) %>% 
  head(10) %>% 
  mutate(data = purrr::map(ftp, safely(read_table)))

map(stations_neeed$ftp, read_table)


year <- 2017
file <- paste0(station,'-',year)
station <- '722066-03853'
# ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
ftp_st <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file2,'.op.gz')
destfile <- paste0(file2,'.op.gz')

table <- read_table(ftp_st)




# New attempt -------------------------------------------------------------





# https://simplemaps.com/data/us-cities
# https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv



ncdc(datasetid = "GSOY",
     locationid = "FIPS:02", 
     startdate = "2012-01-01", 
     enddate = "2013-01-01")
























# Initial import. run and comment out - why tease the ftp with multiple requests?
year <- 1987
file <- paste0('gsod_',year)
file2 <- paste0(station,'-',year)
station <- '722066-03853'
# ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
ftp_st <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file2,'.op.gz')
destfile <- paste0(file2,'.op.gz')

table <- read_table(ftp_st)

curl::curl_download(ftp_st, destfile)
unzip(destfile, exdir = file2)

# data_all <- map_df(list.files(file, full.names = TRUE), read_table, col_types = c('cccccccccccccccccccccc'))
# write_csv(data_all, paste0(file, '.csv'))

data_all <- read_csv("gsod_2017.csv", 
                     col_types = cols(FRSHTT = col_character(), 
                                      WBAN = col_character(),
                                      YEARMODA = col_date(format = "%Y%m%d")),
                     na = c("9999.9", '99999', '99.99', '999.9', '0.00I'))

colnames(data_all) <- tolower(colnames(data_all))

data_clean <- data_all %>% 
  # rename columns
  select(stn = `stn---`, wban, date = yearmoda, temp_mean = temp, temp_min = min, temp_max = max, prcp, sndp) %>% 
  # clean up asterisks and flags
  # (see data description for details)
  map_df(~ str_replace_all(.,'A|B|C|D|E|F|G|H|I|\\*', '')) %>% 
  # convert some columns to numeric after cleaning up
  map_at(c('temp_mean', 'temp_min', 'temp_max', 'prcp', 'sndp'), as.numeric) %>% 
  dplyr::bind_rows() %>% 
  # precipitation and snowfall NAs can be converted to 0 for this project 
  # (see data description for details)
  replace_na(replace = list(prcp = 0, sndp = 0))

stations <- read_table("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt", 
                       skip = 20,
                       na = c('999999', '99999', '-999.0', ''))

colnames(stations) <- tolower(colnames(stations))

stations_us <- stations %>% 
  select(usaf, state = st, lat, lon) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% 
  mutate(usa = map.where('usa', lon, lat),
         lakes = map.where('lakes', lon, lat)) %>% 
  filter(!is.na(usa) & is.na(lakes)) %>% 
  select(-c(usa,lakes)) %>% 
  distinct() %>% 
  group_by(usaf) %>% 
  summarise(state = max(state, na.rm = TRUE),
            lat = mean(lat),
            lon = mean(lon))

zip <- read_csv('zip.csv', col_types = c('cdd'))



ggplot(stations_us, aes(x=lon, y =lat)) + geom_point(size = 1)


data_us <- data_clean %>% 
  inner_join(stations_us, by = c('stn' = 'usaf'))

data_us %>% 
  filter(stn == '747820') %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(x = date)) +
  geom_point(aes(y = temp_mean), col = 'grey', alpha = 0.5) +
  geom_point(aes(y = temp_min), col = 'blue', alpha = 0.5) +
  geom_point(aes(y = temp_max), col = 'red', alpha = 0.5)
