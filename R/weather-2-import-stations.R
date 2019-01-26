setwd("weather")

library(lubridate)
library(tidyverse)
library(sf)
library(maps)

# Load locations of interest ----------------------------------------------
locations_import <- readRDS("data/1-locations.RDS")

locs_exact <- locations_import %>%  select(csafp, cbsafp)
# test <- locs_join %>% filter(cbsafp == 42660)
# test_scale <-  test$geometry * 1.5
locs_buffer <- st_buffer(locs_exact, dist = 0.4, nQuadSegs = 1)
# ggplot() +  geom_sf(data = locs_buffer) + geom_sf(data = test)







# Import stations ---------------------------------------------------------------

# path_stations <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt"
path_stations <- "data/0-raw/isd-history.txt"

stations_import <- read_table(path_stations,
                              col_types = cols(BEGIN = col_date(format = "%Y%m%d"), 
                                               CALL = col_character(), 
                                               CTRY = col_character(), 
                                               `ELEV(M)` = col_double(), 
                                               END = col_date(format = "%Y%m%d"), 
                                               LAT = col_double(), 
                                               LON = col_double(), 
                                               ST = col_character(), 
                                               `STATION NAME` = col_character(), 
                                               USAF = col_character(), 
                                               WBAN = col_character()),
                              skip = 20,
                              na = c('-999.0', ''))

colnames(stations_import) <- tolower(colnames(stations_import))


stations_all <- stations_import %>% 
  rename(station = `station name`,
         elev = `elev(m)`) %>% 
  filter(!is.na(usaf) & !is.na(wban) & !(is.na(lat) | is.na(lon)))

stations_land <- stations_all %>% 
  mutate(country = map.where('world', lon, lat),
         lakes = map.where('lakes', lon, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)

stations_sf <- stations_land %>% 
  select(usaf, wban, lat, lon) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4269)



## Find what stations match to what locations: exactly and also with a wider net.


stations_to_locs_exact <- st_join(locs_exact, stations_sf) %>% 
  mutate(match = "exact") %>% 
  filter(!is.na(usaf))
st_geometry(stations_to_locs_exact) <- NULL


## Wider net
stations_to_locs_buffer <- st_join(locs_buffer, stations_sf) %>% 
  mutate(match = "buffer") %>% 
  filter(!is.na(usaf))
st_geometry(stations_to_locs_buffer) <- NULL



## Create a df of all loc to st matches, and have a flag which is exact and which is from buffer
stations_to_locs <- union_all(
                                stations_to_locs_exact, 
                                anti_join(stations_to_locs_buffer, 
                                          stations_to_locs_exact, 
                                          by = c("csafp",  "cbsafp", "usaf", "wban")
                                          )
                              )
  

##Filter missing values
stations_filtered <- stations_land %>% 
  semi_join(stations_to_locs, by = c("usaf", "wban"))

locations_filtered <- locations_import %>% semi_join(stations_to_locs, by = "cbsafp")
st_geometry(locations_filtered) <- NULL

ggplot() + 
  geom_point(data = stations_filtered, aes(x=lon, y=lat), size = 0.1, alpha = 0.1) 
  #geom_point(data = locations_filtered, aes(x=intptlon, y=intptlat), size = 0.1, alpha = 0.1, col = 'red') 

# 1.4 Save necessary data -----------------------------------------------------

saveRDS(stations_to_locs, file = "data/2-stations-to-locs.RDS")
saveRDS(stations_filtered, file = "data/2-stations-filtered.RDS")
saveRDS(locations_filtered, file = "data/2-locations-filtered.RDS")

