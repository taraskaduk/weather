library(tidyverse)

# Download ------------------------------------------------------------------

# Cities ------------------------------------------------------------------

url_cities <- 'https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv'



cities <- read_csv(url_cities)
write_csv(cities, "data/0-raw/cities.csv")
  
cities <- cities %>% 
  rename(lon = lng)

write_csv(cities, "data/cities.csv")
save(cities, file = "data/cities.RData")


# Cities2 -----------------------------------------------------------------

url_cities2 <- "http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2017_Gazetteer/2017_Gaz_ua_national.zip"
zip <- "cities.zip"

curl::curl_download(url_cities2, destfile = zip)
untar(zip)

untar(zip, exdir = "data/0-raw")


cities2 <- read_delim("2017_Gaz_ua_national.txt", 
                      "\t", escape_double = FALSE, 
                      col_types = cols(ALAND = col_skip(), 
                                       ALAND_SQMI = col_skip(), 
                                       AWATER = col_skip(), 
                                       AWATER_SQMI = col_skip()), 
                      trim_ws = TRUE) %>% 
  rename(name = NAME,
         geoid = GEOID,
         lat = INTPTLAT,
         lon = INTPTLONG,
         type = UATYPE) %>% 
  mutate(name = str_remove(name, " Urban Cluster"),
         name = str_remove(name, " Urbanized Area")) %>% 
  separate(name, c("city", "state"), ",")

save(cities2, file = "data/cities2.RData")

# Stations -----------------------------------------------------------------

url_stations <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt"
stations_import <- read_table(url_stations,
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

stations <- stations_import %>% 
  rename(station = `station name`,
         elev = `elev(m)`) %>% 
  filter(!is.na(usaf) & !(is.na(lat) | is.na(lon)))

write_csv(stations, "data/stations.csv")
save(stations, file = "data/stations.RData")

stations_us <- stations %>% 
  filter(lat <= 49 & lat >= 24 & lon <= -66 & lon >= -125)
save(stations_us, file = "data/stations_us.RData")

# ggplot(stations_us, aes(x=lon, y=lat)) + geom_point(size=0.2)

# Data --------------------------------------------------------------------

weather_data_raw <- NULL

for (year in 2012:2017) {
  
  file <- paste0('gsod_',year)
  ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
  destfile <- paste0(file,'.op.gz')
  
  curl::curl_download(ftp, destfile)
  untar(destfile, exdir = file)
  
  
  files_all <- list.files(path = file)
  files_stations <- paste0(stations_us$usaf, "-", stations_us$wban, "-", year, ".op.gz")
  files_keep <- subset(files_all, files_all %in% files_stations)

  
  data_raw <- map_df(paste0(file, "/",files_keep),
                     read_table,
                     col_types = cols(.default = "c",
                                      YEARMODA = col_date(format = "%Y%m%d")),
                     na = c("9999.9", '99.99', '999.9', '0.00I')
  )
  
  colnames(data_raw) <- tolower(colnames(data_raw))
  
  data_raw_renamed <- data_raw %>%
    rename(stn = `stn---`, date = yearmoda, temp_mean = temp, temp_min = min, temp_max = max)
  
  weather_data_raw <- rbind(weather_data_raw, data_raw_renamed)
  file.remove(destfile)
  unlink(file, recursive = TRUE)
  
}

weather_data <- weather_data_raw %>%
  # rename columns
  select(stn, wban, date, temp_mean, temp_min, temp_max, prcp, sndp) %>%
  # clean up asterisks and flags
  # (see data description for details)
  map_df(~ str_replace_all(.,'A|B|C|D|E|F|G|H|I|\\*', '')) %>%
  # convert some columns to numeric after cleaning up
  map_at(c('temp_mean', 'temp_min', 'temp_max', 'prcp', 'sndp'), as.numeric, na.rm = TRUE) %>%
  dplyr::bind_rows() %>% 
  # precipitation and snowfall NAs can be converted to 0 for this project
  # (see data description for details)
  # or maybe not...
  replace_na(replace = list(prcp = 0, sndp = 0))

save(weather_data, file = "data/data.RData")
