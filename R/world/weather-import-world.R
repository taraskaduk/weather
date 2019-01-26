library(tidyverse)

# Cities ------------------------------------------------------------------

url_cities <- 'https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv'

cities <- read_csv(url_cities) %>% 
  rename(lon = lng)

write_csv(cities, "data/cities.csv")

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


# Data --------------------------------------------------------------------

weather_data <- NULL

for (year in 2014:2017) {

  file <- paste0('gsod_',year)

  ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
  destfile <- paste0(file,'.op.gz')

  curl::curl_download(ftp, destfile)
  untar(destfile, exdir = file)

  data_all <- map_df(list.files(file, full.names = TRUE),
                     read_table,
                     col_types = cols(.default = "c",
                                      YEARMODA = col_date(format = "%Y%m%d")),
                     na = c("9999.9", '99.99', '999.9', '0.00I')
                     )

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
  
  weather_data <- rbind(weather_data, data_clean)

  # data_us <- data_clean %>%
  #   inner_join(stations_us, by = c('stn' = 'usaf'))
  # 
  # data_us %>%
  #   select(lat, lon, stn) %>%
  #   distinct() %>%
  #   ggplot(aes(x=lon, y = lat)) + geom_point(size = 0.25, alpha = 0.1)
  # 
  # data_us %>%
  #   mutate(latr = round(lat*4,0)/4,
  #          lonr = round(lon*4,0)/4) %>%
  #   select(latr, lonr, stn) %>%
  #   distinct() %>%
  #   ggplot(aes(x=lonr, y = latr)) + geom_point(size = 0.25, alpha = 0.8)


  # write_csv(data_clean, paste0("data/data_clean_", year, ".csv"))

  file.remove(destfile)
  unlink(file, recursive = TRUE)

}


save(cities, stations, weather_data, file = "data/data-world.RData")
