library(tidyverse)

setwd("weather")

# Stations -----------------------------------------------------------------

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

stations <- stations_import %>% 
  rename(station = `station name`,
         elev = `elev(m)`) %>% 
  filter(!is.na(usaf) & !(is.na(lat) | is.na(lon)))

stations_us <- stations %>% 
  filter(lat <= 49 & lat >= 24 & lon <= -66 & lon >= -125)


# Data --------------------------------------------------------------------

weather_data_raw <- NULL

for (year in 2012) {
  
  destfile <- paste0('data/0-raw/gsod/gsod_',year,'.op.gz')
  path_untar <- 'data/0-raw/gsod/untar'
  untar(destfile, exdir = path_untar)
  
  
  files_all <- list.files(path = path_untar)
  files_stations <- paste0(stations_us$usaf, "-", stations_us$wban, "-", year, ".op.gz")
  files_keep <- subset(files_all, files_all %in% files_stations)

  
  data_raw <- map_df(paste0(path_untar, "/",files_keep),
                     read_table,
                     col_types = cols(.default = "c",
                                      YEARMODA = col_date(format = "%Y%m%d")),
                     na = c("9999.9", '99.99', '999.9')
  )
  
  colnames(data_raw) <- tolower(colnames(data_raw))
  
  data_raw_renamed <- data_raw %>%
    rename(stn = `stn---`, date = yearmoda, temp_mean = temp, temp_min = min, temp_max = max)
  
  weather_data_raw <- rbind(weather_data_raw, data_raw_renamed)
  #file.remove(destfile)
  #unlink(file, recursive = TRUE)
  
}

weather_data <- weather_data_raw %>%
  select(stn, wban, date, temp_mean, temp_min, temp_max, precip = prcp, snow = sndp, pressure = stp, wind = wdsp, gust, frshtt) %>%
  mutate(prcp_hours = case_when(str_detect(precip, "A") ~ 6,
                           str_detect(precip, "B") ~ 12,
                           str_detect(precip, "C") ~ 18,
                           str_detect(precip, "D") ~ 24,
                           str_detect(precip, "E") ~ 12,
                           str_detect(precip, "F") ~ 24,
                           str_detect(precip, "G") ~ 24,
                           str_detect(precip, "H") ~ 1,
                           str_detect(precip, "I") ~ 1,
                           TRUE ~ NA_real_)) %>% 
  map_df(~ str_replace_all(.,'A|B|C|D|E|F|G|H|I|\\*', '')) %>%
  map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', 'prcp_hours', 'gust', 'wind', 'pressure'), as.numeric, na.rm = TRUE) %>%
  dplyr::bind_rows() %>% 
  # precipitation and snowfall NAs can be converted to 0 for this project
  # (see data description for details)
  # except for the one notated with an I
  replace_na(replace = list(precip = 0, snow = 0)) %>% 
  mutate(precip = precip / prcp_hours) %>% 
  mutate(is_fog     = as.integer(substr(frshtt, 1,1)),
         is_rain    = as.integer(substr(frshtt, 2,2)),
         is_snow    = as.integer(substr(frshtt, 3,3)),
         is_hail    = as.integer(substr(frshtt, 4,4)),
         is_thunder = as.integer(substr(frshtt, 5,5)),
         is_tornado = as.integer(substr(frshtt, 6,6))) %>% 
  select(-frshtt, -prcp_hours)


#############

save(weather_data, stations_us, file = "data/1-import.RData")
