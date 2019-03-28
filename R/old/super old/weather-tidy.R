library(tidyverse)

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

for (year in 2012:2017) {
  
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
                     na = c("9999.9", '99.99', '999.9', '0.00I')
  )
  
  colnames(data_raw) <- tolower(colnames(data_raw))
  
  data_raw_renamed <- data_raw %>%
    rename(stn = `stn---`, date = yearmoda, temp_mean = temp, temp_min = min, temp_max = max)
  
  weather_data_raw <- rbind(weather_data_raw, data_raw_renamed)
  #file.remove(destfile)
  #unlink(file, recursive = TRUE)
  
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

save(weather_data, file = "data/1-tidy/data.RData")
