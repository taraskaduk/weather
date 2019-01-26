# # Way #1 ------------------------------------------------------------------
# 
# stations <- read_table("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt", skip = 20) %>% 
#   filter(!is.na(USAF))
# 
# AL <- read_csv("https://www1.ncdc.noaa.gov/pub/orders/CDO8069527522505.txt")
# AK <- read_csv("https://www1.ncdc.noaa.gov/pub/orders/CDO2826477522516.txt")
# AZ <- read_csv("https://www1.ncdc.noaa.gov/pub/orders/CDO8861097522518.txt")
# AR <- read_csv("https://www1.ncdc.noaa.gov/pub/orders/CDO6212467522520.txt")
# CA <- read_csv("https://www1.ncdc.noaa.gov/pub/orders/CDO6207247522539.txt")



# Way #2: archive ---------------------------------------------------------
library(tidyverse)
library(readr)
library(rvest)
library(tools)
library(stringr)
library(maps)

# Initial import. run and comment out - why tease the ftp with multiple requests?
# year <- 2017
# file <- paste0('gsod_',year)
# ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
# destfile <- paste0(file,'.tar')

# curl::curl_download(ftp, destfile)
# untar(destfile, exdir = file)

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

stations_loc <- stations %>% 
  select(usaf, state = st, country = ctry, lat, lon) %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% 
  distinct() %>% 
  group_by(usaf) %>% 
  summarise(state = max(state, na.rm = TRUE),
            country = max(country, na.rm = TRUE),
            lat = mean(lat),
            lon = mean(lon))

stations_loc %>% filter(country == 'US' & is.na(state)) 


zip <- read_csv('zip.csv', col_types = c('cdd'))



ggplot(stations_loc %>% filter(country == 'US' & !is.na(state)), aes(x=lon, y =lat)) + geom_point(size = 1)


data_us <- data_clean %>% 
  inner_join(stations_us, by = c('stn' = 'usaf'))

data_us %>% 
  filter(stn == '747820') %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(x = date)) +
    geom_point(aes(y = temp_mean), col = 'grey', alpha = 0.5) +
    geom_point(aes(y = temp_min), col = 'blue', alpha = 0.5) +
    geom_point(aes(y = temp_max), col = 'red', alpha = 0.5)

write_csv(data_us, 'data_us.csv')





