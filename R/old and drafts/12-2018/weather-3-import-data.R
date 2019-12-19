setwd("weather")

library(lubridate)
library(tidyverse)



# Load --------------------------------------------------------------------

locations_filtered <- readRDS("data/2-locations-filtered.RDS")
stations <- readRDS("data/2-stations-filtered.RDS")



# 1.2.1 Import weather data --------------------------------------------------

weather_data_raw <- NULL
p_yearend <- year(Sys.Date()) - 1

for (year in (p_yearend-4):p_yearend) {
  
  destfile <- paste0('data/0-raw/gsod/gsod_',year,'.op.gz')
  path_untar <- 'data/0-raw/gsod/untar'
  untar(destfile, exdir = path_untar)
  
  
  files_all <- list.files(path = path_untar)
  files_stations <- paste0(stations$usaf, "-", stations$wban, "-", year, ".op.gz")
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
  
}


# 1.2.2 Clean weather data ------------------------------------------------------

data <- weather_data_raw %>%
  select(usaf = stn, wban, date, temp_mean, temp_min, temp_max, precip = prcp, snow = sndp, pressure = stp, wind = wdsp, gust, frshtt) %>%
  ##read txt file for instructions. Different letters designate different periods during which the data was collected.
  mutate(prcp_hours = case_when(str_detect(precip, "A") ~ 6,
                                str_detect(precip, "B") ~ 12,
                                str_detect(precip, "C") ~ 18,
                                str_detect(precip, "D") ~ 24,
                                str_detect(precip, "E") ~ 12,
                                str_detect(precip, "F") ~ 24,
                                str_detect(precip, "G") ~ 24,
                                str_detect(precip, "H") ~ 1,
                                str_detect(precip, "I") ~ 1,
                                TRUE ~ 1)) %>% 
  map_df(~ str_replace_all(.,'A|B|C|D|E|F|G|H|I|\\*', '')) %>%
  map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', 'prcp_hours', 'gust', 'wind', 'pressure'), as.numeric, na.rm = TRUE) %>%
  dplyr::bind_rows() %>% 
  # precipitation and snowfall NAs can be converted to 0 for this project
  # (see data description for details)
  # except for the one notated with an I
  
  # 2018-08-15 Change my mind - There is something weird about snow and precip. I may need to treat NAs as NAs...
  # still can treat NA as 0 for snow though. Read the readme
  ##replace_na(replace = list(precip = 0, snow = 0)) %>% 
  mutate(is_fog     = as.integer(substr(frshtt, 1,1)),
         is_rain    = as.integer(substr(frshtt, 2,2)),
         is_snow    = as.integer(substr(frshtt, 3,3)),
         is_hail    = as.integer(substr(frshtt, 4,4)),
         is_thunder = as.integer(substr(frshtt, 5,5)),
         is_tornado = as.integer(substr(frshtt, 6,6))) %>% 
  mutate(precip= if_else(is.na(precip) & is_rain == 0, 0, precip),
         ## hourly precip. I keep going back and forth on this one.
         ## Does the legth of report matter? IDK
         ## Reverting back to daily for now... Meaning - uncomment the transformation to hourly
         
         #precip = precip / prcp_hours, 
         snow = if_else(is.na(snow) & is_snow == 0, 0, snow)) %>% 
  mutate(date = as.Date(date)) %>% 
  select(-frshtt, -prcp_hours)



# 1.4 Save necessary data -----------------------------------------------------
saveRDS(data, file = "data/3-data-import.RDS")
