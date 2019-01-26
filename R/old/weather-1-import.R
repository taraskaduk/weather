library(maps)
library(lubridate)
library(tidyverse)

setwd("weather")


# 1.1 Import stations ---------------------------------------------------------------

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
  #USA doesn't include AK, HI, PR. We start at the world level
  mutate(country = map.where('world', lon, lat)) %>% 
  filter(
    # by filtering out everything not USA, we're losing some precision for towns on the border,
    # but the US data alone is rich enough.
    (str_detect(country, "USA") == TRUE |  str_detect(country, "Puerto Rico") == TRUE) & lon < 0 &
      # remove old stations
      year(end) >= year(today()) & year(begin) <= 2013
  )


ggplot(stations_us, aes(x=lon, y = lat)) + 
  geom_point(size = 0.5, alpha = 0.4) +
  theme_bw()


# 1.2.1 Import weather data --------------------------------------------------

weather_data_raw <- NULL
p_yearend <- year(Sys.Date()) - 1

for (year in 2013:p_yearend) {
  
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
  
}


# 1.2.2 Clean weather data ------------------------------------------------------

data_weather <- weather_data_raw %>%
  select(stn, wban, date, temp_mean, temp_min, temp_max, precip = prcp, snow = sndp, pressure = stp, wind = wdsp, gust, frshtt) %>%
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
                                TRUE ~ NA_real_)) %>% 
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
  mutate(precip = if_else(is.na(precip) & is_rain == 0, 0, precip),
         snow = if_else(is.na(snow) & is_snow == 0, 0, snow),
         precip = precip / prcp_hours * 24) %>% 
  mutate(date = as.Date(date)) %>% 
  select(-frshtt, -prcp_hours)





# 1.3 Import locations ------------------------------------------------------------

path_raw <- "data/0-raw/"
path_msa <- paste0(path_raw,"2015_Gaz_cbsa_national.txt")

locations <- read_delim(path_msa, 
                        "\t", escape_double = FALSE, 
                        locale = locale(encoding = "LATIN1", 
                                        asciify = TRUE),
                        col_types = cols(ALAND = col_skip(), 
                                         ALAND_SQMI = col_skip(), 
                                         AWATER = col_skip(), 
                                         AWATER_SQMI = col_skip(),
                                         GEOID = col_character()), 
                        trim_ws = TRUE) %>% 
  rename(csafp = CSAFP,
         geoid = GEOID,
         name = NAME,
         lat = INTPTLAT,
         lon = INTPTLONG,
         type = CBSA_TYPE) %>% 
  select(-type) %>% 
  separate(name, c("name", "type"), sep = -10) %>% 
  mutate(name = trimws(name)) 


path_msa_pop <- paste0(path_raw,"PEP_2017_PEPANNRES_with_ann.csv")
locations_pop <- read_csv(path_msa_pop, 
                          col_types = cols(GEO.id2 = col_character()),
                          locale = locale(encoding = "LATIN1", 
                                          asciify = TRUE),
                          trim_ws = TRUE) %>% 
  filter(GEO.id != "Id") %>% 
  select(geoid = GEO.id2,
         pop10 = rescen42010,
         pop17 = respop72017)

locations <- left_join(locations, locations_pop, by = "geoid") %>% 
  mutate(lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2)



ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = locations, aes(x=lon, y = lat, col = type),alpha = 0.3, size = 1)


ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = locations, aes(x=lon0, y = lat0), col = 'blue',alpha = 0.3)

ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = locations, aes(x=lon05, y = lat05), col = 'blue',alpha = 0.3)




# 1.4 Save necessary data -----------------------------------------------------
save(data_weather, stations_us, locations, file = "data/1-import.RData")

