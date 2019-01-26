library(tidyverse)

setwd("weather")


# 1. Import ---------------------------------------------------------------


## Stations

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


## Data

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


## Checkpoint
save(weather_data, stations_us, file = "data/1-import.RData")



# 1. Locations ------------------------------------------------------------

library(jsonlite)
# library(tidyverse)
# setwd("weather")

path_raw <- "data/0-raw/"
path_top1000 <- paste0(path_raw, "cities.json")
loc_top1000 <- fromJSON(path_top1000) %>% 
  rename(lon = longitude,
         lat = latitude,
         growth = growth_from_2000_to_2013) %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0))


path_cities <- paste0(path_raw, "uscitiesv1.4.csv")
loc_cities <- read_csv(path_cities) %>% 
  rename(lon = lng,
         city_nonascii = city,
         city = city_ascii,
         state = state_id) %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0))



path_msa <- paste0(path_raw,"2015_Gaz_cbsa_national.txt")
loc_msa <- read_delim(path_msa, 
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
  filter(type == "1") %>% 
  select(-type) %>% 
  mutate(name = str_remove(name, " Metro Area"),
         name = str_remove(name, " Micro Area"))


path_msa_pop <- paste0(path_raw,"PEP_2017_PEPANNRES.csv")
loc_msa_pop <- read_csv(path_msa_pop, 
                        col_types = cols(GEO.id2 = col_character()),
                        locale = locale(encoding = "LATIN1", 
                                        asciify = TRUE),
                        trim_ws = TRUE) %>% 
  select(geoid = GEO.id2,
         pop10 = rescen42010,
         pop17 = respop72017)

loc_msa <- left_join(loc_msa, loc_msa_pop, by = "geoid") %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0))



ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = loc_msa, aes(x=lon, y = lat, size = pop17), col = 'blue',alpha = 0.3)

ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = loc_msa, aes(x=lon.0, y = lat.0), col = 'blue',alpha = 0.3)

## Checkpoint
save(loc_cities, loc_msa, loc_top1000, file = "data/1-locations.RData")


# 2. Tidy -----------------------------------------------------------------

library(lubridate)
# library(tidyverse)
# setwd("weather")
# load("data/1-import.RData")

stations_join <- stations_us %>% 
  select(usaf, wban, lat, lon)

weather <- weather_data %>% 
  inner_join(stations_join, by = c('stn' = 'usaf', 'wban'))

w_mutated <- weather %>% 
  mutate(lat.0 = round(lat,0),
         lon.0 = round(lon,0),
         flag = "existing",
         day = if_else(day(date) == 29 & month(date) == 2, 28, as.numeric(day(date))),
         date = date(ISOdate(year(date),month(date),day))
  ) %>% 
  select(-c(lat, lon, stn, wban, day)) %>% 
  group_by(lat.0, lon.0, flag, date) %>% 
  summarise_all(funs(mean, max), na.rm = TRUE) %>% 
  select(lat.0:date,
         temp_mean = temp_mean_mean,
         temp_min = temp_min_mean,
         temp_max = temp_max_mean,
         precip = precip_max,
         snow = snow_max,
         pressure = pressure_mean,
         wind = wind_mean,
         gust = gust_mean,
         is_fog = is_fog_max,
         is_rain = is_rain_max,
         is_snow = is_snow_max,
         is_hail = is_hail_max,
         is_thunder = is_thunder_max,
         is_tornado = is_tornado_max) %>% 
  ungroup() %>% 
  purrr::map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', 'pressure', 'wind', 'gust'), ~ifelse(is.nan(.x) | is.infinite(.x), NA, .x)) %>% 
  bind_rows()


coords <- w_mutated %>% 
  select(lat.0, lon.0) %>% 
  distinct()

dummy <- tibble(date = seq.Date(date("2012-01-01"), date("2017-12-31"), by = "day")) %>% 
  filter(!(day(date)==29 & month(date)==2)) %>% 
  mutate(flag = "missing") %>% 
  merge(coords, all = TRUE)

data_missing <- dummy %>% 
  anti_join(w_mutated, by = c("lat.0","lon.0","date"))

data_all <- w_mutated %>% 
  union_all(data_missing) %>% 
  mutate(yday = if_else(leap_year(date) & yday(date) > 60, yday(date) - 1, yday(date)),
         year = year(date))

data_all %>% group_by(flag) %>% count()


## Checkpoint
save(data_all, file = "data/2-tidy.RData")


# 3. Predict --------------------------------------------------------------

library(maps)
library(ggthemes)
library(scales)
library(rsample)
library(caret)
library(recipes)
library(yardstick)
library(RANN)

# library(tidyverse)
# library(lubridate)
# setwd("weather")
# load("data/2-tidy.RData")


data_all <- data_all %>% 
  mutate(is_element = if_else(is_rain + is_snow + is_hail > 0, 1, 0))

data_select <- data_all %>% 
  select(lat.0, lon.0, yday, year, date, flag, temp_mean, temp_max, temp_min, precip, snow, is_element)


data_prep <- data_select %>% 
  mutate(flag = if_else((is.na(temp_max) | is.na(temp_min) | is.na(precip)) & !is.na(temp_mean), "partial", flag))


nn <- nn2(data = data_prep %>% filter(flag == "existing") %>% select(lat.0:year),
          query = data_prep %>% filter(flag == "partial") %>% select(lat.0:year),
          k = 5)
index_leave <- nn[[1]] %>% as.vector() %>% unique()
data_prep$flag_train_prep <- rownames(data_prep) %in% index_leave





f_knn <- function(df_train = data_prep %>% filter(flag_train_prep == TRUE), 
                  df_pred = data_prep, col){
  
  formula <- as.formula(substitute(col ~ lat.0 + lon.0 + yday + year, list(col = as.name(col))))
  
  knn_model <- knnreg(formula = formula, 
                      data = df_train,
                      k = 5)
  col <- quo_name(enquo(col))
  
  
  predict(knn_model, newdata = df_pred)
  
}


data_prepped <- data_prep %>%
  filter(flag == 'partial') %>% 
  mutate(temp_min =  f_knn(df_pred = ., col = "temp_min"),
         temp_max =  f_knn(df_pred = ., col = "temp_max"),
         precip =    f_knn(df_pred = ., col = "precip")) %>% 
  union_all(data_prep %>% filter(flag != "partial"))



# Final prediction

nn <- nn2(data = data_prepped %>% filter(flag != "missing") %>% select(lat.0:year),
          query = data_prepped %>% filter(flag == "missing") %>% select(lat.0:year),
          k = 5)

index_leave <- nn[[1]] %>% as.vector() %>% unique()

data_prepped$flag_train <- rownames(data_prepped) %in% index_leave



data_predicted <- data_prepped %>% 
  filter(flag == 'missing') %>% 
  mutate(temp_mean = f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "temp_mean"),
         temp_min =  f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "temp_min"),
         temp_max =  f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "temp_max"),
         precip =    f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "precip"),
         snow =      f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "snow"),
         is_element= f_knn(df_pred = ., df_train = data_prepped %>% filter(flag_train == TRUE), col = "is_element")) %>% 
  union_all(data_prep %>% filter(flag != "missing"))



## Checkpoint
save(data_predicted, file = "data/3-predict.RData")


# 4. Final ----------------------------------------------------------------

library(maps)
library(ggthemes)
library(scales)

# library(tidyverse)
# library(lubridate)
# setwd("weather")
# load("data/3-predict.RData")
# load("data/1-locations.RData")



## Notes
# https://www.coynecollege.edu/news-events/ideal-temperatures-heat-cool
# https://weather.com/news/news/how-hot-is-too-hot-survey
# https://en.wikipedia.org/wiki/Rain#Intensity


p_temp_max <- 85
p_temp_min <- 40 #lowest for only one extra layer of clothing
p_temp_mean_low <- 50
p_temp_mean_high <- 75
p_precip <- 0.1


w_pleasant <- data_predicted %>% 
  mutate(day = day(date),
         month = month(date)) %>% 
  mutate(pleasant = if_else(temp_min >= p_temp_min &
                              temp_max <= p_temp_max &
                              temp_mean >= p_temp_mean_low & temp_mean <= p_temp_mean_high & 
                              precip <= p_precip &
                              is_element < 0.5,
                            1,
                            0),
         hot = if_else(temp_max > p_temp_max |
                         temp_mean > p_temp_mean_high,
                       1,
                       0),
         cold = if_else(temp_min < p_temp_min |
                          temp_mean < p_temp_mean_low,
                        1,
                        0),
         elements = if_else(is_element >= .5 |
                              precip > 0.3 |
                              snow > 0, 
                            1, 0),
         distinct_class = case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1      ~ "hot",
                                    elements == 1 ~ "elements",
                                    cold == 1     ~ "cold",
                                    TRUE          ~ NA_character_))

msa_pleasant <- loc_msa %>% 
  left_join(w_pleasant, by = c("lat.0", "lon.0"))




loc_top1000 %>% 
  arrange(desc(as.integer(population))) %>% 
  head(25) %>% 
  left_join(w_pleasant, by = c("lat.0", "lon.0")) %>% 
  
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = distinct_class)) +
  geom_tile(col = "black") +
  facet_wrap(~city) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d7191c", 
                               cold = "#0571b0", 
                               elements = "#b3cde3"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()



# msa_pleasant %>% 
#   filter(name == "Jacksonville, FL") %>% 
# ggplot(aes(x=date, alpha = as.factor(pleasant))) +
#   geom_point(aes(y = temp_mean), col = "grey")+
#   geom_point(aes(y = temp_max), col = "red") +
#   geom_point(aes(y = temp_min), col = "blue") +
#   facet_wrap(~ year, scales = "free")


msa_pleasant %>% 
  filter(year == 2017 &
           name %in% c("Seattle-Tacoma-Bellevue, WA", 
                       "Jacksonville, FL",
                       "San Diego-Carlsbad, CA",
                       "New York-Newark-Jersey City, NY-NJ-PA",
                       "Los Angeles-Long Beach-Anaheim, CA",
                       "Chicago-Naperville-Elgin, IL-IN-WI",
                       "San Francisco-Oakland-Hayward, CA",
                       "Washington-Arlington-Alexandria, DC-VA-MD-WV",
                       "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD")) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = distinct_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d7191c", 
                               cold = "#0571b0", 
                               elements = "#b3cde3"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()

msa_summary <- msa_pleasant %>% 
  group_by(name, year) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  mutate(rank = row_number(desc(pleasant)))

msa_top25 <- msa_summary %>% 
  filter(rank <= 25) %>% 
  mutate(name2 = reorder(name, rank))

msa_pleasant %>% 
  inner_join(msa_top25, by = "name") %>% 
  filter(year == 2017) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = distinct_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name2) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d7191c", 
                               cold = "#0571b0", 
                               elements = "#b3cde3"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()






msa_pleasant %>% 
  filter(name == "Seattle-Tacoma-Bellevue, WA") %>% 
  ggplot(aes(x=day, y = year, fill = as.factor(elements)), col = grey) +
  geom_tile() +
  facet_wrap( ~ month) +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  scale_fill_fivethirtyeight()+
  coord_equal()

msa_pleasant %>% 
  filter(name == "Seattle-Tacoma-Bellevue, WA") %>% 
  ggplot(aes(x=day, y = year, fill = precip), col = grey) +
  geom_tile() +
  facet_wrap( ~ month) +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  coord_equal()

msa_pleasant %>% 
  filter(name == "Seattle-Tacoma-Bellevue, WA") %>% 
  group_by(year, month) %>% 
  summarize(precip = sum(precip)) %>% 
  ggplot(aes(x=month, y = precip, col = as.factor(year), group = year, alpha = year, size = year)) +
  geom_line() +
  theme_bw() + 
  theme(panel.grid.major = element_blank())



w_pleasant_summar <- w_pleasant %>% 
  group_by(lat.0, lon.0, year) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(lat.0, lon.0) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  mutate(pleasant_cat = case_when(pleasant >=300 ~ "Over 300",
                                  pleasant >=200 ~ "200 - 299",
                                  pleasant >=100 ~ "100 - 199",
                                  pleasant >=50 ~ "50 - 99",
                                  TRUE ~ "Less than 50"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 50", "50 - 99", "100 - 199", "200 - 299", "Over 300")))




ggplot() + 
  geom_point(data = w_pleasant_summar %>% filter(pleasant_cat == "Over 300"), aes(x=lon.0, y=lat.0), col = "red", size = 9, alpha = 0.4) +
  geom_point(data = w_pleasant_summar %>% filter(pleasant_cat == "200 - 299"), aes(x=lon.0, y=lat.0), col = "red", size = 8, alpha = 0.3) +
  geom_point(data = w_pleasant_summar, aes(x=lon.0, y=lat.0, col=pleasant_cat), size = 6) +
  #  scale_color_brewer(type = "seq", name = "Pleasant days") +
  scale_colour_manual(values = c("grey90", "#c6dbef", "#4292c6", "#2171b5", "#08306b"), name = "Pleasant days") +
  theme_fivethirtyeight() +
  theme(
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")

lat <- data_frame(lat.0 = seq(-90, 90, by = 1))
lon <- data_frame(lon.0 = seq(-180, 180, by = 1))
dots <- lat %>% 
  merge(lon, all = TRUE)

dots <- dots %>% 
  mutate(country = map.where('state', lon.0, lat.0),
         lakes = map.where('lakes', lon.0, lat.0)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)

ggplot() + 
  geom_point(data = dots, aes(x=lon.0, y = lat.0), col = "grey90", size = 6) +
  geom_point(data = w_pleasant_summar, aes(x=lon.0, y=lat.0, col=pleasant), size = 6, alpha = 0.4) +
  geom_point(data = w_pleasant_summar, aes(x=lon.0, y=lat.0, col=pleasant, alpha = pleasant), size = 6) + 
  
  scale_colour_gradient2(low = "grey85",
                         high = "darkblue",
                         
                         na.value = "grey85") +
  theme_fivethirtyeight() +
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")










w_msa <- loc_msa %>% 
  #filter(!(state %in% c("AK", "HI", "PR", "VI"))) %>% 
  # select(id, city = city_ascii, state = state_id, lat.0, lon.0, lat, lon) %>% 
  left_join(w_pleasant_summar, by = c("lat.0", "lon.0")) %>% 
  mutate(pleasant_cat = case_when(pleasant >=200 ~ "Over 200",
                                  pleasant >=150 ~ "150 - 199",
                                  pleasant >=100 ~ "100 - 149",
                                  TRUE ~ "Less than 100"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 100", "100 - 149", "150 - 199", "Over 200")))

top_msa <- w_msa %>% 
  group_by(lat.0, lon.0, pleasant) %>% 
  summarise(loc = paste(name, collapse = ", "))

top_cities$data


ggplot() + 
  geom_point(data = cities_w, aes(x=lon, y=lat, col=pleasant_cat), size = 0.1) +
  scale_colour_manual(values = c("grey90", "#c6dbef", "#4292c6", "#084594"), name = "Pleasant days") +
  theme_fivethirtyeight() +
  theme(
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")






# Visualize ---------------------------------------------------------------

# Generate a data frame with all dots -----------------------------------------------

lat <- data_frame(lat.0 = seq(-90, 90, by = 1))
lon <- data_frame(lon.0 = seq(-180, 180, by = 1))
dots <- lat %>% 
  merge(lon, all = TRUE)


## Only include dots that are within borders. Also, exclude lakes.
dots <- dots %>% 
  mutate(country = map.where('state', lon.0, lat.0),
         lakes = map.where('lakes', lon.0, lat.0)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)












weather_summary <- w_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  semi_join(dots, by = c("lat.0", "lon.0")) %>% 
  mutate(pleasant200 = if_else(pleasant >= 200, 1, 0),
         pleasant150 = if_else(pleasant >= 150, 1, 0),
         pleasant_cat = case_when(pleasant >=200 ~ "Over 200",
                                  pleasant >=150 ~ "150 - 199",
                                  pleasant >=100 ~ "100 - 149",
                                  TRUE ~ "Less than 100"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 100", "100 - 149", "150 - 199", "Over 200")))


ggplot() + 
  geom_point(data = dots, aes(x=lon.0, y = lat.0), col = "grey90", size = 6) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant), size = 6, alpha = 0.4) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant, alpha = pleasant), size = 6) + 
  
  scale_colour_gradient2(low = "grey90",
                         high = "darkblue",
                         
                         na.value = "grey95") +
  theme_fivethirtyeight() +
  theme(legend.position="none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")


ggplot() + 
  geom_point(data = dots, aes(x=lon.0, y = lat.0), col = "grey95", size = 6) +
  geom_point(data = weather_summary, aes(x=lon.0, y=lat.0, col=pleasant_cat), size = 6) +
  
  scale_colour_manual(values = c("grey90", "#c6dbef", "#4292c6", "#084594"), name = "Pleasant days") +
  
  theme_fivethirtyeight() +
  theme(
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")




w_daily_pleasant %>% 
  filter(lat.0 == 48 & lon.0 == -122) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

w_daily_pleasant %>% 
  filter(lat.0 == 48 & lon.0 == -122) %>% 
  ggplot(aes(x=yday, y = prcp, col = as.factor(pleasant))) + 
  geom_point()


w_daily_pleasant %>% 
  filter(lat.0 == 30 & lon.0 == -81) %>% 
  ggplot(aes(x=yday, y = temp_mean, col = as.factor(pleasant))) + 
  geom_point()

w_daily_pleasant %>% 
  filter(lat.0 == 30 & lon.0 == -81) %>% 
  ggplot(aes(x=yday, y = prcp, col = as.factor(pleasant))) + 
  geom_point()

w_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=pleasant)) + 
  geom_point(size = 3) + 
  scale_color_continuous(low='white', high='red') +
  theme_void()


w_daily_pleasant %>% 
  group_by(lon.0, lat.0) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  mutate(pleasant_group = as.factor(case_when(pleasant > 250 ~ 3,
                                              pleasant > 150 ~ 2,
                                              TRUE ~ 1))) %>% 
  ggplot(aes(x=lon.0, y=lat.0, col=pleasant_group)) + 
  geom_point(size = 4) + 
  theme_void() +
  coord_map(projection = "albers", lat_0=45, lon_0=-100)







