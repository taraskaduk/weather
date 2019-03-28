library(maps)
library(jsonlite)
library(RANN)
library(ggthemes)
library(scales)
library(caret)
library(lubridate)
library(tidyverse)


setwd("weather")


# 1. Import stations ---------------------------------------------------------------

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
    year(end) >= year(today()) & year(begin) <= 2012
    )
  

ggplot(stations_us, aes(x=lon, y = lat)) + 
  geom_point(size = 0.5, alpha = 0.4) +
  theme_bw()


# 1. Import weather data --------------------------------------------------

weather_data_raw <- NULL
p_yearend <- year(Sys.Date()) - 1

for (year in 2012:p_yearend) {
  
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


# Clean weather data ------------------------------------------------------

data_weather <- weather_data_raw %>%
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
save(data_weather, stations_us, file = "data/1-import.RData")



# 1. Import locations ------------------------------------------------------------

path_raw <- "data/0-raw/"
path_top1000 <- paste0(path_raw, "cities.json")
loc_top1000 <- fromJSON(path_top1000) %>% 
  rename(lon = longitude,
         lat = latitude,
         growth = growth_from_2000_to_2013) %>% 
  mutate(lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2)


path_cities <- paste0(path_raw, "uscitiesv1.4.csv")
loc_cities <- read_csv(path_cities) %>% 
  rename(lon = lng,
         city_nonascii = city,
         city = city_ascii,
         state = state_id) %>% 
  mutate(lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2)



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
  select(-type) %>% 
  separate(name, c("name", "type"), sep = -10) %>% 
  mutate(name = trimws(name)) 


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
  mutate(lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2)



ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = loc_msa, aes(x=lon, y = lat, col = type),alpha = 0.3, size = 1)

ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = loc_msa, aes(x=lon0, y = lat0), col = 'blue',alpha = 0.3)

ggplot() + 
  theme_void() +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  coord_equal()+
  geom_point(data = loc_msa, aes(x=lon05, y = lat05), col = 'blue',alpha = 0.3)

## Checkpoint
save(loc_cities, loc_msa, loc_top1000, file = "data/1-locations.RData")


# 2. Tidy -----------------------------------------------------------------

load("data/1-locations.RData")
load("data/1-import.RData")

stations_join <- stations_us %>% 
  select(usaf, wban, lat, lon)

data_coords <- data_weather %>% 
  inner_join(stations_join, by = c('stn' = 'usaf', 'wban'))

#This creates a list of stations with consistent observations over time
stations_stable <- data_coords %>% 
  group_by(stn, wban, lat, lon, year(date)) %>% 
  count() %>% 
  group_by(stn, wban, lat, lon) %>% 
  summarise(min = min(n)) %>% 
  filter(min > 365*0.9) %>% 
  ungroup()






nn <- nn2(data = stations_stable %>% select(lat,lon),
          query = loc_msa %>% select(lat,lon),
          k = 5)
index_leave <- nn[[1]] %>% as.vector() %>% unique()

## This one create a column rather than filters out. Uncomment - Imma filter out this time
#data_stable$nn <- rownames(data_stable) %in% index_leave

stations_nn <- stations_stable %>% 
  mutate(index = rownames(.)) %>% 
  .[index_leave, ]


data_nn <- data_coords %>% 
  inner_join(stations_nn %>% select(stn, wban, index), by = c("stn", "wban"))


loc_nn <- loc_msa %>% 
  cbind(as_data_frame(nn[[1]]))



result_dummy <- loc_nn %>% 
  select(geoid, name, lat, lon, V1:V5) %>% 
  distinct() %>% 
  merge(tibble(date = seq.Date(date("2012-01-01"), date("2017-12-31"), by = "day")), all = TRUE)

result_gathered <- result_dummy %>% 
  mutate(year = year(date),
         yday = yday(date)) %>% 
  gather(key = "neighbor", value = "index", c(V1:V5)) %>% 
  mutate(index = as.character(index))

data_tojoin <- data_nn %>% 
  mutate(date = as.Date(date)) %>% 
  select(index, date, temp_mean:snow, is_rain, is_snow)

result_joined <- result_gathered %>% 
  left_join(data_tojoin, by=c("index", "date"))



result_summed <- result_joined %>% 
  group_by(geoid, name, lat, lon, date, year, yday) %>% 
  summarise(temp_mean = mean(temp_mean, na.rm = TRUE),
            temp_min = mean(temp_min, na.rm = TRUE),
            temp_max = mean(temp_max, na.rm = TRUE),
            precip = max(precip, na.rm = TRUE),
            snow = max(snow, na.rm = TRUE),
            is_rain = max(is_rain, na.rm = TRUE),
            is_snow = max(is_snow, na.rm = TRUE)) %>% 
  ungroup() %>% 
  purrr::map_at(c('temp_mean', 'temp_min', 'temp_max', 'precip', 'snow', "is_rain", "is_snow"), ~ifelse(is.nan(.x) | is.infinite(.x), NA, .x)) %>% 
  bind_rows() %>% 
  mutate(is_na = if_else(rowSums(is.na(.)) > 0, 1, 0))






knn_train <- data_nn %>% 
  select(lat, lon, date:snow, is_rain, is_snow) %>% 
  mutate(year = year(date),
         yday = yday(date))



f_knn <- function(df_train = knn_train, 
                  df_pred, col){
  
  nn <- nn2(data = df_train %>% select(lat,lon,yday,year),
            query = df_pred %>% select(lat,lon,yday,year),
            k = 5)
  i_nn <- nn[[1]] %>% as.vector() %>% unique()
  
  df_train_nn <- df_train[i_nn, ]
  
  formula <- as.formula(substitute(col ~ lat + lon + yday + year, list(col = as.name(col))))
  knn_model <- knnreg(formula = formula, 
                      data = df_train_nn,
                      k = 5)
  col <- quo_name(enquo(col))

  predict(knn_model, newdata = df_pred)

  }





#save(knn_dummy, knn_train, f_knn, loc_msa, file = "data/zz-test.RData")

## This is where it will probably choke...

result_predicted <- result_summed %>%
  filter(is_na == 1) %>% 
  ##I just can't work this into the function... I give up
  mutate(temp_mean = if_else(is.na(temp_mean), f_knn(df_pred = ., col = "temp_mean"), temp_mean),
         temp_max = if_else(is.na(temp_max), f_knn(df_pred = ., col = "temp_max"), temp_max),
         temp_min = if_else(is.na(temp_min), f_knn(df_pred = ., col = "temp_min"), temp_min),
         precip = if_else(is.na(precip), f_knn(df_pred = ., col = "precip"), precip),
         snow = if_else(is.na(snow), f_knn(df_pred = ., col = "snow"), snow),
         is_rain = if_else(is.na(is_rain), f_knn(df_pred = ., col = "is_rain"), is_rain),
         is_snow = if_else(is.na(is_snow), f_knn(df_pred = ., col = "is_snow"), is_snow)) %>% 
  union_all(result_summed %>% filter(is_na == 0))


## Checkpoint
save(result_predicted, file = "data/3-predict.RData")


# 4. Final ----------------------------------------------------------------


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


w_pleasant <- result_predicted %>% 
  left_join(loc_msa %>% select(geoid, type), by = "geoid") %>% 
  mutate(day = day(date),
         month = month(date)) %>% 
  mutate(pleasant = if_else(temp_min >= p_temp_min &
                              temp_max <= p_temp_max &
                              temp_mean >= p_temp_mean_low & temp_mean <= p_temp_mean_high & 
                              precip <= p_precip &
                              is_rain < 0.5 &
                              is_snow < 0.5,
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
         elements = if_else(is_rain >= .5 |
                              is_snow >= 0.5 |
                              precip > 0.3,
                            1, 0),
         distinct_class = case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1      ~ "hot",
                                    elements == 1 ~ "elements",
                                    cold == 1     ~ "cold",
                                    TRUE          ~ NA_character_),
         double_class =   case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1 & elements == 0  ~ "hot",
                                    hot == 1 & elements == 1  ~ "hot & elements",
                                    cold == 1 & elements == 0    ~ "cold",
                                    cold == 1 & elements == 1    ~ "cold & elements",
                                    elements == 1    ~ "elements",
                                    TRUE          ~ NA_character_))



w_pleasant %>% 
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

w_summary_msa <- w_pleasant %>% 
  filter(type == "Metro Area") %>% 
  group_by(geoid, name, year) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(geoid, name) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  ungroup() %>% 
  mutate(rank = row_number(desc(pleasant)),
         name2 = reorder(name, rank))

w_top25_msa <- w_summary_msa %>% 
  filter(rank <= 25) %>% 
  mutate(name2 = reorder(name, rank))

w_pleasant %>% 
  inner_join(w_summary_msa %>% filter(rank <= 25), by = "geoid") %>% 
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


w_pleasant %>% 
  inner_join(w_summary_msa %>% filter(rank <= 25), by = "geoid") %>% 
  filter(year == 2017) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = double_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name2) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d7191c", 
                               cold = "#0571b0", 
                               elements = "#756bb1",
                               `hot & elements` = "#e6550d",
                               `cold & elements` = "#9ecae1"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  coord_equal()


w_pleasant %>% 
  inner_join(w_summary_msa %>% filter(rank >= nrow(w_summary_msa) - 25), by = "geoid") %>% 
  filter(year == 2017) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
  ggplot(aes(x=day, y = month, fill = double_class)) +
  geom_tile(col = "black") +
  facet_wrap(~name2) +
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d7191c", 
                               cold = "#0571b0", 
                               elements = "#756bb1",
                               `hot & elements` = "#e6550d",
                               `cold & elements` = "#9ecae1"), 
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







