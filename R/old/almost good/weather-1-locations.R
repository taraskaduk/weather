library(tidyverse)
library(jsonlite)

setwd("weather")

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


save(loc_cities, loc_msa, loc_top1000, file = "data/1-locations.RData")

