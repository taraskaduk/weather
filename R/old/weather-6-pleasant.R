library(lubridate)
library(tidyverse)


data <- readRDS("weather/rds/5-data-predicted.RDS")
locations <- readRDS("locations/output.RDS")

locations <- locations %>% 
  mutate(name_short = str_sub(name, start = 1, end = if_else(is.na(str_locate(name, "-")[,1]), str_locate(name, ",")[,1], str_locate(name, "-")[,1]) -1),
         city = str_sub(name, start = 1, end = str_locate(name, ",")[,1] -1),
         state = str_sub(name, start = str_locate(name, ",")[,1] + 2),
         name_edited = if_else(str_detect(city, "--") == TRUE, 
                               paste(str_replace_all(city, "--", " – "), state, sep = ", "),
                               paste(str_replace_all(city, "-", " – "), state, sep = ", ")),
         name_wrapped = str_replace_all(name_edited, " – ", "/\n"))

## Notes
# https://www.coynecollege.edu/news-events/ideal-temperatures-heat-cool
# https://weather.com/news/news/how-hot-is-too-hot-survey
# https://en.wikipedia.org/wiki/Rain#Intensity


# Parameters --------------------------------------------------------------

params <- list(
  temp_max = c(60, 90), #65 - "if it didn't get up to 65F in the warmest hour..."
  temp_min = c(40, 70), #lowest would be night + sunrise temp. Let's rule out near freezing temps.
                          #the upper limit is "when even the lowest night temp is too hot..."
  temp_mean = c(55, 75),
  precip = 0.1,
  snow = 0.5
)



# Data --------------------------------------------------------------------

data2 <- data %>% 
  filter(cbsafp == "10380" & year == 2016)

data_daily <- data %>% 
  left_join(locations %>% select(cbsafp, name, name_short, name_edited, name_wrapped, lsad, lat = intptlat, lon = intptlon, pop17), by = "cbsafp") %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2) %>% 
  mutate(pls_temp = case_when(is.na(temp_max) | is.na(temp_mean) | is.na(temp_min) ~ "unknown",
                              temp_min > params$temp_min[2] ~ 'hot',
                              temp_min <  params$temp_min[1] ~ 'cold',
                              temp_max >  params$temp_max[2] ~ 'hot',
                              temp_max <  params$temp_max[1] ~ 'cold',
                              temp_mean > params$temp_mean[2] ~ 'hot',
                              temp_mean < params$temp_mean[1] ~ 'cold',
                              TRUE ~ "pleasant"),
         ## keep tweaking this one... What's the "pleasant" amount of elements???
         pls_elements = case_when(is.na(is_rain) & is.na(is_snow) ~ "unknown",
                                  precip <= params$precip &
                                  snow <= params$snow &
                                  is_rain <= .5 &
                                  is_snow <= .5
                                                          ~ "pleasant",
                                  TRUE ~ "elements"),
         
         pleasant = case_when(pls_temp == 'unknown' & pls_elements == "unknown" ~ NA_real_,
                              pls_temp == 'pleasant' & pls_elements == "pleasant" ~1,
                              TRUE ~ 0),
         distinct_class = case_when(is.na(pleasant) ~ "unknown",
                                    pleasant == 1 ~ "pleasant",
                                    pls_temp == "hot" ~ "hot",
                                    pls_elements == "elements" ~ "elements",
                                    pls_temp == "cold" ~ "cold",
                                    TRUE          ~ NA_character_),
         double_class =   case_when(is.na(pleasant) ~ "unknown",
                                    pleasant == 1 ~ "pleasant",
                                    pls_temp == "hot" & pls_elements != "elements"  ~ "hot",
                                    pls_temp == "hot" & pls_elements == "elements"  ~ "hot & elements",
                                    pls_temp == "cold" & pls_elements != "elements"  ~ "cold",
                                    pls_temp == "cold" & pls_elements == "elements"  ~ "cold & elements",
                                    pls_elements == "elements"    ~ "elements",
                                    TRUE          ~ NA_character_),
         double_class = factor(double_class, levels = c("pleasant", "elements", "cold", "cold & elements", "hot", "hot & elements"))
         )



summary_coords <- data_daily %>% 
  group_by(cbsafp, lat05, lon05, year) %>% 
  summarise(pleasant = sum(pleasant)/365) %>% 
  ungroup() %>% 
  group_by(lat05, lon05) %>% 
  summarise(pleasant = mean(pleasant)*365) %>% 
  mutate(pleasant_cat = case_when(pleasant >=300 ~ "Over 300",
                                  pleasant >=200 ~ "200 - 299",
                                  pleasant >=100 ~ "100 - 199",
                                  pleasant >=50 ~ "50 - 99",
                                  TRUE ~ "Less than 50"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 50", "50 - 99", "100 - 199", "200 - 299", "Over 300")))

# 
# test <- pleasant_daily %>% 
#   group_by(cbsafp, name, lat, lon, lat05, lon05, year) %>% 
#   summarise(unknown = sum(is.na(pleasant)),
#             pleasant = sum(pleasant),
#             all = n())



summary_locations <- data_daily %>%
  group_by(cbsafp, name, name_short, name_edited, name_wrapped, lat, lon, lsad, year, pop17) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(cbsafp, name, name_short, name_edited, name_wrapped, lsad, pop17) %>% 
  summarise(pleasant_days = mean(pleasant)) %>% 
  ungroup() %>% 
  filter(!is.na(pleasant_days)) %>% 
  mutate(rank = row_number(desc(pleasant_days)),
         rank_rev = row_number(pleasant_days),
         points = pleasant_days / 365 * 100,
         name = reorder(name, rank),
         name_short = reorder(name_short, rank),
         name_wrapped = reorder(name_wrapped, rank),
         pop17 = as.integer(pop17))







summary_avg <- data_daily %>%
  group_by(cbsafp, year, double_class) %>% 
  count() %>%
  filter(!is.na(double_class)) %>% 
  
  group_by(cbsafp, year) %>% 
  mutate(sum_n = sum(n)) %>% 
  
  group_by(cbsafp, double_class) %>% 
  summarise(n = sum(n)) %>% 
  
  group_by(cbsafp) %>% 
  mutate(n = 365*n/sum(n)) %>% 
  ungroup() %>% 
  
  replace_na(list(n = 0))


data_daily <- data_daily %>% 
  select(-c(name, name_short, name_edited, name_wrapped, pop17, lat, lon, lat0:lon05)) %>% 
  mutate(month = factor(format(date, "%b"), levels = rev(month.abb)))

pleasant_daily <- data_daily %>% 
  select(cbsafp, date, year, month, day, yday, pleasant, distinct_class, double_class)


saveRDS(pleasant_daily, file = "weather/rds/6-pleasant-daily.RDS")
saveRDS(data_daily, file = "weather/rds/6-data-daily.RDS")
saveRDS(summary_avg, file = "weather/rds/6-summary-avg.RDS")
saveRDS(summary_coords, file = "weather/rds/6-summary-coords.RDS")
saveRDS(summary_locations, file = "weather/rds/6-summary-locations.RDS")

output <- summary_locations %>% 
  select(cbsafp, weather = pleasant_days)

saveRDS(output, file = "weather/output.RDS")
