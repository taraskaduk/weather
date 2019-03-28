# Pseudo-code -------------------------------------------------------------

# 1. Get the list of the cities
# 2. Get coordinates
# 3. Get closest stations
#   3.1. Stations withing 25 miles
#   3.2. Then look with 50 miles radius
# 4. Get weather data for said stations
# 5. Group by and average reading per city
# 6. Ta-da!


# Setup -------------------------------------------------------------------

library(tidyverse)
library(maps)
library(GSODR)
library(weathermetrics)

library(ggthemes)
library(scales)
library(maps)
library(lubridate)


years <- seq(year(today()) - 5, year(today()))


cities_import <- read_csv("data/worldcities.csv") %>%
  rename(lon = lng)

cities <- cities_import %>%
  filter(population >= 1000000)


# Stattions ----------------------------------------------------

cities_stations <- cities %>% 
  mutate(stnid = purrr::map2(lat, lon, nearest_stations, distance = 25)) %>% 
  unnest()

# Get unique stations
stations <- cities_stations %>% 
  select(stnid) %>% 
  distinct()

stations_v <- as_vector(stations)


to_untar <- list.files("data/gsod", full.names = TRUE)
purrr::map(to_untar, untar, exdir = tempdir())


files_all <- list.files(path = tempdir())
files_stations <- NULL
for (year in years) {
  files_stations <-
    c(files_stations, paste0(stations_v, "-", year, ".op.gz"))
}

files_keep <- subset(files_all, files_all %in% files_stations)
files_remove <- subset(files_all, !(files_all %in% files_stations))

file.remove(paste(tempdir(),files_remove, sep = "/"))

weather_import <- purrr::map_dfr(tempdir(), reformat_GSOD)
unlink(tempdir())
colnames(weather_import) <- tolower(colnames(weather_import))
saveRDS(weather_import, file = "data/weather_import.rds")


weather <- weather_import %>% 
  # Get rid of stations on water: oceans and lakes
  mutate(country = map.where('world', lon, lat),
         lakes = map.where('lakes', lon, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-c(lakes,  country)) %>% 
  # Remove counts and flags - won't be used in this analysis, 
  # although could have been useful for precip and snow at the very least.
  select(-c(usaf, wban, dewp_cnt, slp_cnt, stp_cnt, visib_cnt, wdsp_cnt, max_flag, prcp_flag))


feels_like <- function(temp, rh, wind) {
  hi <-  if_else(is.na(rh), temp, heat.index(t = temp, rh = rh, temperature.metric = "celsius", output.metric = "celsius", round = 2))
  temp_f <- celsius.to.fahrenheit(temp)
  wind_mph <- wind * 1.1507794
  wc_f = (35.74 + 0.6215*temp_f) - 35.75*(wind_mph^0.16) + 0.4275 * temp_f * (wind_mph^0.16)
  wc <- if_else(is.na(wind), temp, fahrenheit.to.celsius(wc_f))
  new_temp <- case_when(temp < 10 & wind_mph > 3 ~ wc,
                        temp > 27 ~ hi,
                        TRUE ~ temp)
  return(new_temp)
}


# Data summarized ---------------------------------------------------------

data <- cities_stations %>% 
  inner_join(weather, by = "stnid") %>% 
  select(city, country, lat = lat.x, lon = lon.y, capital, date = yearmoda, year, month, day,
         temp_max = max, temp_min = min, temp_mean = temp, dewp, slp, stp, visib, wdsp, mxspd, gust, prcp, sndp, i_fog:rh) %>% 
  group_by(city, country, lat, lon, capital, date, year, month, day) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  # Clean up NaN
  mutate_all(~if_else(is.nan(.x) | is.infinite(.x), NA_character_, as.character(.x))) %>% 
  mutate_at(vars(lat, lon, capital, temp_max:rh), as.numeric) %>% 
  mutate(date = as.Date(date),
         #Override or create new? Override for now
         temp_mean = feels_like(temp_mean, rh, wdsp),
         temp_min = feels_like(temp_min, rh, wdsp),
         temp_max = feels_like(temp_max, rh, wdsp)) %>%
  
  # A few possible substitutions
  replace_na(list(sndp = 0, prcp = 0)) %>% 
  mutate(yday = yday(date),
         temp_max = if_else(is.na(temp_max) & is.na(temp_min), temp_mean,
                            if_else(is.na(temp_max), 2*temp_mean - temp_min, temp_max)),
         temp_min = if_else(is.na(temp_max) & is.na(temp_min), temp_mean, 
                            if_else(is.na(temp_min), 2*temp_mean - temp_max, temp_min)))


# Pleasant ----------------------------------------------------------------

# Parameters
params <- list(
  temp_max = c(16, 32), #65 - "if it didn't get up to 65F in the warmest hour..."
  temp_min = c(4, 21), #lowest would be night + sunrise temp. Let's rule out near freezing temps.
  #the upper limit is "when even the lowest night temp is too hot..."
  temp_mean = c(13, 24),
  prcp = 2.5,
  sndp = 1.7
)



data_daily <- data %>% 
  mutate(pls_temp = case_when(is.na(temp_max) | is.na(temp_mean) | is.na(temp_min) ~ "unknown",
                              temp_min > params$temp_min[2] ~ 'hot',
                              temp_min <  params$temp_min[1] ~ 'cold',
                              temp_max >  params$temp_max[2] ~ 'hot',
                              temp_max <  params$temp_max[1] ~ 'cold',
                              temp_mean > params$temp_mean[2] ~ 'hot',
                              temp_mean < params$temp_mean[1] ~ 'cold',
                              TRUE ~ "pleasant"),
         ## keep tweaking this one... What's the "pleasant" amount of elements???
         pls_elements = case_when(is.na(i_rain_drizzle) & is.na(i_snow_ice) ~ "unknown",
                                  prcp <= params$prcp &
                                    sndp <= params$sndp &
                                    i_rain_drizzle <= 0.5 &
                                    i_snow_ice <= 0.5
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

saveRDS(data_daily, file = "data/data_daily.rds")
data_daily %>% 
  filter(year >= 2018) %>% 
  write_csv("data/data_daily_sample.csv")


summary_locations <- data_daily %>%
  group_by(city, country, lat, lon, capital, year) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(city, country, lat, lon, capital) %>% 
  summarise(pleasant_days = mean(pleasant)) %>% 
  ungroup() %>% 
  filter(!is.na(pleasant_days)) %>% 
  mutate(rank = row_number(desc(pleasant_days)),
         rank_rev = row_number(pleasant_days),
         points = pleasant_days / 365 * 100,
         name = reorder(city, rank))










# Plots -------------------------------------------------------------------


theme_set(theme_fivethirtyeight()+
            theme(rect = element_blank(),
                  panel.border = element_blank(),
                  strip.background = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title=element_blank()))


colors <- c(pleasant = "#1a9641", 
            hot = "#d6604d", 
            cold = "#4393c3", 
            elements = "#bebada",
            `hot & elements` = "#ca0020",
            `cold & elements` = "#0571b0")

caption <- ("Sources: NOAA Global Summary of the Day, U.S. Census\n taraskaduk.com | @taraskaduk")



ggplot(data_daily) +
  geom_tile(aes(x=yday, y=year, col = double_class, fill = double_class)) +
  facet_wrap(~name, ncol = 5) +
  scale_x_continuous(
    breaks = c(1, 182),
    label = c("January", "July")
  ) +
  theme() +
  scale_fill_manual(values = colors,
                    name = "Your distinct classification",
                    aesthetics = c("colour", "fill")) +
  labs(title = "Your title",
       subtitle = "Your subtitle",
       caption = "Your caption")

