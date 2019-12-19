# Setup -------------------------------------------------------------------

library(tidyverse)
library(maps)
library(GSODR)
library(weathermetrics)

library(ggthemes)
library(scales)
library(lubridate)

years <- seq(year(today()) - 5, year(today()))

# Cities -----------------------------------

cities_import <- read_csv("data/worldcities.csv") %>%
  rename(lon = lng)

cities <- cities_import %>%
  filter(population >= 250000)


# Stations  ----------------------------------------------------

cities_stations <- cities %>% 
  mutate(stnid = purrr::map2(lat, lon, nearest_stations, distance = 30)) %>% 
  unnest()

# Get unique stations
stations <- cities_stations %>% 
  select(stnid) %>% 
  distinct()

stations_v <- as_vector(stations)


# Unpack weather data --------------------------------------

## Unpack downloaded yearly archives 
to_untar <- list.files("data/gsod", full.names = TRUE)
purrr::map(to_untar, untar, exdir = tempdir())

## Go through all unpacked files, decide what to remove and what to keep
## based on the stations of interest
files_all <- list.files(path = tempdir())
files_stations <- purrr::cross(list(x1 = paste0(stations_v, "-"), x2 = paste0(years, ".op.gz"))) %>%
  purrr::map(lift(paste0)) %>% 
  as_vector()

files_keep <- subset(files_all, files_all %in% files_stations)
files_remove <- subset(files_all, !(files_all %in% files_stations))
file.remove(paste(tempdir(),files_remove, sep = "/"))


# Transform weather data ----------------------------------------------------

weather_import <- purrr::map_dfr(tempdir(), reformat_GSOD)
unlink(tempdir())
colnames(weather_import) <- tolower(colnames(weather_import))
saveRDS(weather_import, file = "data/weather_import.rds")
weather_import <- readRDS("data/weather_import.rds")


weather <- weather_import %>% 
  ## Get rid of stations on water: oceans and lakes
  mutate(country = map.where('world', lon, lat),
         lakes = map.where('lakes', lon, lat),
         yday = yday(yearmoda)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-c(lakes,  country)) %>% 
  ## Remove counts and flags - won't be used in this analysis, 
  ## although could have been useful for precip and snow at the very least.
  select(-c(usaf, wban, dewp_cnt, slp_cnt, stp_cnt, visib_cnt, wdsp_cnt, max_flag, prcp_flag))

## "Feels like" function, accounting for heat index and wind chills
feels_like <- function(temp, rh, wind) {
  hi <-  if_else(is.na(rh), temp, heat.index(t = temp, rh = rh, temperature.metric = "celsius", output.metric = "celsius", round = 2))
  temp_f <- celsius.to.fahrenheit(temp)
  wc_f = (35.74 + 0.6215*temp_f) - 35.75*(wind^0.16) + 0.4275 * temp_f * (wind^0.16)
  wc <- if_else(is.na(wind), temp, fahrenheit.to.celsius(wc_f))
  new_temp <- case_when(temp < 10 & wind > 3 ~ wc,
                        temp > 27 ~ hi,
                        TRUE ~ temp)
  return(new_temp)
}


# Join city and weather data ---------

data <- cities_stations %>% 
  inner_join(weather, by = "stnid") %>% 
  select(city, country, lat = lat.x, lon = lon.x, capital, population, date = yearmoda, year, month, day, yday,
         temp_max = max, temp_min = min, temp_mean = temp, dewp, slp, stp, visib, wdsp, mxspd, gust, prcp, sndp, i_fog:rh) %>% 
  group_by(city, country, lat, lon, capital, population, date, year, month, day, yday) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  # Clean up NaN
  mutate_at(vars(temp_max:rh), ~if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)) %>% 
  mutate_at(vars(year:day), as.numeric) %>% 
  #Override or create new? Override for now
  mutate(temp_mean = feels_like(temp_mean, rh, wdsp),
         temp_min = feels_like(temp_min, rh, wdsp),
         temp_max = feels_like(temp_max, rh, wdsp)) %>%
  # A few possible substitutions
  replace_na(list(sndp = 0, prcp = 0)) %>% 
  mutate(temp_max = if_else(is.na(temp_max) & is.na(temp_min), temp_mean,
                            if_else(is.na(temp_max), 2*temp_mean - temp_min, temp_max)),
         temp_min = if_else(is.na(temp_max) & is.na(temp_min), temp_mean, 
                            if_else(is.na(temp_min), 2*temp_mean - temp_max, temp_min)))

data %>% as.data.frame() %>% save(file = "data/data.RData")
load("data/data.RData")


# Pleasant days --------------------------------


# AUC of cosine function --------------------------------------------------

temp_auc <- function(temp_min, temp_max, temp_perfect = 18) {
  a <- (temp_max - temp_min) / 2 #amplitude
  period <- 24
  b <- 2 * pi / period
  d <- temp_min + a
  temperature <- function(x) {
    -a * cos(b * x) + d
  }
  if (temp_min >= temp_perfect) {
    # integral <- -a*sin(24*b) + 24*d - 24*temp_perfect
    integral <- integrate(temperature, 0, 24)$value - temp_perfect * 24
    area <- round(integral,2)
  } else if (temp_max <= temp_perfect) {
    integral <- temp_perfect * 24 - integrate(temperature, 0, 24)$value
    area <- round(integral,2)
  } else {
    intercept1 <- acos((d - temp_perfect) / a) / b
    intercept2 <- (12 - intercept1) * 2 + intercept1
    
    integral1 <-
      temp_perfect * intercept1 - integrate(temperature, 0, intercept1)$value
    integral2 <-
      integrate(temperature, intercept1, intercept2)$value - temp_perfect * (intercept2 -
                                                                               intercept1)
    integral3 <-
      temp_perfect * (24 - intercept2) - integrate(temperature, intercept2, 24)$value
    
    area <- round(integral1 + integral2 + integral3,2)
  }
  return(area)
}



## effects of the auc approach

df <- tibble(min=seq(0L,10L, by = 1), max=seq(20L, 25L, by = 0.5))
df <- df %>% mutate(auc = map2(min, max, temp_auc),
                    auc = round(as.numeric(auc)))
ggplot(df, aes(x=min, y=max, size = auc)) + geom_point()



ggplot(data.frame( x = c(0,24)), aes( x = x)) + 
  stat_function(fun = function(x) cos(x), 
                geom = "line", n=5000, col='blue')



# https://www.reddit.com/r/askscience/comments/ulxdg/what_is_the_ideal_temperature_of_surroundings_for/
# http://www.healthyheating.com/solutions.htm#.XMnNYJO6Mne
# https://www.scientificamerican.com/article/why-people-feel-hot/
# https://www.outsideonline.com/1784591/whats-best-temperature-productivity
# http://www.city-data.com/forum/general-u-s/54730-what-your-ideal-outdoor-temperature-4.html
# https://www.huffingtonpost.com.au/2017/11/27/this-is-the-perfect-temperature-for-being-happy-and-social-study-finds_a_23288718/
# https://www.coynecollege.edu/news/ideal-temperatures-heat-cool/
# https://www.sleep.org/articles/temperature-for-sleep/
# https://health.clevelandclinic.org/what-is-the-ideal-sleeping-temperature-for-my-bedroom/






################## NEW STUFF ####################

# params = list(
#   temp_max = c(20, 35),#65 - "if it didn't get up to 65F in the warmest hour..."
#   temp_min = c(5, 15),
#   #lowest would be night + sunrise temp. Let's rule out near freezing temps.
#   #the upper limit is "when even the lowest night temp is too hot..."
#   #temp_mean = c(13, 24),
#   prcp = 5,
#   sndp = 5),
# 
# 
# data_pleasant = data_collapsed %>% 
#   mutate(hot = if_else(temp_min > params$temp_min[2] |
#                          temp_max >  params$temp_max[2], 1, 0),
#          cold = if_else(temp_min <  params$temp_min[1] |
#                           temp_max <  params$temp_max[1], 1, 0),
#          elements = if_else(prcp > params$prcp |
#                               sndp > params$sndp |
#                               i_rain_drizzle > 0.5 |
#                               i_snow_ice > 0.5, 1, 0),
#          wind = if_else(wdsp > 20 | gust > 40, 1, 0),
#          pleasant = if_else(hot + cold + elements + wind == 0, 1, 0),
#          distinct_class = case_when(pleasant == 1 ~ "pleasant",
#                                     hot == 1 ~ "hot",
#                                     cold == 1 ~ "cold", 
#                                     elements == 1 ~ "elements",
#                                     wind == 1 ~ "wind",
#                                     TRUE  ~ NA_character_),
#          double_class =   case_when(pleasant == 1 ~ "pleasant",
#                                     hot == 1 & elements == 1 ~ "hot & elements",
#                                     cold == 1 & elements == 1 ~ "cold & elements",
#                                     hot == 1 ~ "hot",
#                                     cold == 1 ~ "cold", 
#                                     elements == 1 ~ "elements",
#                                     wind == 1 ~ "wind",
#                                     TRUE ~ NA_character_),
#          double_class = factor(double_class, levels = c("pleasant", "elements", "wind", "cold", "cold & elements", "hot", "hot & elements")) 
#   ),
# 
# data_auc = bind_cols(data_pleasant, map2_dfr(data_pleasant$temp_min, data_pleasant$temp_max, get_auc)),
# 
# location_rating = data_auc %>%
#   mutate(year = year(date)) %>% 
#   filter(year < year(today())) %>% 
#   group_by(location_id, year) %>% 
#   summarise(pleasant = sum(pleasant),
#             hot = sum(hot),
#             cold = sum(cold),
#             elements = sum(elements),
#             wind = sum(wind),
#             auc_hot = mean(auc_hot),
#             auc_cold = mean(auc_cold),
#             auc_total = mean(auc_total)) %>%
#   ungroup() %>% 
#   group_by(location_id) %>% 
#   summarise_at(vars(pleasant, hot, cold, elements, wind, auc_hot, auc_cold, auc_total), ~round(mean(.),0)) %>% 
#   ungroup(),
# 
# locations_rated = locations_filtered %>% 
#   left_join(location_rating, by = "location_id"),




#####################






























# ----------------------

# Parameters

## http://www.localweather.net.nz/smf/general-discussion/1/what-is-the-definition-of-a-rain-day-or-a-wet-day/1873/msg11217#msg11217



params <- list(
  temp_max = c(20, 35), #65 - "if it didn't get up to 65F in the warmest hour..."
  temp_min = c(5, 15), #lowest would be night + sunrise temp. Let's rule out near freezing temps.
  #the upper limit is "when even the lowest night temp is too hot..."
  temp_mean = c(13, 24),
  
  prcp = 10,
  sndp = 10
)



data_daily <- data %>% 
  mutate(hot = if_else(temp_min > params$temp_min[2] |
                       temp_max >  params$temp_max[2] |
                       temp_mean > params$temp_mean[2], 1, 0),
         cold = if_else(temp_min <  params$temp_min[1] |
                        temp_max <  params$temp_max[1]  |
                        temp_mean < params$temp_mean[1], 1, 0),
         auc = map2_dbl(temp_min, temp_max, temp_auc),
         elements = if_else(prcp > params$prcp |
                            sndp > params$sndp |
                            i_rain_drizzle > 0.66 |
                            i_snow_ice > 0.66, 1, 0),
         wind = if_else(wdsp > 10, 1, 0),
         pleasant = if_else(hot + cold + elements + wind == 0, 1, 0),
         distinct_class = case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1 ~ "hot",
                                    cold == 1 ~ "cold", 
                                    elements == 1 ~ "elements",
                                    wind == 1 ~ "wind",
                                    TRUE  ~ NA_character_),
         double_class =   case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1 & elements == 1 ~ "hot & elements",
                                    cold == 1 & elements == 1 ~ "cold & elements",
                                    hot == 1 ~ "hot",
                                    cold == 1 ~ "cold", 
                                    elements == 1 ~ "elements",
                                    wind == 1 ~ "wind",
                                    TRUE ~ NA_character_),
         double_class = factor(double_class, levels = c("pleasant", "elements", "wind", "cold", "cold & elements", "hot", "hot & elements"))
  )

saveRDS(data_daily, file = "data/data_daily.rds")
data_daily <- readRDS("data/data_daily.rds")

summary_locations_auc <- data_daily %>%
  filter(year < year(today())) %>% 
  group_by(city, country, lat, lon, capital, population, year) %>% 
  summarise(auc = mean(auc),
            days = n()) %>% 
  ungroup() %>% 
  ## This if_else accounts for cases of leap year with all known days.
  ## It makes sure we don't have negative unknown days
  ## But also levels out leap year for the next step of averaging
  mutate(unknown = if_else(days >= 365, 0, 365 - days)) %>% 
  filter(unknown < 365 * 0.1) %>% 
  #filter(country == "United States") %>% 
  filter(population >= 1000000) %>% 
  group_by(city, country, lat, lon, capital, population) %>% 
  summarise(auc = mean(auc)) %>% 
  ungroup() %>% 
  mutate(rank = row_number(auc),
         rank_rev = row_number(desc(auc)),
         points = auc / max(auc) * 100,
         name = reorder(city, rank))


ggplot(summary_locations_auc, aes(x=lon, y=lat, col = rank)) + geom_point()







summary_locations <- data_daily %>%
  filter(year < year(today())) %>% 
  group_by(city, country, lat, lon, capital, population, year) %>% 
  summarise_at(vars(pleasant, hot, cold, elements, wind), sum) %>% 
  ungroup() %>% 
  ## This if_else accounts for cases of leap year with all known days.
  ## It makes sure we don't have negative unknown days
  ## But also levels out leap year for the next step of averaging
  mutate(unknown = if_else(pleasant + hot + cold + wind >= 365, 0, 365 - pleasant - hot - cold - wind)) %>% 
  filter(unknown < 365 * 0.1) %>% 
  group_by(city, country, lat, lon, capital, population) %>% 
  summarise_at(vars(pleasant, hot, cold, elements, wind, unknown), ~round(mean(.),0)) %>% 
  ungroup() %>% 
  mutate(rank = row_number(desc(pleasant)),
         rank_rev = row_number(pleasant),
         points = pleasant / 365 * 100,
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
            `cold & elements` = "#0571b0",
            IDK = '#000000')

caption <- ("Sources: NOAA Global Summary of the Day, U.S. Census\n taraskaduk.com | @taraskaduk")










f_baseplot <- function(df = summary_locations, 
                       df2 = data_daily, 
                       pop = 0, 
                       n = 20, 
                       dir = "most", 
                       year = 2018, 
                       ncol = 4) 
{
  
  data <- df %>% 
    filter(population > pop) %>% 
    arrange(rank)
  
  if(dir == "most") { 
    data <- head(data, n) %>% 
      mutate(rank = row_number(desc(pleasant)),
             label = paste0(rank, ". ", city, ", \n", country),
             label = reorder(label, rank))
    } 
  else { 
    data <- data %>% 
      mutate(city = fct_rev(city)) %>% 
      tail(n) %>% 
      mutate(rank = row_number(pleasant),
             label = paste0(rank, ". ", city, ", \n", country),
             label = reorder(label, rank))
    }
  
  data <- data %>% 
    inner_join(df2, by = c("city", "lat", "lon"))
  
  ggplot(data) +
    facet_wrap(~label, ncol = ncol) +
    scale_fill_manual(values = colors,
                      name = "Distinct classification",
                      aesthetics = c("colour", "fill")) +
    
    labs(title = paste("Top", n, "cities with", dir, "pleasant days in a year", sep = " "),
         caption = caption)
}





for (dir in c("most")) {
  for (pop in c(1000000)) {
    
    n <- 25
    file <- paste(n, dir, pop/1000, "polar", ".png", sep = "_")
    sub <- paste0("With population over ", comma(pop), " people.\nYears 2014-2019",
    "\nRanked based on years with over 90% of daily data available.",
    "\nVisualizing all data, including incomplete years")
    
    f_baseplot(n = 25, ncol = 7, pop = pop, dir = dir) +
      geom_tile(aes(x=yday, y=year, col = double_class, fill = double_class)) +
      labs(subtitle = sub) +
      theme(axis.text.y = element_blank()) +
      scale_x_continuous(
        # breaks = c(1, 91, 182, 275),
        # label = c("Jan", "Apr", "Jul", "Oct")
        breaks = c(1, 182),
        label = c("January", "July")
      ) +
      expand_limits(y = 2009) +
      theme(strip.text = element_text(face = "bold")) +
      coord_polar()
    
    ggsave(paste0("charts/",file), width = 12, height = 11, units = "in")
    
  }
}







data_daily %>% 
  left_join(summary_locations %>% select(city, lat, lon, rank), by = c("city", "lat", "lon")) %>% 
  filter(rank <= 25) %>% 
ggplot() +
  geom_tile(aes(x=yday, y=year, col = double_class, fill = double_class)) +
  facet_wrap(~city, ncol = 5) +
  scale_x_continuous(
    breaks = c(1, 182),
    label = c("January", "July")
  ) +
  scale_fill_manual(values = colors,
                    name = "Your distinct classification",
                    aesthetics = c("colour", "fill")) +
  labs(title = "Your title",
       subtitle = "Your subtitle",
       caption = "Your caption")




