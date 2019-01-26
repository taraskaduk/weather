setwd("weather")

library(ggthemes)
library(scales)
library(maps)
library(lubridate)
library(tidyverse)
##library(urbnmapr)

theme_set(theme_fivethirtyeight())

colors <- c(pleasant = "#1a9641", 
            hot = "#d6604d", 
            cold = "#4393c3", 
            elements = "#bebada",
            `hot & elements` = "#ca0020",
            `cold & elements` = "#0571b0")

caption <- ("Sources: NOAA Global Summary of the Day, U.S. Census\n taraskaduk.com | @taraskaduk")



data_daily <- readRDS("data/6-data-daily.RDS")
pleasant_daily <- readRDS("data/6-pleasant-daily.RDS")
summary_avg <- readRDS("data/6-summary-avg.RDS")
summary_coords <- readRDS("data/6-summary-coords.RDS")
summary_locations <- readRDS("data/6-summary-locations.RDS")



## Relocation AK and HI
## https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/




# Pixel map  -------------------------------------------------------------------

lat <- data_frame(lat05 = seq(-90, 90, by = .5))
lon <- data_frame(lon05 = seq(-180, 180, by = .5))
dots <- lat %>% 
  merge(lon, all = TRUE)

dots <- dots %>% 
  mutate(country = maps::map.where('state', lon05, lat05),
         lakes = maps::map.where('lakes', lon05, lat05)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)


plot_map <- ggplot() + 
  geom_point(data = dots, aes(x=lon05, y = lat05), col = "grey90", size = 2) +
  #geom_point(data = pleasant_summary %>% filter(pleasant_cat == "Over 300"), aes(x=lon05, y=lat05), col = "red", size = 9, alpha = 0.4) +
  #geom_point(data = pleasant_summary %>% filter(pleasant_cat == "200 - 299"), aes(x=lon05, y=lat05), col = "red", size = 8, alpha = 0.3) +
  geom_point(data = summary_coords, aes(x=lon05, y=lat05, col=pleasant_cat), size = 2) +
  # scale_color_brewer(type = "seq", name = "Pleasant days") +
  coord_map(projection = "albers", parameters = c(25,50)) +
  scale_x_continuous(limits = c(-125, -65)) +
  scale_y_continuous(limits = c(25, 50)) +
  #scale_color_brewer(type = "div", palette = "RdBu")+
  scale_colour_manual(values = c("#bdd7e7",
                                 "#6baed6", 
                                 "#2171b5", 
                                 "#08306b"), 
                      name = "Pleasant days") +
  theme(
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = caption)

plot_map




# Another map??? ----------------------------------------------------------

# counties <- urbnmapr::counties
#   
# cbsa_to_fips <- read_csv("data/0-raw/cbsa2fipsxw.csv") %>% 
#   filter(!is.na(cbsacode)) %>% 
#   select(cbsacode, fipsstatecode, fipscountycode) %>% 
#   mutate_all(as.character) %>% 
#   mutate(fipscountycode = str_pad(fipscountycode, 3, side = "left", pad = "0"),
#          fipsstatecode = str_pad(fipsstatecode, 2, side = "left", pad = "0"),
#          county_fips = paste0(fipsstatecode, fipscountycode))
# 
# 
# ## Something is wrong here...
# pleasant_counties <- pleasant_daily %>% 
#   head(1000) %>% 
#   left_join(cbsa_to_fips, by = c('cbsafp' = 'cbsacode')) %>% 
#   left_join(counties, by = c('county_fips'))


# Ranks -------------------------------------------------------------------


f_baseplot <- function(df = summary_locations, df2 = pleasant_daily, pop = 0, metro = "all", n = 20, dir = "most", year = 2017, ncol = 4) {
  
  data <- df %>% 
    filter(pop17 > pop & (metro == "all" | lsad == metro)) %>% 
    arrange(rank)
  
  if(dir == "most") {
    data <- head(data, n)
  } else data <- tail(data, n)
  
  if(metro == "M1") {
    word_metro <- "metropolitan"
  } else {
    if (metro == "M2") {word_metro <- "micropolitan"} else {
      word_metro <- "metro and micro"
    }
  } 
  
  data <- data %>% 
    inner_join(df2 %>% filter(year == year), by = "cbsafp")
  
  ggplot(data) +
    facet_wrap(~name_wrapped, ncol = ncol) +
    scale_fill_manual(values = colors,
                      name = "Distinct classification",
                      aesthetics = c("colour", "fill"))+
    theme(panel.grid.major = element_blank(),
          #axis.ticks=element_blank(),
          axis.title=element_blank()) +

    labs(title = paste("Top", n, word_metro, "areas with", dir, "pleasant days in a year", sep = " "),
         caption = caption)
}


f_baseplot(metro = "M1", n = 50, ncol = 10, dir = "fewest", pop = 500000) +
  geom_tile(aes(x=day, y = month, fill = double_class), col = "black") +
  scale_y_discrete(breaks = c("Jan", "Apr", "Jul", "Oct")) +
  scale_x_continuous(breaks = c(1,15,30)) +
  labs(subtitle = "With population over 500,000 people\nRanking based on years 2012 through 2017\nDisplaying year 2017") +
  coord_equal()


f_baseplot(metro = "M1", n = 20, ncol = 5, pop = 1000000) +
  geom_tile(aes(x=yday, y=year, col = double_class, fill = double_class)) +
  labs(subtitle = "With population over 1,000,000 people.\nYears 2012-2017") +
  theme(axis.text.y = element_blank()) +
  scale_x_continuous(
    breaks = c(1, 91, 182, 275),
    label = c("Jan", "Apr", "Jul", "Oct")
  ) +
  expand_limits(y = 2007) +
  coord_polar()

ggsave("polar.png")
















f_cal3 <- function(df = summary_locations, df2 = pleasant_daily, pop = 0, metro = "all", n = 20, dir = "most", ncol = 4) {
  
  data <- df %>% 
    filter(pop17 > pop & (metro == "all" | lsad == metro)) %>% 
    arrange(rank)
  
  if(dir == "most") {
    data <- head(data, n)
  } else data <- tail(data, n)
  
  
  data2 <- df2
  
  data_plot <- data %>% 
    inner_join(data2, by = "cbsafp")
  
  ggplot(data_plot, aes(x=yday, y=year, col = double_class, fill = double_class)) +
    geom_tile() +
    scale_color_manual(values = colors,
                       name = "Distinct classification",
                       aesthetics = c("colour", "fill")) +
    theme(panel.grid.major = element_blank(),
          axis.title=element_blank(),
          axis.text.y = element_blank(),
          axis.ticks=element_blank()
    ) +
    scale_x_continuous(
      breaks = c(1, 91, 182, 275),
      label = c("Jan", "Apr", "Jul", "Oct")
    ) +
    expand_limits(y = 2007) +
    coord_polar()+
    facet_wrap(~name, ncol = ncol) +
    labs(title = paste("Top", n, "places with", dir, "pleasant days in a year", sep = " "),
         caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")
}

f_cal3(metro = "M1", n = 15, ncol = 3)



f_cal2 <- function(df = summary_locations, df2 = pleasant_daily, pop = 0, metro = "all", n = 20, dir = "top", yr = 2017, ncol = 4) {
  
  data <- df %>% 
    filter(pop17 > pop & (metro == "all" | lsad == metro)) %>% 
    arrange(rank)
  
  if(dir == "top") {
    data <- head(data, n)
  } else data <- tail(data, n)
  
  data2 <- df2 %>% 
    filter(year == yr)
  
  data_plot <- data %>% 
    inner_join(data2, by = "cbsafp")
  
  ggplot(data_plot, aes(x=date, name, fill = double_class)) +
    geom_tile(col = "black") +
    scale_fill_manual(values = colors,
                      name = "Distinct classification")+
    theme(panel.grid.major = element_blank()) +
    #coord_equal() +
    labs(title = paste("Top", n, "places with", dir, "pleasant days in a year", sep = " "),
         caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")
}

f_cal2(metro = "M1", yr = 2017, n = 200, ncol = 5)















f_city <- function(city, data = pleasant_daily) {
  
city_str <-  paste(city, collapse ="|") 
  
data %>% filter(str_detect(name, city_str) == TRUE) %>% 
    mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
    ggplot(aes(x=day, y = month, fill = double_class)) +
    geom_tile(col = "black") +
    facet_grid(year ~ name) +
    scale_fill_manual(values = c(pleasant = "#1a9641", 
                                 hot = "#d6604d", 
                                 cold = "#4393c3", 
                                 elements = "#bebada",
                                 `hot & elements` = "#ca0020",
                                 `cold & elements` = "#0571b0"), 
                      name = "Distinct classification")+
    theme(panel.grid.major = element_blank()) +
    coord_equal()
  
}



f_city(city = c("Seattle", "Los Angeles", "Jacksonville"))


pleasant_yearly %>% 
  filter(rank < 50) %>% 
  ggplot(aes(x = name, y = n, fill = double_class)) +
    geom_col() +
    theme_fivethirtyeight() + 
    scale_fill_manual(values = c(pleasant = "#1a9641", 
                                 hot = "#d6604d", 
                                 cold = "#4393c3", 
                                 elements = "#bebada",
                                 `hot & elements` = "#ca0020",
                                 `cold & elements` = "#0571b0"), 
                      name = "Distinct classification")+
    theme(panel.grid.major = element_blank()) +
    coord_flip()

pleasant_yearly %>% 
  filter(rank_rev < 25) %>% 
  ggplot(aes(x = name, y = n, fill = double_class)) +
  geom_col(
    #width = 0.5
    )+
  theme_fivethirtyeight() + 
  scale_fill_manual(values = c(pleasant = "#1a9641", 
                               hot = "#d6604d", 
                               cold = "#4393c3", 
                               elements = "#bebada",
                               `hot & elements` = "#ca0020",
                               `cold & elements` = "#0571b0"), 
                    name = "Distinct classification")+
  theme(panel.grid.major = element_blank()) +
  #geom_hline(yintercept = seq(1,364, by = 1), col = "grey94", size = 0.5)+
  coord_flip()
