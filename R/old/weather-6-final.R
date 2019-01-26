setwd("weather")

library(ggthemes)
library(scales)
library(maps)
library(lubridate)
library(tidyverse)


data <- readRDS("data/5-data-predicted.RDS")
locations <- readRDS("data/2-locations-filtered.RDS")

locations <- locations %>% 
  mutate(name_short = str_sub(name, start = 1, end = if_else(is.na(str_locate(name, "-")[,1]), str_locate(name, ",")[,1], str_locate(name, "-")[,1]) -1))

## Notes
# https://www.coynecollege.edu/news-events/ideal-temperatures-heat-cool
# https://weather.com/news/news/how-hot-is-too-hot-survey
# https://en.wikipedia.org/wiki/Rain#Intensity


# Parameters --------------------------------------------------------------

p_temp_max <- c(60, 85) #65 - "if it didn't get up to 65F in the warmest hour..."
p_temp_min <- c(40, 70) #lowest would be night + sunrise temp. Let's rule out near freezing temps.
                        #the upper limit is "when even the lowest night temp is too hot..."
p_temp_mean <- c(55, 75)
p_precip <- 0.1
p_snow <- 0.5



# Data --------------------------------------------------------------------

pleasant <- data %>% 
  left_join(locations %>% select(cbsafp, name, name_short, lsad, lat = intptlat, lon = intptlon, pop17), by = "cbsafp") %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         lat0 = round(lat,0),
         lon0 = round(lon,0),
         lat05 = round(2*lat,0)/2,
         lon05 = round(2*lon,0)/2) %>% 
  mutate(pls_temp = case_when(temp_min > p_temp_min[2] ~ 'hot',
                              temp_min < p_temp_min[1] ~ 'cold',
                              temp_max > p_temp_max[2] ~ 'hot',
                              temp_max < p_temp_max[1] ~ 'cold',
                              temp_mean > p_temp_mean[2] ~ 'hot',
                              temp_mean < p_temp_mean[1] ~ 'cold',
                              TRUE ~ "pleasant"),
         ## keep tweaking this one... What's the "pleasant" amount of elements???
         pls_elements = if_else(precip <= p_precip &
                                snow <= p_snow &
                                is_rain <= .5 &
                                is_snow <= .5,
                                "pleasant",
                                "elements"),
         pleasant = if_else(pls_temp == 'pleasant' & pls_elements == "pleasant",
                            1,
                            0),
         distinct_class = case_when(pleasant == 1 ~ "pleasant",
                                    pls_temp == "hot" ~ "hot",
                                    pls_elements == "elements" ~ "elements",
                                    pls_temp == "cold" ~ "cold",
                                    TRUE          ~ NA_character_),
         double_class =   case_when(pleasant == 1 ~ "pleasant",
                                    pls_temp == "hot" & pls_elements != "elements"  ~ "hot",
                                    pls_temp == "hot" & pls_elements == "elements"  ~ "hot & elements",
                                    pls_temp == "cold" & pls_elements != "elements"  ~ "cold",
                                    pls_temp == "cold" & pls_elements == "elements"  ~ "cold & elements",
                                    pls_elements == "elements"    ~ "elements",
                                    TRUE          ~ NA_character_))



# MAP ---------------------------------------------------------------------

pleasant_summary <- pleasant %>% 
  
  group_by(cbsafp, lat05, lon05, year) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(lat05, lon05) %>% 
  summarise(pleasant = mean(pleasant)) %>% 
  mutate(pleasant_cat = case_when(pleasant >=300 ~ "Over 300",
                                  pleasant >=200 ~ "200 - 299",
                                  pleasant >=100 ~ "100 - 199",
                                  pleasant >=50 ~ "50 - 99",
                                  TRUE ~ "Less than 50"),
         pleasant_cat = factor(pleasant_cat, levels = c("Less than 50", "50 - 99", "100 - 199", "200 - 299", "Over 300")))


lat <- data_frame(lat05 = seq(-90, 90, by = .5))
lon <- data_frame(lon05 = seq(-180, 180, by = .5))
dots <- lat %>% 
  merge(lon, all = TRUE)

dots <- dots %>% 
  mutate(country = maps::map.where('state', lon05, lat05),
         lakes = maps::map.where('lakes', lon05, lat05)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)






summary_metro <- pleasant %>%
  filter(lsad == 'M1') %>% 
  group_by(cbsafp, name, name_short, year, pop17) %>% 
  summarise(pleasant = sum(pleasant)) %>% 
  ungroup() %>% 
  group_by(cbsafp, name, name_short, pop17) %>% 
  summarise(pleasant_days = mean(pleasant)) %>% 
  ungroup() %>% 
  mutate(rank = row_number(desc(pleasant_days)),
         rank_rev = row_number(pleasant_days),
         points = round(percent_rank(desc(pleasant_days)) * 100,0),
         name = reorder(name, rank),
         name_short = reorder(name_short, rank),
         pop17 = as.integer(pop17)) %>% 
  filter(!is.na(rank))




pleasant_metro <- pleasant %>% 
  select(-c(name, name_short, pop17)) %>% 
  inner_join(summary_metro, 
             by = "cbsafp")



summary2 <- pleasant %>%
  filter(lsad == 'M1') %>% 
  group_by(cbsafp, name, year, pop17, double_class) %>% 
  count() %>%
  filter(!is.na(double_class)) %>% 
  
  group_by(cbsafp, name, year, pop17) %>% 
  mutate(sum_n = sum(n)) %>% 
  
  group_by(cbsafp, name, pop17, double_class) %>% 
  summarise(n = sum(n)) %>% 
  
  group_by(cbsafp, name, pop17) %>% 
  mutate(n = 365*n/sum(n)) %>% 
  ungroup() %>% 
  
  replace_na(list(n = 0)) %>% 
  spread(double_class, n) %>% 
  mutate(rank = min_rank(desc(pleasant)),
         name = reorder(name, desc(rank)),
         pop17 = as.integer(pop17)) %>% 
  gather(class, n, -c(cbsafp, name, pop17,name, rank)) %>% 
  filter(!is.na(rank) & class != "<NA>")



# Plots -------------------------------------------------------------------

theme_set(theme_fivethirtyeight())


colors <- c(pleasant = "#1a9641", 
            hot = "#d6604d", 
            cold = "#4393c3", 
            elements = "#bebada",
            `hot & elements` = "#ca0020",
            `cold & elements` = "#0571b0")


plot_map <- ggplot() + 
  geom_point(data = dots, aes(x=lon05, y = lat05), col = "grey90", size = 3) +
  #geom_point(data = pleasant_summary %>% filter(pleasant_cat == "Over 300"), aes(x=lon05, y=lat05), col = "red", size = 9, alpha = 0.4) +
  #geom_point(data = pleasant_summary %>% filter(pleasant_cat == "200 - 299"), aes(x=lon05, y=lat05), col = "red", size = 8, alpha = 0.3) +
  geom_point(data = pleasant_summary, aes(x=lon05, y=lat05, col=pleasant_cat), size = 3) +
  # scale_color_brewer(type = "seq", name = "Pleasant days") +
  scale_x_continuous(limits = c(-125, -60)) +
  scale_y_continuous(limits = c(25, 50)) +
  #scale_color_brewer(type = "div", palette = "RdBu")+
  scale_colour_manual(values = c("#bdd7e7",
                                 "#6baed6", 
                                 "#2171b5", 
                                 "#08306b"), 
                      name = "Pleasant days") +
  theme_fivethirtyeight() +
  theme(
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())+
  labs(title = "Places with most pleasant days in a year",
       caption = "A pleasant day is a day when the min temp was over 45F, the max temp was under 85F, the mean temp was between 55F and 75F, no significant rain or snow. \n
       Average of years 2012 - 2017 of NOAA Global Summary of the Day data\n
       taraskaduk.com | @taraskaduk")



f_cal <- function(data = pleasant_metro, year = 2017, ncol = 3) {
  data %>% 
    filter(year == year) %>% 
    mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
    ggplot(aes(x=day, y = month, fill = double_class)) +
    geom_tile(col = "black") +
    facet_wrap(~name_short, ncol = ncol) +
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

f_cal(pleasant_metro %>% filter(rank <= 25))
f_cal(pleasant_metro %>% filter(rank_rev <= 25))
f_cal(pleasant_metro %>% filter(pop17 > 1000000), ncol = 5)




f_city <- function(city, data = summary_metro, ncol = 3) {
  
  pleasant %>% 
    inner_join(data %>% filter(str_detect(name, city) == TRUE), by = "cbsafp") %>% 
    mutate(month = factor(format(date, "%b"), levels = rev(month.abb))) %>% 
    ggplot(aes(x=day, y = month, fill = double_class)) +
    geom_tile(col = "black") +
    facet_wrap(~year) +
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



f_city("Seattle")


summary2 %>% 
  filter(rank < 50) %>% 
  ggplot(aes(x = name, y = n, fill = class)) +
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

summary2 %>% 
  filter(rank > max(summary2$rank) - 50) %>% 
  ggplot(aes(x = name, y = n, fill = class)) +
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
