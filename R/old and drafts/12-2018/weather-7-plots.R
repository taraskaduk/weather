library(ggthemes)
library(scales)
library(maps)
library(lubridate)
library(tidyverse)
##library(urbnmapr)

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



data_daily <- readRDS("rds/6-data-daily.RDS")
pleasant_daily <- readRDS("rds/6-pleasant-daily.RDS")
summary_avg <- readRDS("rds/6-summary-avg.RDS")
summary_coords <- readRDS("rds/6-summary-coords.RDS")
summary_locations <- readRDS("rds/6-summary-locations.RDS")



## Relocation AK and HI
## https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/




# Pixel map  -------------------------------------------------------------------

lat <- tibble(lat05 = seq(-90, 90, by = .5))
lon <- tibble(lon05 = seq(-180, 180, by = .5))
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
ggsave("map.png", width = 12, height = 8, units = "in")



# Ranks -------------------------------------------------------------------
f_baseplot <- function(df = summary_locations, 
                       df2 = pleasant_daily, 
                       pop = 0, 
                       metro = "all", 
                       n = 20, 
                       dir = "most", 
                       year = 2017, 
                       ncol = 4) 
{
  
  data <- df %>% 
    filter(pop17 > pop & (metro == "all" | lsad == metro)) %>% 
    arrange(rank)
  
  if(dir == "most") {
    data <- head(data, n)
  } else data <- data %>% mutate(name_wrapped = fct_rev(name_wrapped)) %>% tail(n)
  
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
                      aesthetics = c("colour", "fill")) +

    labs(title = paste("Top", n, word_metro, "areas with", dir, "pleasant days in a year", sep = " "),
         caption = caption)
}

for (dir in c("most", "least")) {
  for (metro in c("all", "M1")) {
    for (ncol in c(5,10)) {
      
      n <- 50
      w <- if_else(ncol == 5, 10, 20)
      h <- if_else(ncol == 5, 15, 10)
      file <- paste(n, dir, metro, ncol, "cols", ".png", sep = "_")
      
      f_baseplot(metro = metro, n = n, ncol = ncol, dir = dir) +
        geom_tile(aes(x=day, y = month, fill = double_class), col = "black") +
        scale_y_discrete(breaks = c("Jan", "Apr", "Jul", "Oct")) +
        scale_x_continuous(breaks = c(1,15,30)) +
        labs(subtitle = "Ranking based on years 2012 through 2017\nDisplaying year 2017") +
        coord_equal()
      
      ggsave(file, width = w, height = h, units = "in")
    }
  }
}



for (dir in c("most", "least")) {
    for (pop in c(500000,1000000)) {
      
    n <- 25
    file <- paste(n, dir, pop/1000, "polar", ".png", sep = "_")
    sub <- paste0("With population over ", comma(pop), " people.\nYears 2012-2017")
    
    f_baseplot(metro = "M1", n = 25, ncol = 7, pop = pop, dir = dir) +
      geom_tile(aes(x=yday, y=year, col = double_class, fill = double_class)) +
      labs(subtitle = sub) +
      theme(axis.text.y = element_blank()) +
      scale_x_continuous(
        # breaks = c(1, 91, 182, 275),
        # label = c("Jan", "Apr", "Jul", "Oct")
        breaks = c(1, 182),
        label = c("January", "July")
      ) +
      expand_limits(y = 2007) +
      theme(strip.text = element_text(face = "bold")) +
      coord_polar()
    
    ggsave(file, width = 12, height = 11, units = "in")
      
  }
}









  
summary_locations %>% 
    filter(pop17 > 1000000 & lsad == "M1") %>% 
    arrange(rank) %>% 
    head(25) %>% 
  mutate(name_edited = fct_reorder(name_edited,rank),
         name_short = fct_reorder(name_short,rank)) %>% 
    inner_join(pleasant_daily %>% filter(year == year), by = "cbsafp") %>% 
  

  ggplot(aes(x=yday, y=year, col = double_class, fill = double_class)) +
    geom_tile() +
    facet_grid(name_short ~ ., switch = "y") +
    scale_x_continuous(
      # breaks = c(1, 91, 182, 275),
      # label = c("Jan", "Apr", "Jul", "Oct")
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      label = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    ) +

    theme(axis.text.y = element_blank(),
          strip.text.y = element_text(angle = 180)) +
    scale_fill_manual(values = colors,
                      name = "Distinct classification",
                      aesthetics = c("colour", "fill")) +
    
    labs(title = "Top 25 metro areas with most pleasant days in a year",
         subtitle = "Displaying years 2012 - 2017",
         caption = caption)

ggsave("new.png", width = 10, height = 8, units = "in", dpi = "retina")




# to share

# Your data is of a daily grain. Each day is marked with a distinct class.
# In my case: pleasant, elements, hot, cold, hot & elements, cold & elements
ggplot(your_data) +
  # Main geom is geom_tile(): x is day of the year (1 through 365 or 366, use yday()), y is year.
  geom_tile(aes(x=yday, y=year, col = distinct_class, fill = distinct_class)) +
  # This is to make a donut hole
  expand_limits(y = 2007) +
  #And this makes a donut
  coord_polar() +
  # And this makes many donuts: one per city in my case
  facet_wrap(~city, ncol = 5) +
    
  # The rest is just aesthetics.
  
  # This one marks "January" and "July" on the donut 
  scale_x_continuous(
    breaks = c(1, 182),
    label = c("January", "July")
  ) +
  # Your customizations go here
  theme() +
  # Legend cleanup
  scale_fill_manual(values = your_colors_vector,
                    name = "Your distinct classification",
                    aesthetics = c("colour", "fill")) +
  # Title and caption
  labs(title = "Your title",
       subtitle = "Your subtitle",
       caption = "Your caption")
