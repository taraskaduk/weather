library(tidyverse)
library(ggthemes)
library(lubridate)
library(scales)

data_collapsed <-  read_csv("data/data.csv")
locations_filtered <- read_csv("data/locations.csv")

dates <- data_daily %>% 
  select(date) %>% 
  distinct() %>% 
  mutate(year = year(date),
         yday = yday(date),
         )


params <-  list(
  temp_max = c(10, 30), 
  temp_min = c(5, 20), #lowest would be night + sunrise temp. Let's rule out near freezing temps.
  #the upper limit is "when even the lowest night temp is too hot..."
 # temp_mean = c(13, 24),
  
  prcp = 10,
  sndp = 20
)


data_daily <- data_collapsed %>% 
  mutate(hot = if_else(temp_min > params$temp_min[2] |
                         temp_max >  params$temp_max[2], 1, 0),
         cold = if_else(temp_min <  params$temp_min[1] |
                          temp_max <  params$temp_max[1], 1, 0),
         # auc = map2_dbl(temp_min, temp_max, get_auc),
         elements = if_else(prcp > params$prcp |
                              sndp > params$sndp |
                              i_rain_drizzle > 0.66 |
                              i_snow_ice > 0.66, 1, 0),
         # wind = if_else(wdsp > 10, 1, 0),
         pleasant = if_else(hot + cold + elements  == 0, 1, 0),
         distinct_class = case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1 ~ "hot",
                                    cold == 1 ~ "cold", 
                                    elements == 1 ~ "elements",
                                    # wind == 1 ~ "wind",
                                    TRUE  ~ NA_character_),
         double_class =   case_when(pleasant == 1 ~ "pleasant",
                                    hot == 1 & elements == 1 ~ "hot & elements",
                                    cold == 1 & elements == 1 ~ "cold & elements",
                                    hot == 1 ~ "hot",
                                    cold == 1 ~ "cold", 
                                    elements == 1 ~ "elements",
                                    # wind == 1 ~ "wind",
                                    TRUE ~ NA_character_),
         double_class = factor(double_class, levels = c("pleasant", "elements", "cold", "cold & elements", "hot", "hot & elements"))
  )


summary_locations <- data_daily %>%
  mutate(year = year(date)) %>% 
  filter(year < year(today())) %>% 
  group_by(location_id, year) %>% 
  summarise_at(vars(pleasant, hot, cold, elements), sum) %>% 
  ungroup() %>% 
  ## This if_else accounts for cases of leap year with all known days.
  ## It makes sure we don't have negative unknown days
  ## But also levels out leap year for the next step of averaging
  mutate(unknown = if_else(pleasant + hot + cold  >= 365, 0, 365 - pleasant - hot - cold)) %>% 
  filter(unknown < 365 * 0.1) %>% 
  group_by(location_id) %>% 
  summarise_at(vars(pleasant, hot, cold, elements, unknown), ~round(mean(.),0)) %>% 
  ungroup() %>% 
  left_join(locations_filtered, by = "location_id") %>% 
  mutate(rank = row_number(desc(pleasant)),
         rank_rev = row_number(pleasant),
         points = pleasant / 365 * 100,
         name = reorder(name, rank),
         name_short = reorder(name_short, rank),
         )


plot_data <- function(df = summary_locations, 
                      df2 = data_daily, 
                      pop = 1000000, 
                      n = 25, 
                      dir = c("most", "least"), 
                      scope = "world",
                      years = years, 
                      ncol = 5) 
{
  
  caption <-  ("Sources: NOAA Global Summary of the Day, U.S. Census\n taraskaduk.com | @taraskaduk")
  colors <-  c(pleasant = "#1a9641", 
               hot = "#d6604d", 
               cold = "#4393c3", 
               elements = "#bebada",
               # wind = '#e6e6e6',
               `hot & elements` = "#ca0020",
               `cold & elements` = "#0571b0")
  
  
  for (dir in dir) {
    for (pop in pop) {
      
      if(scope != "world") {
        df <- df %>% filter(country == scope)
      }
      data <- df %>% 
        filter(population > pop) %>% 
        arrange(rank)
      
      if(dir == "most") { 
        data <- head(data, n) %>% 
          mutate(rank = row_number(desc(pleasant)),
                 label = paste0(rank, ". ", name_short, ", \n", country),
                 label = reorder(label, rank))
      } else { 
        data <- data %>% 
          mutate(city = fct_rev(name_short)) %>% 
          tail(n) %>% 
          mutate(rank = row_number(pleasant),
                 label = paste0(rank, ". ", name_short, ", \n", country),
                 label = reorder(label, rank))
      }
      
      file <- paste(n, dir, pop/1000, "polar", ".png", sep = "_")
      sub <- paste0("With population over ", comma(pop), " people.\nYears ", min(years), "-", max(years),
                    "\nRanked based on years with over 90% of daily data available.",
                    "\nVisualizing all data, including incomplete years")
      
      data <- data %>% 
        rename(total_pleasant = pleasant,
               total_hot = hot,
               total_cold = cold,
               total_elements = elements,
               # total_wind = wind,
               total_unknown = unknown) %>% 
        inner_join(df2, by = c("location_id")) %>% 
        mutate(year = year(date),
               yday = yday(date))
      
      theme_set(theme_fivethirtyeight()+
                  theme(rect = element_blank(),
                        panel.border = element_blank(),
                        strip.background = element_blank(),
                        panel.grid.major = element_blank(),
                        axis.title=element_blank())
      )
      
      ggplot(data) +
        geom_tile(aes(x=yday, y=year, col = double_class, fill = double_class)) +
        facet_wrap(~label, ncol = ncol) +
        scale_fill_manual(values = colors,
                          name = "Distinct classification",
                          aesthetics = c("colour", "fill")) +
        labs(title = paste("Top", n, "cities with", dir, "pleasant days in a year", sep = " "),
             caption = caption,
             subtitle = sub) +
        scale_x_continuous(
          # breaks = c(1, 91, 182, 275),
          # label = c("Jan", "Apr", "Jul", "Oct")
          breaks = c(1, 182),
          label = c("January", "July")
        ) +
        expand_limits(y = min(years)-length(years)) +
        theme(strip.text = element_text(face = "bold", size = 10),
              axis.text.y = element_blank()) +
        coord_polar()
      
      ggsave(file, width = 10, height = 16, units = "in")
    }
  }
}



years <- data_daily %>% 
  mutate(year = year(date)) %>% 
  select(year) %>% 
  distinct() %>% 
  as_vector()

plot_data(df = summary_locations, 
                         df2 = data_daily, 
                         pop = 1000000, 
                         n = 25, 
                         dir = c("most", "least"), 
                        scope = "United States",
                         years = years,
                         ncol = 5)
