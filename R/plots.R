library(ggthemes)
library(lubridate)
library(scales)
library(tidyverse)

source(Sys.getenv("theme_url"))
theme_set(theme_tk()+
            theme(rect = element_blank(),
                  panel.border = element_blank(),
                  strip.background = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title=element_blank(),
                  axis.text = element_blank(),
                  plot.title = element_text(size = rel(2),
                                            family="Oswald"),
                  plot.subtitle = element_text(size = rel(1.3),
                                               family="Oswald")))

data_daily <-  readRDS("data/data.RDS")
summary_locations <- readRDS("data/summary_locations.RDS") %>% 
  filter(!(city %in% c("Bronx","Manhattan","Brooklyn", "Queens"))) 


dates <- data_daily %>% 
  select(date) %>% 
  distinct() %>% 
  mutate(year = year(date),
         yday = yday(date),
         )

years <- data_daily %>% 
  mutate(year = year(date)) %>% 
  select(year) %>% 
  distinct() %>% 
  as_vector()

caption <-  ("Sources: NOAA Global Summary of the Day, U.S. Census\n taraskaduk.com | @taraskaduk")
colors <-  c(pleasant = "#1a9641", 
             hot = "#d6604d", 
             cold = "#4393c3", 
             elements = "#bebada",
             # wind = '#e6e6e6',
             `hot & elements` = "#ca0020",
             `cold & elements` = "#0571b0")

colors2 <- c(`Hot EDD`  = "#ca0020", 
             `Cold EDD` = "#0571b0", 
             `Total EDD` = "#3B325D")

plot_data <- function(df = summary_locations, 
                      df2 = data_daily, 
                      pop = 1000000, 
                      n = 25, 
                      dir = c("most", "least"), 
                      scope = "world",
                      years = years, 
                      ncol = 5,
                      width = 10,
                      height = 16) 
{
  
  for (dir in dir) {
    for (pop in pop) {
      
      if(scope != "world") {
        df <- df %>% 
          filter(country == scope) %>% 
          mutate(name = paste(city, admin_name, sep=",\n"))
      } else {
        df <- df %>% 
          mutate(name = paste(city, country, sep=",\n"))
      }
      data <- df %>% 
        filter(population > pop) %>%
        arrange(desc(pleasant))
      
      if(dir == "most") { 
        data <- head(data, n) %>% 
          mutate(rank = row_number(desc(pleasant)),
                 label = paste0(rank, ". ", name),
                 label = reorder(label, rank))
      } else { 
        data <- data %>% 
          tail(n) %>% 
          mutate(rank = row_number(pleasant),
                 label = paste0(rank, ". ", name),
                 label = reorder(label, rank))
      }
      
      file <- paste(n, dir, scope, pop/1000, "polar", ".png", sep = "_")
      sub <- paste0("Cities with population over ", comma(pop), " people.",
                    "\nYears ", min(years), " (inward layer) to ", max(years), " (outward layer),",
                    "\nvisualized Jan 1 to Dec 31 clockwise.",
                    "\n")
      
      data <- data %>% 
        rename(total_pleasant = pleasant,
               total_hot = hot,
               total_cold = cold,
               total_elements = elements,
               # total_wind = wind,
               total_unknown = unknown) %>% 
        inner_join(df2, by = c("location_id")) %>% 
        mutate(year = year(date),
               yday = yday(date)) %>% 
        filter(yday != 366)
      
      
      p <- ggplot(data) +
        geom_tile(aes(x=yday, y=year, col = double_class, fill = double_class)) +
        facet_wrap(~label, ncol = ncol) +
        scale_fill_manual(values = colors,
                          name = "Distinct classification",
                          aesthetics = c("colour", "fill")) +
        labs(title = paste("Top", n, scope, "cities with", dir, "pleasant days in a year", sep = " "),
             caption = caption,
             subtitle = sub) +
        # scale_x_continuous(
        #   # breaks = c(1, 91, 182, 275),
        #   # label = c("Jan", "Apr", "Jul", "Oct")
        #   breaks = c(1, 182),
        #   label = c("January", "July")
        # ) +
        expand_limits(y = min(years)-length(years)) +
        coord_polar() +
        theme(#axis.text.x = element_text(size = rel(1-ncol/50)),
              strip.text = element_text(face = "bold", size = rel(7/ncol))
              )
      
      # # https://stackoverflow.com/questions/36779537/ggplot2-facet-wrap-y-axis-scale-on-the-first-row-only/36780639#36780639
      # p_tab <- ggplotGrob(p)
      # print(p_tab)
      # 
      # gtable_filter_remove <- function (x, name, trim = TRUE){
      #   matches <- (str_detect(x$layout$name,"axis-t")==FALSE | x$layout$name == "axis-t-1-1")
      #   x$layout <- x$layout[matches, , drop = FALSE]
      #   x$grobs <- x$grobs[matches]
      #   if (trim)
      #     x <- gtable_trim(x)
      #   x
      # }
      # 
      # p_filtered <- gtable_filter_remove(p_tab, trim = FALSE)
      # 
      # grid.newpage()
      # grid.draw(p_filtered)
      # ggsave(file, width = 10, height=6+1.35*n/ncol, units = "in", limitsize = FALSE)
      
      ggsave(file, 
             p,
             width = width, 
             height=height,
             units = "in",
             limitsize = FALSE)
    }
  }
}

plot_data(df = summary_locations, 
                         df2 = data_daily, 
                         pop = 1000000, 
                         n = 25, 
                         dir = "most", 
                         scope = "United States",
                         years = years,
                         ncol = 5,
          width = 10,
          height = 16)


plot_data(df = summary_locations, 
          df2 = data_daily, 
          pop = 1000000, 
          n = 100, 
          dir = c("most", "least"),
          scope = "world",
          years = years,
          ncol = 10,
          width = 10,
          height = 17)

plot_data(df = summary_locations, 
          df2 = data_daily, 
          pop = 1000000, 
          n = 50, 
          dir = "most",
          scope = "world",
          years = years,
          ncol = 5,
          width = 10,
          height = 36)

plot_data(df = summary_locations, 
          df2 = data_daily, 
          pop = 1000000, 
          n = 100, 
          dir = "most",
          scope = "United States",
          years = years,
          ncol = 10,
          width = 10,
          height = 17)

plot_data(df = summary_locations, 
          df2 = data_daily, 
          pop = 1000000, 
          n = 100, 
          dir = "most",
          scope = "United States",
          years = years,
          ncol = 5,
          width = 10,
          height = 50)









# Testing -----------------------------------------------------------------
df= summary_locations
df2 = data_daily
pop = 1000000
n = 25
dir = "most"
scope = "United States"
years = years
ncol = 5
width = 10
height = 26




if(scope != "world") {
  df <- df %>% 
    filter(country == scope) %>% 
    mutate(name = paste(city, admin_name, sep=",\n"))
} else {
  df <- df %>% 
    mutate(name = paste(city, country, sep=",\n"))
}

if(nrow(filter(df, population > pop)) < n) {
  data <- df %>% 
    arrange(desc(population)) %>% 
    head(n) %>%
    arrange(edd_total)
} else {
  data <- df %>% 
    filter(population > pop) %>%
    arrange(edd_total)
}


if(dir == "most") { 
  data <- head(data, n) %>% 
    mutate(rank = row_number(edd_total),
           label = paste0(rank, ". ", name),
           label = reorder(label, rank))
} else { 
  data <- data %>% 
    tail(n) %>% 
    mutate(rank = row_number(desc(edd_total)),
           label = paste0(rank, ". ", name),
           label = reorder(label, rank))
}

file <- paste(n, dir, scope, pop/1000, "polar", ".png", sep = "_")
sub <- paste0("Cities with population over ", comma(pop), " people.",
              "\nYears ", min(years), " (inward layer) to ", max(years), " (outward layer), ",
              "visualized Jan 1 to Dec 31 clockwise.",
              "\n")

data2 <- data %>% 
  select(location_id,
         label,
         avg_edd_hot = edd_hot,
         avg_edd_cold = edd_cold,
         avg_edd_total = edd_total) %>% 
  inner_join(df2 %>% 
               select(location_id,
                      date,
                      edd_hot,
                      edd_cold,
                      edd_total), by = c("location_id")) %>% 
  mutate(year = year(date),
         yday = yday(date),
         edd_hot = rescale(edd_hot, to=c(0,1)),
         edd_cold = rescale(edd_cold, to=c(0,1))
         ) %>% 
  filter(yday != 366)


p <- ggplot(data2) +
  geom_tile(aes(x=yday, y=year, alpha = edd_cold), 
            #col = colors[["cold"]],
            fill = colors[["cold & elements"]]) +
  geom_tile(aes(x=yday, y=year, alpha = edd_hot), 
            #col = colors[["hot"]],
            fill = colors[["hot & elements"]]) +
  
  facet_wrap(~label, ncol = ncol) +
  labs(title = paste("Top", n, scope, "cities with", dir, "pleasant days in a year", sep = " "),
       caption = caption,
       subtitle = sub) +
  expand_limits(y = min(years)-length(years)) +
  coord_polar() +
  theme(#axis.text.x = element_text(size = rel(1-ncol/50)),
    strip.text = element_text(face = "bold", size = rel(7/ncol)),
    legend.position = "none"
  )

ggsave(file, 
       p,
       width = width, 
       height=height,
       units = "in",
       limitsize = FALSE)



# Testing 2 ---------------------------------------------------------------



df = summary_locations
df2 = data_daily
pop = 1000000
n = 10
dir = "most"
scope = "United States"
years = years
width = 10
height = 20



if(scope != "world") {
  df <- df %>% 
    filter(country == scope) %>% 
    mutate(name = paste(city, admin_name, sep=",\n"))
} else {
  df <- df %>% 
    mutate(name = paste(city, country, sep=",\n"))
}
data <- df %>% 
  filter(population > pop) %>%
  arrange(edd_total)

if(dir == "most") { 
  data2 <- head(data, n) %>% 
    mutate(rank = row_number(edd_total),
           label = paste0(rank, ". ", name),
           label = reorder(label, rank))
} else { 
  data2 <- data %>% 
    tail(n) %>% 
    mutate(rank = row_number(desc(edd_total)),
           label = paste0(rank, ". ", name),
           label = reorder(label, rank))
}

file <- paste(n, dir, scope, pop/1000, "polar", ".png", sep = "_")
sub <- paste0("Cities with population over ", comma(pop), " people.",
              "\nYears ", min(years), " (inward layer) to ", max(years), " (outward layer), ",
              "visualized Jan 1 to Dec 31 clockwise.",
              "\n")

data3 <- data2 %>% 
  dplyr::select(location_id,
         label,
         avg_edd_hot = edd_hot,
         avg_edd_cold = edd_cold,
         avg_edd_total = edd_total) %>% 
  inner_join(df2 %>% 
               dplyr::select(location_id,
                      date,
                      edd_hot,
                      edd_cold,
                      edd_total
                      ), by = c("location_id")) %>% 
  mutate(year = year(date),
         yday = yday(date),
         edd_hot = rescale(edd_hot, to=c(0,1)),
         edd_cold = rescale(edd_cold, to=c(0,1)),
         edd_total = rescale(edd_total, to=c(0,1)),
         edd_reverse = rescale(edd_total, to=c(1,0))
  ) %>% 
  filter(yday != 366) %>% 
  rename(`Hot EDD` = edd_hot,
         `Cold EDD` = edd_cold,
         `Total EDD` = edd_total) %>% 
  pivot_longer(cols = c(`Hot EDD`, `Cold EDD`, `Total EDD`)) %>% 
  mutate(name = factor(name, levels = c("Hot EDD", "Cold EDD", "Total EDD")))


p <- ggplot(data3) +
  geom_tile(aes(x=yday, y=year, alpha = value, fill=name)) + 
  facet_grid(label~name) +
  labs(title = paste("Top", n, scope, "cities with", dir, "pleasant days in a year", sep = " "),
       caption = caption,
       subtitle = sub) +
  expand_limits(y = min(years)-length(years)) +
  coord_polar() +
  scale_fill_manual(values = colors2) +
  theme(#axis.text.x = element_text(size = rel(1-ncol/50)),
    strip.text = element_text(face = "bold")
  )

ggsave(file, 
       p,
       width = width, 
       height=height,
       units = "in",
       limitsize = FALSE)
