library(lubridate)
library(scales)
library(ggnewscale)
library(tidyverse)

source(Sys.getenv("theme_url"))
theme_set(theme_tk()+
            theme(rect = element_blank(),
                  panel.border = element_blank(),
                  strip.background = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title=element_blank(),
                  axis.text = element_blank(),
                  legend.direction = "horizontal", 
                  legend.box = "horizontal",
                  plot.title = element_text(family="Oswald", 
                                            size = rel(2.5)),
                  plot.subtitle = element_text(family="Oswald",
                                               size = rel(1.5))))

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

caption <-  ("\n\nSources: NCEI Global Summary of the Day, simplemaps.com \nAuthor: @taraskaduk | taraskaduk.com")
colors <-  c(pleasant = "#1a9641", 
             hot = "#d6604d", 
             cold = "#4393c3", 
             elements = "#bebada",
             # wind = '#e6e6e6',
             `hot & elements` =  "#B8001F",
             `cold & elements` = "#05669E")

col_hot <- "#B8001F"
col_hot_low <- "#FFBEC9"
col_cold <- "#0067A3"
col_cold_low <- "#BAE6FF"

colors2 <- c(`Hot EDD`  = col_hot, 
             `Cold EDD` = col_cold)

plot_data <- function(df = summary_locations, 
                      df2 = data_daily, 
                      pop = 1000000, 
                      n = 50, 
                      dir = "most", 
                      scope = "world",
                      plot = "both",
                      output = "both",
                      years = years, 
                      ncol = 5,
                      width = 10,
                      height = 16) 
{
  # title_size = rel(4*ncol/10)
  # subtitle_size = rel(3*ncol/10)
  # facet_size = rel(0.2 + 5/ncol)
  # caption_size = rel(2*ncol/10)

  if (scope != "world") {
    data1 <- df %>%
      filter(country == scope) %>%
      mutate(name = paste(city, admin_name, sep = ",\n"))
  } else {
    data1 <- df %>%
      mutate(name = paste(city, country, sep = ",\n"))
  }
  
  file <- paste0("plots/", paste(n, dir, scope, pop / 1000, ncol, sep = "_"))
  
  # Pleasant ----------------------------------------------------------------
  
  plot_pleasant <- function() {
    if (nrow(filter(data1, population > pop)) < n |
        pop == 0) {
      data2 <- data1 %>%
        arrange(desc(population)) %>%
        head(n) %>%
        arrange(desc(pleasant))
    } else {
      data2 <- data1 %>%
        filter(population > pop) %>%
        arrange(desc(pleasant))
    }
    
    
    if (dir == "most") {
      data3 <- head(data2, n) %>%
        mutate(
          rank = row_number(desc(pleasant)),
          label = paste0(rank, ". ", name),
          label = reorder(label, rank)
        )
    } else {
      data3 <- data2 %>%
        tail(n) %>%
        mutate(
          rank = row_number(pleasant),
          label = paste0(rank, ". ", name),
          label = reorder(label, rank)
        )
    }
    
    
    if (pop == 0) {
      title <- paste(n,
                     "largest",
                     scope,
                     "cities ranked by",
                     "amount of pleasant days in a year",
                     sep = " ")
      sub <- paste0("Years ", min(years), " - ", max(years),
                    "\n")
    } else{
      title <- paste(n,
                     scope,
                     "cities with",
                     dir,
                     "amount of pleasant days in a year",
                     sep = " ")
      sub <-
        paste0(
          "Cities with population over ",
          comma(pop),
          " people.",
          "\nYears ",
          min(years),
          " - ",
          max(years),
          "\n"
        )
    }
    
    data4 <- data3 %>%
      rename(
        total_pleasant = pleasant,
        total_hot = hot,
        total_cold = cold,
        total_elements = elements,
        # total_wind = wind,
        total_unknown = unknown
      ) %>%
      inner_join(df2, by = c("location_id")) %>%
      mutate(year = year(date),
             yday = yday(date)) %>%
      filter(yday != 366)
    
    
    p1 <- ggplot(data4) +
      geom_tile(
        aes(x = yday, y = year, fill = double_class),
        col = NA,
        size = 0.1,
        width = 0.99,
        height = 0.99
      ) +
      facet_wrap( ~ label, ncol = ncol) +
      scale_fill_manual(values = colors,
                        name = "Distinct classification",
                        aesthetics = c("fill")) +
      labs(title = title,
           caption = caption,
           subtitle = sub) +
      expand_limits(y = min(years) - length(years)) +
      coord_polar() +
      theme(
        #axis.text.x = element_text(size = rel(1-ncol/50)),
        strip.text = element_text(face = "bold")
      )
    
    if (output %in% c("png", "both")) {
      ggsave(
        paste0(file, "_pleasant.png"),
        p1,
        device= "png",
        width = (ncol * 600)/300,
        height = 1.5 * (n/ncol * 400 + 100)/300,
        units = "in",
        dpi = 300,
        limitsize = FALSE
      )
    }
    if (output %in% c("svg", "both")) {
      ggsave(
        paste0(file, "_pleasant.svg"),
        p1,
        width = width,
        height = height,
        units = "in",
        limitsize = FALSE
      )
    }
  }
  
  
  # EDD ---------------------------------------------------------------------
  plot_edd <- function() {
    dir2 <- if_else(dir == "most", "lowest", "highest")
    
    if (pop == 0) {
      title <-
        paste(n,
              "largest",
              scope,
              "cities ranked by total Excess Degree-Days",
              sep = " ")
      sub <- paste0("Years ", min(years), " - ", max(years),
                    "\n")
    } else{
      title <-
        paste(n,
              scope,
              "cities with",
              dir2,
              "total Excess Degree-Days",
              sep = " ")
      sub <-
        paste0(
          "Cities with population over ",
          comma(pop),
          " people.",
          "\nYears ",
          min(years),
          " - ",
          max(years),
          "\n"
        )
    }
    
    if (nrow(filter(data1, population > pop)) < n | pop == 0) {
      data2 <- data1 %>%
        arrange(desc(population)) %>%
        head(n) %>%
        arrange(edd_total)
    } else {
      data2 <- data1 %>%
        filter(population > pop) %>%
        arrange(edd_total)
    }
    
    
    if (dir == "most") {
      data3 <- head(data2, n) %>%
        mutate(
          rank = row_number(edd_total),
          label = paste0(rank, ". ", name),
          label = reorder(label, rank)
        )
    } else {
      data3 <- data2 %>%
        tail(n) %>%
        mutate(
          rank = row_number(desc(edd_total)),
          label = paste0(rank, ". ", name),
          label = reorder(label, rank)
        )
    }
    
    data4 <- data3 %>%
      select(
        location_id,
        label,
        avg_edd_hot = edd_hot,
        avg_edd_cold = edd_cold,
        avg_edd_total = edd_total
      ) %>%
      inner_join(df2 %>%
                   select(location_id,
                          date,
                          edd_hot,
                          edd_cold,
                          edd_total),
                 by = c("location_id")) %>%
      mutate(year = year(date),
             yday = yday(date)) %>%
      filter(yday != 366)
    
    
    p2 <- ggplot(data4) +
      geom_tile(
        aes(
          x = yday,
          y = year,
          fill = edd_hot,
          alpha = edd_hot / edd_total
        ),
        col = "grey95",
        size = 0.01
      ) +
      scale_fill_gradient(name = "Hot Degree-Days",
                          #trans="sqrt",
                          low = "grey98", high = col_hot) +
      
      new_scale_fill() +
      geom_tile(
        aes(
          x = yday,
          y = year,
          fill = edd_cold,
          alpha = edd_cold / edd_total
        ),
        col = "grey95",
        size = 0.01
      ) +
      scale_fill_gradient(name = "Cold Degree-Days",
                          #trans="sqrt",
                          low = "grey98", high = col_cold) +
      scale_alpha(guide = 'none') +
      facet_wrap( ~ label, ncol = ncol) +
      labs(title = title,
           caption = caption,
           subtitle = sub) +
      expand_limits(y = min(years) - length(years)) +
      coord_polar() +
      
      theme(
        #axis.text.x = element_text(size = rel(1-ncol/50)),
        # legend.position = "none",
        strip.text = element_text(face = "bold", size = facet_size),
        plot.title = element_text(size = title_size,
                                  family = "Oswald"),
        plot.subtitle = element_text(size = subtitle_size,
                                     family = "Oswald"),
        plot.caption = element_text(size = caption_size)
      )
    
    if (output %in% c("png", "both")) {
      ggsave(
        paste0(file, "_edd.png"),
        p2,
        device = agg_png,
        width = width,
        height = height,
        scaling = 2,
        units = "in",
        limitsize = FALSE
      )
    }
    # ggsave(paste0(file,"_edd.eps"),
    #        p2,
    #        width = width,
    #        height=height,
    #        units = "in",
    #        limitsize = FALSE)
    
    if (output %in% c("svg", "both")) {
      ggsave(
        paste0(file, "_edd.svg"),
        p2,
        width = width,
        height = height,
        units = "in",
        limitsize = FALSE
      )
    }
  }
  
  if (plot %in% c("pleasant", "both")) {
    plot_pleasant()
  }
  if (plot %in% c("edd", "both")) {
    plot_edd()
  }
  
  
}

# test
plot_data(
  df = summary_locations,
  df2 = data_daily,
  pop = 1000000,
  n = 50,
  dir = "most", 
  scope = "United States",
  plot="pleasant",
  output="png",
  years = years,
  # width = 20,
  # height = 56,
  ncol = 5
)
plot_data(
  df = summary_locations,
  df2 = data_daily,
  pop = 1000000,
  n = 50,
  dir = "most", 
  scope = "United States",
  plot="pleasant",
  output="png",
  years = years,
  ncol = 10,
  width = 20,
  height = 20
)




plot_data(
  df = summary_locations,
  df2 = data_daily,
  pop = c(0,1000000),
  n = 50,
  dir = c("most", "least"), 
  scope = c("world", "United States"),
  plot="both",
  output="png",
  years = years,
  ncol = 10,
  width = 20,
  height = 20
)

plot_data(df = summary_locations, 
          df2 = data_daily, 
          pop = c(0,1000000), 
          n = 50, 
          output = "png",
          dir = c("most", "least"), 
          scope = c("world", "United States"),
          years = years,
          ncol = 5,
          width = 10,
          height = 28)

plot_data(df = summary_locations, 
          df2 = data_daily, 
          pop = c(0,1000000), 
          n = 50, 
          plot = "both",
          output = "svg",
          dir = c("most", "least"), 
          scope = c("world", "United States"),
          years = years,
          ncol = 10,
          width = 10,
          height = 10)