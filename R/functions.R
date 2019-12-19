# get_weather_old <- function(yrs = year(today()),
#                         #yrs = seq(year(today()) - 5, year(today())), 
#                         stns = stations_v) {
#   for (yr in yrs) {
#     file <- paste0('gsod_', yr)
#     destfile <- paste0('data/gsod/', file, '.tar')
#     if(file.exists(destfile) #& yr != year(today())
#     ) next
#     ftp <-
#       paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',
#              yr,
#              '/',
#              file,
#              '.tar')
#     curl::curl_download(ftp, destfile)
#   }
#   
#   # Unpack weather data --------------------------------------
#   ## Unpack downloaded yearly archives 
#   to_untar <- list.files("data/gsod", full.names = TRUE)
#   purrr::map(to_untar, untar, exdir = tempdir())
#   
#   ## Go through all unpacked files, decide what to remove and what to keep
#   ## based on the stations of interest
#   files_all <- list.files(path = tempdir(), pattern = "([0-9]+)-([0-9]+)-([0-9]+).op.gz")
#   files_stations <- purrr::cross(list(x1 = paste0(stns, "-"), x2 = paste0(yrs, ".op.gz"))) %>%
#     purrr::map(purrr::lift(paste0)) %>% 
#     as_vector()
#   
#   files_keep <- subset(files_all, files_all %in% files_stations)
#   files_remove <- subset(files_all, !(files_all %in% files_stations))
#   file.remove(paste(tempdir(),files_remove, sep = "/"))
#   
#   # Transform weather data ----------------------------------------------------
#   out <- GSODR::reformat_GSOD(dsn = tempdir())
#   unlink(tempdir(), force = TRUE, recursive = TRUE)
#   out
# }




get_weather <- function(yrs = seq(year(today()) - 11, year(today())),
                        #yrs = seq(year(today()) - 5, year(today())), 
                        stns = stations_v) {
  for (yr in yrs) {
    file <- paste0(yr, '.tar.gz')
    destfile <- paste0('data/gsod/', file)
    if (!file.exists(destfile)) {
      link <- paste0('https://www.ncei.noaa.gov/data/global-summary-of-the-day/archive/',file)
      curl::curl_download(link, destfile)
    }
    untar(destfile, exdir = paste(tempdir(), yr, sep = "/"))
    # file.rename(from = list.files(tempdir(), full.names = TRUE, pattern = "([0-9])+.csv"),
    #              to = paste0(tempdir(), "/", yr, "-", list.files(tempdir(), pattern = "([0-9])+.csv")))
  }
  
  ## Go through all unpacked files, decide what to remove and what to keep
  ## based on the stations of interest
  files_all <- list.files(path = tempdir(), pattern = "^.*\\.csv$", recursive = TRUE, full.names = FALSE)
  # files_stations <- paste0(stns, ".csv")
  files_stations <- purrr::cross(list(x1 = paste0(yrs, "/"), x2 = paste0(stns, ".csv"))) %>%
    purrr::map(purrr::lift(paste0)) %>%
    as_vector()
  
  files_keep <- subset(files_all, files_all %in% files_stations)
  files_remove <- subset(files_all, !(files_all %in% files_stations))
  file.remove(files_remove, recursive = TRUE)
  
  # Transform weather data ----------------------------------------------------
  out <- GSODR::reformat_GSOD(file_list = paste(tempdir(),files_keep, sep="/"))
  unlink(tempdir(), force = TRUE, recursive = TRUE)
  out
}

# get_isd_history <-function(...) {
#     curl::curl_download('ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt',
#                         destfile = "data/isd-history.txt")
# }

colnames_tolower <- function(data){
  colnames(data) <-  tolower(colnames(data))
  out <- data
  }

get_cities <- function() {
  curl::curl_download("https://simplemaps.com/static/data/world-cities/basic/simplemaps_worldcities_basicv1.4.zip",
                                      destfile = "data/cities.zip")
  unzip_cities <-  unzip("data/cities.zip", exdir = "data/")
  unlink("data/cities.zip")
  out <- read_csv("data/worldcities.csv") %>%
    rename(lon = lng)
}


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


# AUC of cosine function --------------------------------------------------

get_auc <- function(min, max, perfect = 18) {
  a <- (max-min)/2 #amplitude
  period <- 24
  b <- 2 * pi / period
  d <- min + a
  temperature <- function(x) {
    -a * cos(b * x) + d
  }
  
  if (min >= perfect) {
    # integral <- -a*sin(24*b) + 24*d - 24*perfect
    integral <- integrate(temperature, 0, 24)$value - perfect * 24 %>% 
      round(2)
    area <- tibble(auc_hot = integral,  
                   auc_cold = 0, 
                   auc_total = integral)
    
  } else if (max <= perfect) {
    integral <- perfect * 24 - integrate(temperature, 0, 24)$value %>% 
      round(2)
    
    area <- tibble(auc_hot = 0,  
                   auc_cold = integral, 
                   auc_total = integral)
    
  } else {
    intercept1 <- acos((d - perfect) / a) / b
    intercept2 <- (12 - intercept1) * 2 + intercept1
    
    integral1 <-
      perfect * intercept1 - integrate(temperature, 0, intercept1)$value
    
    integral2 <-
      integrate(temperature, intercept1, intercept2)$value - perfect * (intercept2 - intercept1) 
    
    integral3 <-
      perfect * (24 - intercept2) - integrate(temperature, intercept2, 24)$value 
    
    area <- tibble(auc_hot = round(integral2,2),  
                   auc_cold = round(integral1 + integral3,2), 
                   auc_total = round(integral1 + integral2 + integral3,2))
  }
  return(area)
}




f_lm <- function(df){
  lm(value ~ yday + year, data = df)
}

f_knnreg <- function(df){
  knnreg(value ~ yday + year, data = df, k = 4)
}

null_geometry <- function(df) {
  st_geometry(df) <- NULL
}



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
