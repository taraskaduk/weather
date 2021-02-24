get_weather <- function(yrs = seq(year(today()) - 10, year(today())-1),
                        stns = stations_v) {
  for (yr in yrs) {
    file <- paste0(yr, '.tar.gz')
    destfile <- paste0('data/gsod/', file)
    if (!file.exists(destfile)) {
      link <- paste0('https://www.ncei.noaa.gov/data/global-summary-of-the-day/archive/', file)
      curl::curl_download(link, destfile)
    }
    untar(destfile, exdir = paste(tempdir(), yr, sep = "/"))
  }
  
  # Go through all unpacked files, decide what to remove and what to keep
  # based on the stations of interest
  files_all <- list.files(path = tempdir(), 
                          pattern = "^.*\\.csv$", 
                          recursive = TRUE)
  
  # Get a cartesian join of all stations of interest and all years.
  files_stations <- 
    purrr::cross(list(x1 = paste0(yrs, "/"), x2 = paste0(stns, ".csv"))) %>%
    purrr::map(purrr::lift(paste0)) %>%
    as_vector()
  
  files_keep <- subset(files_all, files_all %in% files_stations)
  # I don't think files_remove is necessary anymore,since reformat_GSOD picks up only files that are in files_keep.
  #files_remove <- subset(files_all, !(files_all %in% files_stations))
  # file.remove(files_remove, force = TRUE, recursive = TRUE)
  
  # Transform weather data ----------------------------------------------------
  #out <- reformat_GSOD(dsn = "data/gsod/2010")
  out <- reformat_GSOD(file_list = paste(tempdir(), files_keep, sep = "/")) %>% 
    rename_all(tolower) %>% 
    rename(lat = latitude,
           lon = longitude)
  #unlink(tempdir(), force = TRUE, recursive = TRUE)
  out
}

# get_isd_history <-function(...) {
#     curl::curl_download('ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt',
#                         destfile = "data/isd-history.txt")
# }

colnames_tolower <- function(data) {
  colnames(data) <-  tolower(colnames(data))
  out <- data
}

get_cities <- function() {
  curl::curl_download(
    "https://simplemaps.com/static/data/world-cities/basic/simplemaps_worldcities_basicv1.73.zip",
    destfile = "data/cities.zip"
  )
  unzip_cities <-  unzip("data/cities.zip", exdir = "data/")
  unlink("data/cities.zip")
  out <- read_csv("data/worldcities.csv") %>%
    rename(lon = lng)
}


feels_like <- function(temp, rh, wind) {
  hi <-  if_else(is.na(rh),
                 temp, 
                 heat.index(t = temp, 
                            rh = rh, 
                            temperature.metric = "celsius", 
                            output.metric = "celsius", 
                            round = 2))
  temp_f <- celsius.to.fahrenheit(temp)
  wc_f = (35.74 + 0.6215*temp_f) - 35.75*(wind^0.16) + 0.4275 * temp_f * (wind^0.16)
  wc <- if_else(is.na(wind), temp, fahrenheit.to.celsius(wc_f))
  new_temp <-  case_when(temp < 10 & wind > 3 ~ wc,
                        temp > 27 ~ hi,
                        TRUE ~ temp) %>% 
    
  return(new_temp)
}


# AUC of cosine function --------------------------------------------------

# get_auc <- function(min, max, perfect = 18) {
#   a <- (max-min)/2 #amplitude
#   period <- 24
#   b <- 2 * pi / period
#   d <- min + a
#   temperature <- function(x) {
#     -a * cos(b * x) + d
#   }
#   
#   if (min >= perfect) {
#     # integral <- -a*sin(24*b) + 24*d - 24*perfect
#     integral <- integrate(temperature, 0, 24)$value - perfect * 24 %>% 
#       round(2)
#     area <- tibble(auc_hot = integral,  
#                    auc_cold = 0, 
#                    auc_total = integral)
#     
#   } else if (max <= perfect) {
#     integral <- perfect * 24 - integrate(temperature, 0, 24)$value %>% 
#       round(2)
#     
#     area <- tibble(auc_hot = 0,  
#                    auc_cold = integral, 
#                    auc_total = integral)
#     
#   } else {
#     intercept1 <- acos((d - perfect) / a) / b
#     intercept2 <- (12 - intercept1) * 2 + intercept1
#     
#     integral1 <-
#       perfect * intercept1 - integrate(temperature, 0, intercept1)$value
#     
#     integral2 <-
#       integrate(temperature, intercept1, intercept2)$value - perfect * (intercept2 - intercept1) 
#     
#     integral3 <-
#       perfect * (24 - intercept2) - integrate(temperature, intercept2, 24)$value 
#     
#     area <- tibble(auc_hot = round(integral2,2),  
#                    auc_cold = round(integral1 + integral3,2), 
#                    auc_total = round(integral1 + integral2 + integral3,2))
#   }
#   return(area)
# }


get_edd <- function(min, max, baseline = 18) {
  
  # First, create the temp. function:
  a <- (max-min)/2 #amplitude
  period <- 24
  b <- 2 * pi / period
  d <- min + a
  
  # This is our temperature function:
  temperature <- function(x) {
    -a * cos(b * x) + d
  }
  
  
  # 3 calculations based on the 3 scenarios
  # of how the curve and the baseline interact
  
  if (min >= baseline) {
    # integral <- -a*sin(24*b) + 24*d - 24*baseline
    integral <- integrate(temperature, 0, 24)$value - baseline * 24 %>% 
      round(2)
    edd <- tibble( edd_hot = round(integral/24,2),  
                   edd_cold = 0, 
                   edd_total = round(integral/24,2))
    
  } else if (max <= baseline) {
    integral <- baseline * 24 - integrate(temperature, 0, 24)$value %>% 
      round(2)
    
    edd <- tibble( edd_hot = 0,  
                   edd_cold = round(integral/24,2), 
                   edd_total = round(integral/24,2))
    
  } else {
    intercept1 <- acos((d - baseline) / a) / b
    intercept2 <- (12 - intercept1) * 2 + intercept1
    
    integral1 <-
      baseline * intercept1 - integrate(temperature, 0, intercept1)$value
    
    integral2 <-
      integrate(temperature, intercept1, intercept2)$value - baseline * (intercept2 - intercept1) 
    
    integral3 <-
      baseline * (24 - intercept2) - integrate(temperature, intercept2, 24)$value 
    
    edd <- tibble(edd_hot = round(integral2/24,2),  
                  edd_cold = round((integral1 + integral3)/24,2), 
                  edd_total = round((integral1 + integral2 + integral3)/24,2))
  }
  return(edd)
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




