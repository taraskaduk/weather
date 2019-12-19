library(rvest)
library(tidyverse)

url_test <- "https://www.wunderground.com/history/monthly/us/pa/philadelphia/KPHL/date/2017-8"
url_test2 <- "https://www.wunderground.com/history/"
url <- url_test

read_html(url) %>% html_nodes("days") %>% html_nodes("td")

df <- read_html(url) %>% 
  html_node("#inner-content .city-body .row.city-history-observation") %>% 
  html_attrs()


