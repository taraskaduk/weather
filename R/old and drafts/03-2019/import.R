library(tidyverse)
library(lubridate)

# Stations ----------------------------------------------------------------
curl::curl_download('ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt', destfile = "data/isd-history.txt")


# NOAA GSOD data ----------------------------------------------------------

years <- seq(year(today()) - 5, year(today()))
future::plan("multisession")
for (year in years) {
  file <- paste0('gsod_',year)
  ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year,'/',file,'.tar')
  destfile <- paste0('data/gsod/',file,'.op.gz')
  curl::curl_download(ftp, destfile)
}

## Current year only
file <- paste0('gsod_',year(today()))
ftp <- paste0('ftp://ftp.ncdc.noaa.gov/pub/data/gsod/',year(today()),'/',file,'.tar')
destfile <- paste0('data/gsod/',file,'.op.gz')
curl::curl_download(ftp, destfile)

# Cities ------------------------------------------------------------

curl::curl_download(
  "https://simplemaps.com/static/data/world-cities/basic/simplemaps_worldcities_basicv1.4.zip",
  destfile = "data/cities.zip"
)
Sys.sleep(3)
unzip("data/cities.zip", exdir = "data/")



# Old links, just in case -------------------------------------------------

# https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv
# https://gist.githubusercontent.com/Miserlou/c5cd8364bf9b2420bb29/raw/2bf258763cdddd704f8ffd3ea9a3e81d25e2c6f6/cities.json'
# http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2015_Gazetteer/2015_Gaz_cbsa_national.zip
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=PEP_2017_PEPANNRES&prodType=table
# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
# cities <- maps::world.cities %>% arrange(desc(pop))
