library(tidyverse)

# Get cities 1 ------------------------------------------------------------

# cities <- maps::world.cities %>%
#   arrange(desc(pop))

# Or Get cities 2 ---------------------------------------------------------

# U.S. only cities here:'https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv'

curl::curl_download(
  "https://simplemaps.com/static/data/world-cities/basic/simplemaps_worldcities_basicv1.4.zip",
  destfile = "data/cities.zip"
)
Sys.sleep(3)
unzip("data/cities.zip", exdir = "data/")