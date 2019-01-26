setwd("weather")

library(tidyverse)
library(sf)
library(tigris)
library(readxl)


path_raw <- "data/0-raw/"



# CBSAs and CSAs ----------------------------------------------------------

locs_cbsa <- core_based_statistical_areas() %>% 
  st_as_sf()
colnames(locs_cbsa) <- tolower(colnames(locs_cbsa))

# locs_csa <- combined_statistical_areas() %>% 
#   st_as_sf()
# colnames(locs_csa) <- tolower(colnames(locs_csa))



# Population --------------------------------------------------------------

path_pop <- paste0(path_raw,"PEP_2017_PEPANNRES_with_ann.csv")
pop <- read_csv(path_pop, 
                          col_types = cols(GEO.id2 = col_character()),
                          locale = locale(encoding = "LATIN1", 
                                          asciify = TRUE),
                          trim_ws = TRUE) %>% 
  filter(GEO.id != "Id") %>% 
  select(geoid = GEO.id2,
         pop10 = rescen42010,
         pop17 = respop72017)



locations_import <- locs_cbsa %>% 
  left_join(pop, by = "geoid") %>% 
  mutate(name_short = str_sub(name, start = 1, end = if_else(is.na(str_locate(name, "-")[,1]), str_locate(name, ",")[,1], str_locate(name, "-")[,1]) -1))




## Principal cities --------------------------------------------------------
# 
# cbsa_principal_cities <- read_excel(paste0(path_raw,"cbsa-principal-cities.xls"), 
#                                     col_types = c("text", "text", "text", 
#                                                   "text", "text", "text",
#                                                   "skip", "skip", "skip",
#                                                   "skip"),
#                                     col_names = c("cbsafp", "cbsa_title", "cbsa_flag", "city", "fips_st", "fips_place"),
#                                     skip = 3
#                                     )
# 
# cbsa_join <- cbsa_principal_cities %>% 
#   select(cbsafp, city) %>% 
#   distinct()
# 
# 
# locs_cbsa <- locs_cbsa %>% 
#   left_join(cbsa_join, by = "cbsafp")



locations <- loctions_import

# 1.4 Save necessary data -----------------------------------------------------
saveRDS(locations_import, file = "data/1-locations.RDS")

