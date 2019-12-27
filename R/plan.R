
plan <- drake_plan(
  years = seq(year(today()) - 11, year(today())),
  

  
  # Cities -----------------------------------
  cities = get_cities(),
  
  # CBSAs and CSAs ----------------------------------------------------------
  
  us_cbsas = core_based_statistical_areas() %>% 
    st_as_sf() %>% 
    rename_all(tolower) %>% 
    mutate(name_short = str_sub(name, start = 1, end = if_else(is.na(str_locate(name, "-")[,1]), str_locate(name, ",")[,1], str_locate(name, "-")[,1]) -1)),
  
  us_csas = combined_statistical_areas() %>%
    st_as_sf() %>% 
    rename_all(tolower) %>% 
    mutate(name_short = str_sub(name, start = 1, end = if_else(is.na(str_locate(name, "-")[,1]), str_locate(name, ",")[,1], str_locate(name, "-")[,1]) -1)),
  
  
  cities_sf = cities %>% 
    select(id, lat, lon) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4269),
  
  
  cbsa_lookup = us_cbsas %>% 
    select(cbsafp, cbsa_name = name, cbsa_name_short = name_short),
  
  csa_lookup = us_csas %>% 
    select(csafp, csa_name = name, csa_name_short = name_short),
  
  ## Find what stations match to what locations: exactly and also with a wider net.
  city_to_csa = st_join(cities_sf, csa_lookup),
  
  city_to_cbsa = st_join(city_to_csa, cbsa_lookup) %>% 
    filter(!is.na(csa_name) | !is.na(cbsa_name)),
  
  city_to_cbsa2 = null_geometry(city_to_cbsa),
  
  
  location_lookup = cities %>% 
    left_join(city_to_cbsa, by = 'id') %>% 
    mutate(type = case_when(!is.na(csafp) ~ "csa",
                            !is.na(cbsafp) ~ "cbsa",
                            TRUE ~ "city"),
           location_id = case_when(!is.na(csafp) ~ paste0("csa",csafp),
                                   !is.na(cbsafp) ~ paste0("cbsa",cbsafp),
                                   TRUE ~ paste0("city", id)),
           name = case_when(!is.na(csa_name) ~ csa_name,
                            !is.na(cbsa_name) ~ cbsa_name,
                            TRUE ~ city),
           name_short = case_when(!is.na(csa_name_short) ~ csa_name_short,
                                  !is.na(cbsa_name_short) ~ cbsa_name_short,
                                  TRUE ~ city)),
  
  parent_location_lookup = location_lookup %>% 
    select(type, country, location_id, name, name_short, population, lat, lon) %>% 
    group_by(type, country, location_id, name, name_short) %>% 
    summarise(population = sum(population),
              lat = mean(lat),
              lon = mean(lon)) %>% 
    ungroup(),
  
  

  locations = parent_location_lookup %>%
    # filter(id %in% c('1840015031', "1152554349")) %>% ##TEST PURPOSES ONLY!!!!!!!!!!!
    ###
    filter(population >= 500000),
  
  # # Stations  ----------------------------------------------------
  # zz_locations_stations = locations %>% 
  #   mutate(stnid = purrr::map2(lat, lon, nearest_stations, distance = 50)) %>% 
  #   unnest(),
  # # Get unique stations
  # zz_stations = locations_stations %>% 
  #   select(stnid) %>% 
  #   mutate(stnid = str_remove_all(stnid, "-")) %>% 
  #   distinct(),
  # stations_v = as_vector(stations),
  
  # Stations  ----------------------------------------------------
  stations_df = locations %>% 
    mutate(st = purrr::map2(lat, lon, isd_stations_search, radius = 50)) %>% 
    select(location_id, st) %>% 
    unnest(cols = c(st)) %>% 
    mutate(stnid = paste(usaf, wban, sep = "-"),
           stnid2 = paste0(usaf, wban)),
  
  locations_stations = locations %>% 
    inner_join(stations_df %>% select(location_id, stnid, stnid2, distance), by="location_id"),
  
  # Get unique stations
  stations = locations_stations %>%
    select(stnid2) %>%
    distinct(),
  stations_v = as_vector(stations),
  
  # Weather -------------------------------------
  # weather_import = get_GSOD(years = years),
  weather_import = get_weather(yrs = years, stns = stations_v),
  lower_colnames = colnames_tolower(weather_import),
  weather = lower_colnames %>%
    # New data return??? WTF, need to rename lat and lon
    rename(lat = latitude,
           lon = longitude) %>% 
    filter(!is.na(lat) & !is.na(lon)) %>% 
    # Get rid of stations on water: oceans and lakes
    mutate(
      country = maps::map.where('world', lon, lat),
      lakes = maps::map.where('lakes', lon, lat),
      yday = yday(yearmoda)
    ) %>%
    filter(!is.na(country) & is.na(lakes)) %>%
    select(-c(lakes,  country)), 
  
  # Join city and weather data ---------
  
  data = locations_stations %>%
    select(location_id, stnid, distance) %>% 
    inner_join(weather, by = "stnid") %>% 
    select(location_id, date = yearmoda,
           temp_max = max,
           temp_min = min, 
           temp_mean = temp, 
           dewp, slp, 
           stp, 
           visib, 
           wdsp, 
           mxspd, 
           gust, 
           prcp, 
           sndp, 
           i_fog:rh,
           distance) %>% 
    # filter(location_id == "cbsa12420" & date == "2008-01-01") %>% 
    # head(100) %>% 
    group_by(location_id, date) %>% 
    summarise_at(vars(temp_max:rh),
                 funs(weighted.mean(., w = 1/distance, na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate_all( ~ case_when(!is.nan(.x) ~ .x)) %>% 
    # A few possible substitutions
    mutate(temp_max = if_else(is.na(temp_max) & !is.na(temp_min) & !is.na(temp_mean), 2*temp_mean - temp_min, temp_max),
           temp_min = if_else(is.na(temp_min) & !is.na(temp_max) & !is.na(temp_mean), 2*temp_mean - temp_max, temp_min)),
  


# Predict missing values --------------------------------------------------
max_date = max(data$date),
min_date = min(data$date),
dummy = data %>%
  select(location_id) %>%
  distinct() %>%
  merge(tibble(date = seq.Date(min_date, max_date, by = "day")), all = TRUE),

full_data = dummy %>%
  left_join(data, by = c("location_id", "date")),


data_check = full_data %>%
  mutate(year = year(date)) %>% 
  group_by(location_id, year) %>%
  summarise(total = n(),
            exist = sum(!is.na(temp_min))) %>% 
  ungroup() %>%
  mutate(ratio = exist / total) %>% 
  group_by(location_id) %>%
  summarise(avg_ratio = mean(ratio)) %>%
  ungroup() %>%
  filter(avg_ratio >= 0.85) %>% 
  select(location_id),

full_data_filtered = full_data %>% 
  semi_join(data_check, by = "location_id"),
locations_filtered = locations %>% 
  semi_join(data_check, by = "location_id"),
data_filtered = data %>% 
  semi_join(data_check, by = "location_id"),


data_completed = full_data_filtered %>%
    #GATHER
    select(location_id, date, temp_min, temp_max, temp_mean, rh, wdsp) %>%
    gather(metric, value, -c(location_id, date)) %>%
    mutate(flag = if_else(is.na(value), "missing", "train"),
           year = year(date),
           yday = yday(date)) %>% 
    #NEST
    group_by(location_id, metric, flag) %>%
    nest() %>%
    pivot_wider(names_from = flag, values_from = data) %>%
    filter(!is.na(missing) & !is.na(train)) %>% 
    #MODEL
    mutate(knn = train %>% purrr::map(f_knnreg)) %>%
    mutate(pred_knn = map2(.x = knn, .y = missing, .f = predict)) %>% 
    #UNNEST
    select(-c(train, knn)) %>%
    unnest(c(missing, pred_knn)) %>%
    mutate(value = pred_knn,
           flag = "predicted") %>%
    select(location_id, date, metric, value, flag, yday, year) %>% 
    #BIND
    select(-flag) %>%
    spread(metric, value) %>%
    bind_rows(data_filtered),

data_collapsed = data_completed %>% 
  group_by(location_id, date, year, yday) %>% 
  summarise_all(max, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate_at(vars(rh:es), ~if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)) %>% 
  # replace_na(list(sndp = 0, prcp = 0, i_rain_drizzle = 0, i_snow_ice = 0, wdsp = 0, gust = 0)) %>%
  mutate_at(vars(rh:es), round, digits = 2) %>% 
  mutate(temp_mean_feel = feels_like(temp_mean, rh, wdsp),
         temp_min_feel = feels_like(temp_min, rh, wdsp),
         temp_max_feel = feels_like(temp_max, rh, wdsp)),

save_data = write_csv(data_collapsed, "data/data.csv"),
save_locations = write_csv(locations_filtered, "data/locations.csv"),
save_cities = write_csv(location_lookup, "data/location_lookup.csv")

# data_daily = data_collapsed %>% 
#   mutate(hot = if_else(temp_min > params$temp_min[2] |
#                          temp_max >  params$temp_max[2] |
#                          temp_mean > params$temp_mean[2], 1, 0),
#          cold = if_else(temp_min <  params$temp_min[1] |
#                           temp_max <  params$temp_max[1]  |
#                           temp_mean < params$temp_mean[1], 1, 0),
#          # auc = map2_dbl(temp_min, temp_max, get_auc),
#          elements = if_else(prcp > params$prcp |
#                               sndp > params$sndp |
#                               i_rain_drizzle > 0.66 |
#                               i_snow_ice > 0.66, 1, 0),
#          # wind = if_else(wdsp > 10, 1, 0),
#          pleasant = if_else(hot + cold + elements  == 0, 1, 0),
#          distinct_class = case_when(pleasant == 1 ~ "pleasant",
#                                     hot == 1 ~ "hot",
#                                     cold == 1 ~ "cold", 
#                                     elements == 1 ~ "elements",
#                                     # wind == 1 ~ "wind",
#                                     TRUE  ~ NA_character_),
#          double_class =   case_when(pleasant == 1 ~ "pleasant",
#                                     hot == 1 & elements == 1 ~ "hot & elements",
#                                     cold == 1 & elements == 1 ~ "cold & elements",
#                                     hot == 1 ~ "hot",
#                                     cold == 1 ~ "cold", 
#                                     elements == 1 ~ "elements",
#                                     # wind == 1 ~ "wind",
#                                     TRUE ~ NA_character_),
#          double_class = factor(double_class, levels = c("pleasant", "elements", "cold", "cold & elements", "hot", "hot & elements"))
#   ),
# 
# 
# # summary_locations_auc = data_daily %>%
# #   filter(year < year(today())) %>% 
# #   group_by(city, country, lat, lon, capital, population, year) %>% 
# #   summarise(auc = mean(auc),
# #             days = n()) %>% 
# #   ungroup() %>% 
# #   ## This if_else accounts for cases of leap year with all known days.
# #   ## It makes sure we don't have negative unknown days
# #   ## But also levels out leap year for the next step of averaging
# #   mutate(unknown = if_else(days >= 365, 0, 365 - days)) %>% 
# #   filter(unknown < 365 * 0.1) %>% 
# #   #filter(country == "United States") %>% 
# #   filter(population >= 1000000) %>% 
# #   group_by(city, country, lat, lon, capital, population) %>% 
# #   summarise(auc = mean(auc)) %>% 
# #   ungroup() %>% 
# #   mutate(rank = row_number(auc),
# #          rank_rev = row_number(desc(auc)),
# #          points = auc / max(auc) * 100,
# #          name = reorder(city, rank)),
# 
# 
# summary_locations = data_daily %>%
#   mutate(year = year(date)) %>% 
#   filter(year < year(today())) %>% 
#   group_by(location_id, year) %>% 
#   summarise_at(vars(pleasant, hot, cold, elements), sum) %>% 
#   ungroup() %>% 
#   ## This if_else accounts for cases of leap year with all known days.
#   ## It makes sure we don't have negative unknown days
#   ## But also levels out leap year for the next step of averaging
#   mutate(unknown = if_else(pleasant + hot + cold  >= 365, 0, 365 - pleasant - hot - cold)) %>% 
#   filter(unknown < 365 * 0.1) %>% 
#   group_by(location_id) %>% 
#   summarise_at(vars(pleasant, hot, cold, elements, unknown), ~round(mean(.),0)) %>% 
#   ungroup() %>% 
#   left_join(locations_filtered, by = "location_id") %>% 
#   mutate(rank = row_number(desc(pleasant)),
#          rank_rev = row_number(pleasant),
#          points = pleasant / 365 * 100,
#          name = reorder(name, rank),
#          name_short = reorder(name_short, rank),
#          ),
# 
# print_plots = plot_data(df = summary_locations, 
#                          df2 = data_daily, 
#                          pop = 1000000, 
#                          n = 25, 
#                          dir = c("most", "least"), 
#                         scope = "United States",
#                          years = years,
#                          ncol = 5)


)
