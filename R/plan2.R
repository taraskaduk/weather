plan <- drake_plan(
  years = seq(year(today()) - 10, year(today())-1),
  start_date = ymd(paste0(min(years),"0101")),
  end_date = ymd(paste0(max(years),"1231")),
  
  

  # Cities -----------------------------------
  cities = get_cities(),
  
  # cities_sf = cities %>% 
  #   select(id, lat, lon) %>% 
  #   st_as_sf(coords = c("lon", "lat"), crs = 4236),
  # 

  locations = cities %>%
    rename(location_id=id) %>% 
    ## TEST TEST TEST TEST
    # filter(location_id %in%
    #          c("1840021990", "1484708778"
    #            , "1604728603", "1840021117", "1804382913"
    #            )) %>%
    filter(population >= 500000),

  
  # Stations  ----------------------------------------------------
  stations1 = locations %>% 
    mutate(st = purrr::map2(lat, lon, isd_stations_search, radius = 100)) %>% 
    select(location_id, st) %>% 
    unnest(cols = c(st)) %>% 
    mutate(stnid = paste(usaf, wban, sep = "-"),
           stnid2 = paste0(usaf, wban),
           begin=ymd(begin),
           end=ymd(end),
           useful_interval = ifelse(end > end_date, end_date, end) -
             ifelse(begin < start_date, start_date, begin) %>% as.numeric(),
           useful_interval = useful_interval / as.numeric(end_date - start_date)) %>% 
    filter(useful_interval > 0.75) %>% 
    mutate(
      country = maps::map.where('world', lon, lat),
      lakes = maps::map.where('lakes', lon, lat)
    ) %>%
    filter(!is.na(country) & is.na(lakes)) %>%
    select(-c(lakes, country)), 
    
  stations_df = stations1 %>% 
    mutate(elev_m =if_else(elev_m == -999.9, NA_real_, elev_m)) %>% 
    group_by(location_id) %>% 
    mutate(elev_min = if_else(distance>25, NA_real_, elev_m) %>% 
             min(na.rm = TRUE),
           elev_rank = abs(elev_m - elev_min) %>% 
             min_rank(),
           dist_rank = sqrt(distance) %>% 
             min_rank(),
           weight = elev_rank + dist_rank %>% 
             scales::rescale(to=c(1,0)),
           index = min_rank(elev_rank + dist_rank)) %>%  
    ungroup() %>% 
    filter(index <= 10), 
  
          
  locations_stations = locations %>% 
    inner_join(stations_df %>% 
                 select(location_id, stnid, stnid2, distance, elev_m, weight, index), 
               by="location_id"),
  
  # Get unique stations
  stations = locations_stations %>%
    select(stnid2) %>%
    distinct(),
  stations_v = as_vector(stations),
  
  # Weather -------------------------------------
  # weather_import = get_GSOD(years = years),
  weather = get_weather(yrs = years, stns = stations_v),
  
  # Join city and weather data ---------
  
  data_join = locations_stations %>%
    select(location_id, stnid, index, weight) %>% 
    inner_join(weather, by = "stnid") %>% 
    select(location_id, date = yearmoda,
           weight,
           index,
           temp_max = max,
           temp_min = min, 
           temp_mean = temp, 
           dewp, 
           # slp, 
           # stp, 
           # visib, 
           wdsp, 
           # mxspd, 
           # gust, 
           prcp, 
           sndp, 
           # i_rain_drizzle,
           # i_snow_ice,
           rh),
  
  
  data_sum = data_join %>% 
    pivot_longer(cols=temp_max:rh,
                 names_to = "var",
                 values_to = "value") %>% 
    filter(!(is.na(value) | ((var=="prcp" | var == "sndp") & value==0))) %>% 
    filter(index<=5) %>% 
    group_by(date,location_id, var) %>% 
    summarise(value = weighted.mean(value , w = weight,
              .groups = "drop")) %>%
    ungroup() %>% 
    # summarise(value = mean(value) %>% round(2),
    #           .groups = "drop") %>% 
    pivot_wider(names_from = "var",
                values_from = "value"),
  
  
    
  
  data = data_sum %>% 
    # mutate_all( ~ case_when(!is.nan(.x) ~ .x)) %>% 
    # mutate_at(vars(temp_max, temp_min, temp_mean, dewp, wdsp, prcp, sndp, rh), 
    #           ~if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)) %>% 
    replace_na(list(sndp = 0, prcp = 0)) %>%
    # A few possible substitutions
    mutate(temp_max = if_else(is.na(temp_max) & !is.na(temp_min) & !is.na(temp_mean), 
                              2*temp_mean - temp_min, temp_max),
           temp_min = if_else(is.na(temp_min) & !is.na(temp_max) & !is.na(temp_mean), 
                              2*temp_mean - temp_max, temp_min)) %>% 
    filter(!(is.na(temp_max) & is.na(temp_min))),
  
  
  
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
    filter(avg_ratio >= 0.75) %>% 
    select(location_id),
  
  full_data_filtered = full_data %>% 
    semi_join(data_check, by = "location_id"),
  locations_filtered = locations %>% 
    semi_join(data_check, by = "location_id"),
  data_filtered = data %>% 
    semi_join(data_check, by = "location_id"),
  
  
  # data_completed = full_data_filtered %>%
  #     #GATHER
  #     select(location_id, date, temp_min, temp_max, temp_mean, 
  #            rh, prcp, sndp, i_rain_drizzle, i_snow_ice, wdsp) %>%
  #     gather(metric, value, -c(location_id, date)) %>%
  #     mutate(flag = if_else(is.na(value), "missing", "train"),
  #            year = year(date),
  #            yday = yday(date)) %>% 
  #     #NEST
  #     group_by(location_id, metric, flag) %>%
  #     nest() %>%
  #     pivot_wider(names_from = flag, values_from = data) %>%
  #     filter(!is.na(missing) & !is.na(train) & 
  #              !is.null(missing)  & !is.null(train) &
  #              missing != "NULL" & train != "NULL") %>% 
  #     #MODEL
  #     mutate(knn = train %>% 
  #              purrr::map(f_knnreg)) %>% 
  #     mutate(pred_knn = map2(.x = knn, .y = missing, .f = predict)) %>% 
  #     #UNNEST
  #     select(-c(train, knn)) %>%
  #     unnest(c(missing, pred_knn)) %>%
  #     ungroup() %>% 
  #     mutate(value = pred_knn,
  #            flag = "predicted") %>%
  #     select(location_id, date, metric, value, flag, yday, year) %>% 
  #     #BIND
  #     select(-flag) %>%
  #     spread(metric, value) %>%
  #     bind_rows(data_filtered),
  # 
  # data_collapsed = data_completed %>% 
  #   select(location_id, date, temp_min, temp_max, temp_mean, 
  #          rh, prcp, sndp, i_rain_drizzle, i_snow_ice, wdsp) %>% 
  #   group_by(location_id, date) %>% 
  #   summarise_all(max, na.rm = TRUE) %>% 
  #   ungroup() %>% 
  #   mutate_at(vars(temp_min:wdsp), ~if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)) %>% 
  #   # replace_na(list(sndp = 0, prcp = 0, i_rain_drizzle = 0, i_snow_ice = 0, wdsp = 0, gust = 0)) %>%
  #   mutate_at(vars(temp_min:wdsp), round, digits = 2) %>% 
  #   mutate(temp_mean_feel = feels_like(temp_mean, rh, wdsp),
  #          temp_min_feel = feels_like(temp_min, rh, wdsp),
  #          temp_max_feel = feels_like(temp_max, rh, wdsp)) %>% 
  #   mutate(year = year(date),
  #          yday = yday(date)),
  
  # data_final = data_collapsed,
  
  data_final = data_filtered %>% 
    # mutate_at(vars(temp_max:rh), ~if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)) %>% 
    # replace_na(list(sndp = 0, prcp = 0, i_rain_drizzle = 0, i_snow_ice = 0, wdsp = 0, gust = 0)) %>%
    # mutate_at(vars(temp_max:rh), round, digits = 2) %>% 
    # mutate(temp_mean_feel = feels_like(temp_mean, rh, wdsp),
    #        temp_min_feel = feels_like(temp_min, rh, wdsp),
    #        temp_max_feel = feels_like(temp_max, rh, wdsp)) %>% 
    filter(!is.na(temp_max) & !is.na(temp_min)) %>% 
    bind_cols(map2_dfr(.$temp_min, .$temp_max, get_edd)) %>% 
    mutate(year = year(date),
           yday = yday(date)),
  

  
  params =  list(
    temp_hot = c(20, 30), 
    temp_cold = c(0, 10), #lowest would be night + sunrise temp. 
    # Let's rule out near freezing temps.
    #the upper limit is "when even the lowest night temp is too hot..."
    # temp_mean = c(13, 24),
    prcp = 2.5,
    sndp = 5
  ),
  
  data_daily = data_final %>%
    mutate(hot = if_else(temp_min > params$temp_hot[1] |
                           temp_max >  params$temp_hot[2], 1, 0),
           cold = if_else(temp_min < params$temp_cold[1] |
                            temp_max < params$temp_cold[2], 1, 0),
           elements = if_else(prcp > params$prcp |
                                sndp > params$sndp,
                              1, 0),
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
           double_class = factor(double_class, 
                                 levels = c("pleasant", "elements", 
                                            "cold", "cold & elements", 
                                            "hot", "hot & elements"))
    ),
  
  
  summary_locations = data_daily %>%
    mutate(year = year(date)) %>% 
    filter(year < year(today())) %>% 
    group_by(location_id, year) %>% 
    summarise_at(vars(pleasant, hot, cold, elements,
                   edd_hot, edd_cold, edd_total), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    ## This if_else accounts for cases of leap year with all known days.
    ## It makes sure we don't have negative unknown days
    ## But also levels out leap year for the next step of averaging
    mutate(unknown = if_else(pleasant + hot + cold + elements >= 365, 0, 
                             365 - pleasant - hot - cold - elements)) %>% 
    filter(unknown < 365 * 0.1) %>% 
    group_by(location_id) %>% 
    summarise_at(vars(pleasant, hot, cold, elements, unknown, 
                      edd_hot, edd_cold, edd_total), ~round(mean(., na.rm=TRUE),0)) %>% 
    ungroup() %>% 
    inner_join(locations_filtered, by = "location_id") %>% 
    mutate(rank = row_number(desc(pleasant)),
           rank_rev = row_number(pleasant),
           points = pleasant / 365 * 100,
           city = reorder(city, rank)
    ),
  
  # save_data = write_csv(data_final, "data/data.csv"),
  save_data = saveRDS(data_daily %>% 
                        select(-c(dewp,hot,cold,elements,pleasant,rh,wdsp,year,yday)), "data/data.RDS"),
  save_locations = write_csv(locations_filtered, "data/locations_filtered.csv"),
  save_cities = write_csv(locations, "data/locations.csv"),
  save_summary = saveRDS(summary_locations, "data/summary_locations.RDS")
  
  
)
