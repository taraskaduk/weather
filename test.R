ne_places <- st_read("data/ne_10m_populated_places")













test <- stations_df %>% 
  inner_join(locations, by = "location_id") %>% 
  filter(city == "San Diego" | city == "Tijuana")

loadd(weather)
loadd(locations_stations)
loadd(stations_df)

data_join = locations_stations %>%
  select(location_id, stnid, distance) %>% 
  filter(location_id %in% c("1840021990", "1484708778")) %>% 
  inner_join(weather, by = "stnid") %>% 
  select(date = yearmoda,
         stnid,
         distance,
         temp_max = max,
         temp_min = min, 
         temp_mean = temp) %>% 
    distinct()

data_max <- data_join %>% 
  group_by(stnid) %>% 
  summarise(temp_max = mean(temp_max, na.rm=TRUE)) %>% 
  ungroup() %>% 
  left_join(stations_df, by = "stnid")

ggplot()+
  geom_point(data=data_max, aes(x=lon, y=lat, alpha=temp_max, size = temp_max, col = as.character(location_id)))+
  geom_point(data=test, aes(x=lon.y, y = lat.y, fill=city ), col="black", size = 5, shape=23)

ggplot(test)+
  geom_point(aes(x=lon.x, y = lat.x, col=city, size = 1/distance), aplha=0.9) +
  geom_point(aes(x=lon.y, y = lat.y, fill=city ), col="black", size = 5, shape=23) +
  facet_wrap(~city) +
  theme_base()

leaflet(test) %>%
  addProviderTiles(providers$Stamen.Terrain) %>% 
  addCircleMarkers(lat = ~lat.x, lng = ~lon.x, color = ~city)
