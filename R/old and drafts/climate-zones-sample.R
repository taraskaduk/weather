library(kgc)

data <-
  data.frame(
    Site = c("GC", "UFS", "NEG", "Jax", "Portland"),
    Longitude = c(-15.42, 10.98, 34.78, -81.96, -70.28),
    Latitude = c(27.82, 47.42, 30.86, 30, 43.666)
  )

data <-
  data.frame(
    data,
    rndCoord.lon = RoundCoordinates(data$Longitude),
    rndCoord.lat = RoundCoordinates(data$Latitude)
  )

data <- data.frame(data, ClimateZ = LookupCZ(data))
data
