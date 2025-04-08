library(sf)
library(dplyr)

setwd("C:/Users/Ognjen/Desktop/Interactive Web Visualization for Hydroclimatologycal Data/Project/geo/")

parks <- data.frame(
  name = c("Đerdap National Park", "Tara National Park", "Kopaonik National Park", 
           "Fruška Gora National Park", "Šar Planina National Park"),
  latitude = c(44.672, 43.890, 43.298, 45.1567, 42.1881),
  longitude = c(22.040, 19.564, 20.812, 19.7251, 20.7536)
)

parks_sf <- st_as_sf(parks, coords = c("longitude", "latitude"), crs = 4326)

# Write to GeoJSON
st_write(parks_sf, "my_parks_locations.geojson", driver = "GeoJSON", delete_dsn = TRUE)
