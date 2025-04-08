library(sf)

serbia <- st_read("C:/Users/Ognjen/Desktop/Interactive Web Visualization for Hydroclimatologycal Data/Project/geo/geoBoundaries-SRB-ADM0-all/geoBoundaries-SRB-ADM0_simplified.geojson")
kosovo <- st_read("C:/Users/Ognjen/Desktop/Interactive Web Visualization for Hydroclimatologycal Data/Project/geo/geoBoundaries-XKX-ADM0-all/geoBoundaries-XKX-ADM0.geojson")

full_border <- rbind(serbia, kosovo)

# Save merged file
st_write(full_border, "geo/srbija_boundary.geojson", driver = "GeoJSON")
