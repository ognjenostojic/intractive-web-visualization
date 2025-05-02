library(readr)
library(dplyr)
library(geosphere)

# Read the CSV file
data <- read_csv("precipitation_type_data.csv")

# Define the park locations with an additional 'radius' column in meters
parks <- data.frame(
  name = c("Đerdap National Park", "Tara National Park", "Kopaonik National Park", 
           "Fruška Gora National Park", "Šar Planina National Park"),
  latitude = c(44.672, 43.890, 43.298, 45.1567, 42.1881),
  longitude = c(22.040, 19.564, 20.812, 19.7251, 20.7536),
  radius = c(20000, 15000, 12000, 15000, 18000)  
)

# Function to find the nearest park within the defined radius
find_nearest_park_within_radius <- function(lat, long) {
  distances <- distm(x = cbind(long, lat), y = cbind(parks$longitude, parks$latitude), fun = distHaversine)
  closest <- apply(distances, 1, function(dist) {
    if (min(dist) < parks$radius[which.min(dist)]) {
      return(parks$name[which.min(dist)])
    } else {
      return(NA)
    }
  })
  return(closest)
}

# Apply the function to assign parks
data$park <- mapply(find_nearest_park_within_radius, data$latitude, data$longitude)

# Remove rows with NA in key columns
data_clean <- data %>%
  filter(!is.na(ptype), !is.na(time), !is.na(park), !is.na(latitude), !is.na(longitude))

# Deduplicate by averaging ptype for each park and time
data_clean <- data_clean %>%
  group_by(park, time) %>%
  summarise(ptype = mean(ptype, na.rm = TRUE), .groups = "drop")

# Write the cleaned and aggregated data to CSV
write_csv(data_clean, "updated_precipitation_type_data.csv")
