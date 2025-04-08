library(ncdf4)

# Open the NetCDF file
nc_data <- nc_open("C:/Users/Ognjen/Desktop/Interactive Web Visualization for Hydroclimatologycal Data/Project/data/b434c48329f85d2eadfa24ba366a5f8/data_stepType-avg.nc")

# Extracting the precipitation type data
ptype_data <- ncvar_get(nc_data, "ptype")

# Getting dimensions information
time <- ncvar_get(nc_data, "valid_time")
latitude <- ncvar_get(nc_data, "latitude")
longitude <- ncvar_get(nc_data, "longitude")


# Create a data frame
library(tidyr)
library(dplyr)

# Create a data frame from the extracted arrays
df <- expand.grid(
  time = time,
  latitude = latitude,
  longitude = longitude,
  stringsAsFactors = FALSE
)

# Add precipitation type data as a new column to the data frame
df$ptype <- as.vector(ptype_data)

# Optionally convert the time from seconds since 1970-01-01 to date-time
df$time <- as.POSIXct(df$time, origin = "1970-01-01", tz = "UTC")

# Write the data frame to a CSV file
write.csv(df, "precipitation_type_data.csv", row.names = FALSE)


library(readr)

# Read the CSV file
precipitation_data <- read_csv("precipitation_type_data.csv")

# Convert the time from numeric to actual datetime format if not already converted
precipitation_data$time <- as.POSIXct(precipitation_data$time, origin = "1970-01-01", tz = "UTC")

# Create the data frame
data_df <- data.frame(
  time = precipitation_data$time,
  latitude = precipitation_data$latitude,
  longitude = precipitation_data$longitude,
  precipitation_type = precipitation_data$ptype
)

# Print the first few rows to check
head(data_df)


# Define the locations including additional parks
locations <- data.frame(
  name = c("Đerdap National Park", "Tara National Park", "Kopaonik National Park", "Fruška Gora National Park", "Šar Planina National Park"),
  latitude = c(44.672, 43.890, 43.298, 45.1567, 42.1881),
  longitude = c(22.040, 19.564, 20.812, 19.7251, 20.7536)
)

locations


library(ggplot2)

# Example data structure (make sure your actual data reflects this structure)
data_df <- data.frame(
  time = rep(seq(as.POSIXct("2005-01-01"), as.POSIXct("2024-12-31"), by="month"), 5),
  precipitation_type = runif(60, 0, 2),  # Random precipitation data
  park = rep(c("Đerdap National Park", "Tara National Park", "Kopaonik National Park", "Fruška Gora National Park", "Šar Planina National Park"), each = 12)
)

# Plotting
ggplot(data_df, aes(x = time, y = precipitation_type, color = park, group = park)) +
  geom_line() +  # Use geom_line() to connect points in the order of the time
  labs(title = "Time Series of Precipitation by National Park",
       x = "Time",
       y = "Precipitation Type",
       color = "National Park") +
  theme_minimal() +
  scale_color_manual(values = c("Đerdap National Park" = "red", 
                                "Tara National Park" = "blue", 
                                "Kopaonik National Park" = "green",
                                "Fruška Gora National Park" = "orange",
                                "Šar Planina National Park" = "purple"))

