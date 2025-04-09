library(dplyr)
library(readr)
library(jsonlite)

data <- read_csv("data/updated_precipitation_type_data.csv")  # Replace with your correct path
class(data)


precip_list <- data %>%
  filter(!is.na(park), !is.na(ptype)) %>%
  group_by(park) %>%
  arrange(time) %>%
  summarise(
    time = as.character(time),
    ptype = ptype,
    .groups = "drop"
  ) %>%
  group_split(park)

precip_json <- lapply(precip_list, function(df) {
  list(
    park = unique(df$park),
    time = df$time,
    values = df$ptype
  )
})

write_json(precip_json, "geo/my_precipitation.json", pretty = TRUE)
