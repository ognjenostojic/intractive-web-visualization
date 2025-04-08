library(stars)
library(leaflet)
library(sf)
library(raster)

load_heatmap_components <- function() {
  serbia <- st_read("geo/srbija_boundary.geojson", quiet = TRUE)
  nc_path <- "data/b434c48329f85d2eadfa24ba366a5f8/data_stepType-avg.nc"
  
  # Load NetCDF as stars and assign CRS
  r <- read_stars(nc_path)
  st_crs(r) <- 4326
  names(r) <- "precip"
  
  # Extract and convert time dimension
  time_dim <- tryCatch({
    raw_times <- st_get_dimension_values(r, "valid_time")
    origin <- as.POSIXct("1970-01-01", tz = "UTC")
    as.Date(as.POSIXct(raw_times, origin = origin))
  }, error = function(e) {
    warning("Could not extract time dimension")
    NULL
  })
  
  # Ensure Serbia has the same CRS and crop
  serbia_aligned <- st_transform(serbia, st_crs(r))
  r_cropped <- tryCatch({
    st_crop(r, serbia_aligned)
  }, error = function(e) {
    warning("Cropping failed:", conditionMessage(e))
    r
  })
  
  list(
    serbia = serbia_aligned,
    r_cropped = r_cropped,
    time_dim = time_dim
  )
}

render_heatmap_ui <- function() {
  tagList(
    leafletOutput("heatmap", height = 700),
    textOutput("selected_date")
  )
}

heatmap_server <- function(input, output, r_cropped, time_dim, serbia) {
  output$heatmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = serbia, color = "black", weight = 2, fill = FALSE)
  })
  
  output$selected_date <- renderText({
    req(time_dim)
    idx <- input$date_index
    if (idx >= 1 && idx <= length(time_dim)) {
      paste("Date:", format(time_dim[idx], "%Y-%m-%d"))
    } else {
      "Invalid date index"
    }
  })
  
  observe({
    req(input$date_index)
    idx <- input$date_index
    if (idx > length(time_dim)) return()
    
    slice <- tryCatch({
      r_cropped[,,,idx, drop = TRUE]
    }, error = function(e) {
      warning("Slice extraction failed:", conditionMessage(e))
      return(NULL)
    })
    
    if (is.null(slice)) return()
    
    slice_raster <- tryCatch({
      as(slice, "Raster")
    }, error = function(e) {
      warning("Raster conversion failed:", conditionMessage(e))
      return(NULL)
    })
    
    if (is.null(slice_raster)) return()
    
    vals <- raster::getValues(slice_raster)
    if (all(is.na(vals))) return()
    
    pal <- colorNumeric("YlGnBu", domain = vals, na.color = "transparent")
    
    leafletProxy("heatmap") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(slice_raster, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = vals,
                title = "Precipitation", position = "bottomright")
  })
}
