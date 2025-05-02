library(shiny)
library(ggplot2)
library(readr)
library(leaflet)
library(dplyr)
library(ggiraph)
library(plotly)
library(echarts4r)
library(sf)
library(jsonlite)
library(stars)
library(terra)
library(shinyWidgets)
library(here)

source(here::here("R", "plots_each_and_all_park.R"))

load_heatmap_components <- function() {
  serbia <- st_read(here::here("geo", "srbija_boundary.geojson"), quiet = TRUE)
  nc_path <- here::here("data", "data_stepType-avg.nc")
  r <- read_stars(nc_path)
  st_crs(r) <- 4326
  names(r) <- "precip"
  time_dim <- tryCatch({
    raw_times <- st_get_dimension_values(r, "valid_time")
    origin <- as.POSIXct("1970-01-01", tz = "UTC")
    as.Date(as.POSIXct(raw_times, origin = origin))
  }, error = function(e) NULL)
  serbia_aligned <- st_transform(serbia, st_crs(r))
  r_cropped <- tryCatch({ st_crop(r, serbia_aligned) }, error = function(e) r)
  all_vals <- as.vector(r_cropped[[1]])
  color_domain <- range(all_vals, na.rm = TRUE)
  list(serbia = serbia_aligned, r_cropped = r_cropped, time_dim = time_dim, color_domain = color_domain)
}

heatmap_components <- load_heatmap_components()

parks_static <- data.frame(
  name = c("Đerdap", "Tara", "Kopaonik", "Fruška Gora", "Šar Planina"),
  lat = c(44.60, 43.89, 43.30, 45.15, 42.19),
  lon = c(22.00, 19.56, 20.81, 19.72, 20.75)
)

color_palettes <- c("YlGnBu", "Plasma" = "plasma", "Green" = "Greens", "Oranges" = "Oranges", "RGB" = "RdYlBu")

ui <- fluidPage(
  titlePanel("Precipitation Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Choose a Park:",
                  choices = c("All Parks", "Đerdap National Park", "Tara National Park",
                              "Kopaonik National Park", "Fruška Gora National Park",
                              "Šar Planina National Park")),
      conditionalPanel(
        condition = "['ggiraph', 'Plotly', 'ECharts'].includes(input.tabs)",
        dateRangeInput("date_range", "Select Date Range:",
                       start = min(heatmap_components$time_dim),
                       end = max(heatmap_components$time_dim))
      ),
      conditionalPanel(
        condition = "input.tabs == 'Heatmap'",
        sliderTextInput("date_index", "Select Date for Heatmap:",
                        choices = format(heatmap_components$time_dim, "%Y-%m-%d"),
                        selected = format(heatmap_components$time_dim[1], "%Y-%m-%d"),
                        grid = TRUE,
                        animate = animationOptions(interval = 2000, loop = TRUE)),
        selectInput("palette_choice", "Color Palette:", choices = color_palettes, selected = "YlGnBu"),
        textOutput("selected_date")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs", selected = "Map",
                  tabPanel("Map", leafletOutput("map")),
                  tabPanel("Static Plot", plotOutput("precipPlot")),
                  tabPanel("Overall Static Plot", plotOutput("overallPlot")),
                  tabPanel("ggiraph", girafeOutput("girafePlot", width = "100%", height = "400px")),
                  tabPanel("Plotly", plotlyOutput("plotlyPlot")),
                  tabPanel("ECharts", echarts4rOutput("echartsPlot")),
                  tabPanel("Heatmap", leafletOutput("heatmap", height = 700)),
                  tabPanel("About", 
                           h3("About this App"),
                           p("This interactive Shiny application visualizes precipitation data across several national parks in Serbia."),
                           p("It supports multiple visualization types including static plots, interactive charts, and a heatmap."),
                           p("The underlying data is derived from NetCDF climate data and JSON-formatted precipitation type records."),
                           p("Use the sidebar to select a national park and time range, then explore the various tabs for visual insights."),
                           h4("Data Sources"),
                           tags$ul(
                             tags$li(
                               strong("Precipitation Types:"),
                               " Derived from JSON-formatted park-specific records prepared for the course."
                             ),
                             tags$li(
                               strong("Gridded Climate Data:"),
                               " ERA5 monthly averaged reanalysis data on single levels (2005–2025), variable: precipitation type. ",
                               "Downloaded from the Copernicus Climate Data Store: ",
                               a("https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means",
                                 href = "https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means", target = "_blank"),
                               ". Format: NetCDF."
                             ),
                             tags$li(
                               strong("Park Locations & Boundaries:"),
                               " Custom GeoJSON files created or curated for the Serbian national park regions."
                             )
                           ),
                           
                           h4("Precipitation Type Categories"),
                           tags$ul(
                             tags$li(strong("1:"), " Rain"),
                             tags$li(strong("2:"), " Snow"),
                             tags$li(strong("3:"), " Freezing Rain"),
                             tags$li(strong("4:"), " Ice Pellets / Sleet")
                           ),
                           p("These values are plotted over time to show how the type of precipitation varied at different national parks.")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- read_csv(here("data", "updated_precipitation_type_data.csv")) %>%
    mutate(time = as.Date(time))
  
  precip_json <- fromJSON(here("geo", "my_precipitation.json"))
  precip_df <- bind_rows(lapply(precip_json, function(x) {
    data.frame(
      park = rep(x$park, length(x$time)),
      time = as.Date(x$time),
      ptype = as.numeric(x$values)
    )
  }))
  
  filtered_data <- reactive({
    req(input$date_range)
    data %>%
      filter(time >= input$date_range[1], time <= input$date_range[2])
  })
  
  all_plots <- generate_precipitation_plots(data)
  
  locations <- data.frame(
    name = c("Đerdap National Park", "Tara National Park",
             "Kopaonik National Park", "Fruška Gora National Park",
             "Šar Planina National Park"),
    latitude = c(44.60, 43.890, 43.298, 45.1567, 42.1881),
    longitude = c(22.00, 19.564, 20.812, 19.7251, 20.7536)
  )
  
  output$map <- renderLeaflet({
    mapData <- if (input$park == "All Parks") locations else locations[locations$name == input$park, ]
    leaflet(data = mapData) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~name)
  })
  
  output$precipPlot <- renderPlot({
    if (input$park == "All Parks") {
      print(all_plots[["All Parks Combined"]])
    } else {
      print(all_plots[[input$park]])
    }
  })
  
  output$overallPlot <- renderPlot({
    print(all_plots[["All Parks Combined"]])
  })
  
  output$girafePlot <- renderGirafe({
    df <- filtered_data()
    if (input$park != "All Parks") df <- filter(df, park == input$park)
    gg <- ggplot(df, aes(x = time, y = ptype, tooltip = park)) +
      geom_line_interactive(aes(color = park), na.rm = TRUE) +
      labs(title = paste("Interactive Precipitation -", input$park),
           x = "Time", y = "Precipitation Type") +
      theme_minimal()
    girafe(ggobj = gg)
  })
  
  output$plotlyPlot <- renderPlotly({
    df <- filtered_data()
    if (input$park != "All Parks") df <- filter(df, park == input$park)
    p <- ggplot(df, aes(x = time, y = ptype, color = park)) +
      geom_line() +
      labs(title = paste("Plotly Precipitation -", input$park), x = "Time", y = "Precipitation Type") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$echartsPlot <- renderEcharts4r({
    df <- filtered_data()
    if (input$park != "All Parks") df <- filter(df, park == input$park)
    df %>%
      e_charts(time) %>%
      e_line(ptype, name = "Precipitation Type") %>%
      e_title(text = paste("ECharts: Precipitation -", input$park)) %>%
      e_tooltip(trigger = "axis") %>%
      e_theme("infographic")
  })
  
  output$heatmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = heatmap_components$serbia, color = "black", weight = 2, fill = FALSE) %>%
      addCircleMarkers(data = parks_static, ~lon, ~lat,
                       label = ~name, radius = 5,
                       fillColor = "red", stroke = TRUE, color = "white", weight = 1,
                       fillOpacity = 0.9)
  })
  
  output$selected_date <- renderText({
    paste("Date:", input$date_index)
  })
  
  observe({
    req(input$date_index)
    idx <- which(format(heatmap_components$time_dim, "%Y-%m-%d") == input$date_index)
    if (length(idx) == 0) return()
    
    slice <- tryCatch({ heatmap_components$r_cropped[,,,idx, drop = TRUE] }, error = function(e) return(NULL))
    if (is.null(slice)) return()
    
    slice_rast <- tryCatch({ terra::rast(slice) }, error = function(e) return(NULL))
    if (is.null(slice_rast)) return()
    
    vals <- terra::values(slice_rast)
    if (all(is.na(vals))) return()
    
    pal <- colorNumeric(input$palette_choice, domain = heatmap_components$color_domain, na.color = "transparent")
    
    leafletProxy("heatmap") %>%
      clearImages() %>%
      clearControls() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = heatmap_components$serbia, color = "black", weight = 2, fill = FALSE) %>%
      addRasterImage(slice_rast, colors = pal, opacity = 0.8) %>%
      addCircleMarkers(data = parks_static, ~lon, ~lat,
                       label = ~name, radius = 5, fillColor = "red",
                       stroke = TRUE, color = "white", weight = 1, fillOpacity = 0.9) %>%
      addLegend(pal = pal, values = heatmap_components$color_domain, title = "Precipitation (fixed scale)", position = "bottomright")
  })
}

shinyApp(ui = ui, server = server)
