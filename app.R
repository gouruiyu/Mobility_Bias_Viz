library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)

cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")

camIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/material-outlined/24/000000/wallmount-camera.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10
)

basemap <- leaflet(data = cams, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>%
  setView(lng = -122.8, lat = 49.15, zoom = 12) %>%
  addMarkers(~longitude, ~latitude, layerId = ~as.character(station_name), popup = ~as.character(station_name), icon = camIcon) %>%
  addTiles()

ui <- fluidPage(
  leafletOutput("basemap"),
  plotOutput("linePlotCarCounts")
)

server <- function(input, output) {
  output$basemap <- renderLeaflet(basemap)
  
  # current selected camera
  current_cam <- reactiveValues()
  
  observeEvent(input$basemap_marker_click, {
    marker <- input$basemap_marker_click
    if (is.null(marker$id))
      return()
    
    current_cam <- cams_data %>%
      filter(station == marker$id)
  
    output$linePlotCarCounts <- renderPlot({
      ggplot(data=current_cam, aes(y=car_count, x=as.POSIXct(time))) +
        geom_line() +
        scale_x_datetime(date_breaks = "24 hours", date_labels = "%Y-%m-%d %H:%M") +
        xlab("Time(hour)") +
        ylab("Car Count") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.title.y.right = element_text(color = "blue"))
        
    })
    
  })

}

shinyApp(ui = ui, server = server)
