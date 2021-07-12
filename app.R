library(shiny)
library(leaflet)
library(sf)

cams <- read.csv("data/surrey_desc.csv")

camIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/material-outlined/24/000000/wallmount-camera.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10
)

basemap <- leaflet(data = cams, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>%
  setView(lng = -122.8, lat = 49.15, zoom = 12) %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(station_name), icon = camIcon) %>%
  addTiles()

ui <- fluidPage(
  leafletOutput("basemap"),
)

server <- function(input, output) {
  output$basemap <- renderLeaflet(basemap)
}

shinyApp(ui = ui, server = server)
