library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)

# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")

# Icon assets
camIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/material-outlined/24/000000/wallmount-camera.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10
)

# Other Constants
surreyLat <- 49.15
surreyLng <- -122.8

########## UI ##########

basemap <- leaflet(data = cams, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>%
  setView(lng = -122.8, lat = 49.15, zoom = 12) %>%
  addMarkers(~longitude, ~latitude, layerId = ~as.character(station_name), popup = ~as.character(station_name), icon = camIcon) %>%
  addTiles()

ui <- navbarPage("Unbiased Mobility", id="nav",
           
           tabPanel("Map",
                    div(class = "outer", 
                        tags$head(includeCSS("styles.css")),
                        leafletOutput("basemap", width = "100%", height = "100%"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Traffic explorer"),
                                      selectInput(inputId = "camid", 
                                                  label = "Camera ID", 
                                                  choices = c("103", "104", "enc_104_cityparkway_cam1", "TODO"), # TODO: load 364 cameras from csv 
                                                  multiple = FALSE),
                                      plotOutput("linePlotCarCounts", height = "200"))))
)

########## Server ##########

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
