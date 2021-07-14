library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)

source("biz_data_clean.R")

# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")


# Camera Icon asset
camIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/000000/camera--v1.png",
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
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Stores",
                      "Food and Restaurants",
                      "Liquor Stores",
                      "Health and Medicine",
                      "Business and Finance",
                      "Services"),
    options=layersControlOptions(collapsed = TRUE))%>%
  hideGroup(c("Stores",
              "Food and Restaurants",
              "Liquor Stores",
              "Health and Medicine",
              "Business and Finance",
              "Services"))%>%
  addMarkers(data=stores, popup = ~as.character(BusinessName),icon= storeIcon,group="Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=food.and.restaurant,popup = ~as.character(BusinessName), icon=restaurantIcon, group= "Food and Restaurants",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=alcohol,popup = ~as.character(BusinessName), icon= liquorIcon, group= "Liquor Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=health_medicine, popup = ~as.character(BusinessName),icon= healthIcon, group= "Health and Medicine",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=finances,popup = ~as.character(BusinessName),icon= bizIcon, group="Business and Finance",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=services,popup = ~as.character(BusinessName),icon= serviceIcon,group= "Services",
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))

plotCarCountWithTime <- function(df, start, end) {
  if (nrow(df) == 0) {
    return(ggplot() + # Draw ggplot2 plot with text only
             annotate("text",
                      x = 1,
                      y = 1,
                      size = 4,
                      color = "Red",
                      label = "The data for this camera is not available.") + 
             theme_void())
  }
  df$time <- as.POSIXct(df$time)
  df <- df %>% filter(time %within% interval(start, end))
  myplot <- ggplot(data=df, aes(y=car_count, x=time)) +
    geom_line() +
    geom_smooth() +
    scale_x_datetime(date_breaks = "24 hours", date_labels = "%Y-%m-%d %H:%M") +
    xlab("Time(hour)") +
    ylab("Car Count") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  return(myplot)
}

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
                                                  choices = cams$station_name, # TODO: load 364 cameras from csv 
                                                  multiple = FALSE),
                                      plotOutput("linePlotCarCounts", height = "200")))))

########## Server ##########

server <- function(input, output, session) {
  output$basemap <- renderLeaflet(basemap)
  
  # current selected camera
  current_cam <- reactiveValues()
  
  observeEvent(input$basemap_marker_click, {
    marker <- input$basemap_marker_click
    if (is.null(marker$id))
      return()
    
    current_cam <- cams_data %>%
      filter(station == marker$id)
    
    isolate({
      updateSelectInput(session, 'camid', selected = marker$id)
    })

      output$linePlotCarCounts <- renderPlot({
        plotCarCountWithTime(current_cam, 
                             as.POSIXct("2020-12-01 00:00:00"), # input$startHour, 
                             as.POSIXct("2020-12-07 23:59:59")) # input$endHour)
        
      })

  })

}

shinyApp(ui = ui, server = server)
