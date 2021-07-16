library(shiny)
library(shinydashboard)
library(shinyjs)
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

VEHICLE_TYPES <- c("car_count", "truck_count", "bus_count", "bicycle_count", "motorcycle_count", "person_count")
SURREY_LAT <- 49.15
SURREY_LNG <- -122.8
ZOOM_MIN = 10
ZOOM_MAX = 18

########## UI ##########

basemap <- leaflet(data = cams, options = leafletOptions(minZoom = ZOOM_MIN, maxZoom = ZOOM_MAX)) %>%
  setView(lng = SURREY_LNG, lat = SURREY_LAT, zoom = (ZOOM_MIN+ZOOM_MAX)/2) %>%
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

plotVehicleCountWithTime <- function(df, start, end, vehicleType) {
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
  myplot <- ggplot(data=df, aes_string(y=vehicleType, x="time")) +
    geom_line() +
    geom_smooth(method = 'loess', formula = 'y~x') +
    scale_x_datetime(date_breaks = "24 hours", date_labels = "%Y-%m-%d %H:%M") +
    xlab("Time(hour)") +
    ylab(vehicleType) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  return(myplot)
}

ui <- dashboardPage(
  dashboardHeader(title = "Unbiased Mobility"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu( id = "sidemenu",
      menuItem("Cam Map", tabName = "basemap", icon = icon("camera")),
      menuItem("User Inputs", tabName = "userInputs", icon = icon("person")),
      hidden(
        sliderInput(
          "timeRange", label = "Choose Time Range:",
          min = as.POSIXct("2020-12-01 00:00:00"),
          max = as.POSIXct("2020-12-31 23:59:59"),
          value = c(as.POSIXct("2020-12-01 00:00:00"), as.POSIXct("2020-12-07 23:59:59")),
          timeFormat = "%Y-%m-%d %H:%M", ticks = F, animate = T
        ),
        selectInput(inputId = "camid",
                    label = "Camera ID",
                    choices = cams$station_name,
                    multiple = FALSE),
        selectInput(inputId = "vehicleType",
                    label = "Vehicle Type",
                    choices = VEHICLE_TYPES,
                    multiple = FALSE)
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "basemap",
              div(class = "outer", 
                  tags$head(includeCSS("styles.css")),
                  leafletOutput("basemap", width = "100%", height = "100%"),
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                width = 330, height = "auto",
                                h2("Traffic explorer"),
                                plotOutput("linePlotVehicleCounts", height = "200"))
                  )
      )
    )
  )
  )

########## Server ##########

server <- function(input, output, session) {
  output$basemap <- renderLeaflet(basemap)

  # current selected camera
  current_cam <- reactiveValues()
  
  # current map center
  map_view <- reactiveValues()
  
  # dynamically show/hide userInputs in the sidebarMenu
  observeEvent(input$sidemenu, {
    if (input$sidemenu == "userInputs") {
      shinyjs::show("timeRange")
      shinyjs::show("camid")
      shinyjs::show("vehicleType")
    } else {
      shinyjs::hide("timeRange")
      shinyjs::hide("camid")
      shinyjs::hide("vehicleType")
    }
  })
  
  # Update current camera according to selected input
  observeEvent(input$camid, {
    if (is.null(input$camid)) return()
    current_cam$id <- input$camid
  })
  
  # Update current camera according to marker click
  observeEvent(input$basemap_marker_click, {
    marker <- input$basemap_marker_click
    if (is.null(marker$id)) return()
    current_cam$id <- marker$id
    isolate({ 
      updateSelectInput(session, 'camid', selected = marker$id)
    })
  })
  
  # Smooth pan map view based on camera selected
  observeEvent(current_cam$id, {
    data <- cams %>% filter(station_name == current_cam$id)
    # Update current_cam data on id change
    current_cam$data <- cams_data %>% filter(station == current_cam$id)
    map_view$lng = data$longitude
    map_view$lat = data$latitude
    map_view$zoom = max(input$basemap_zoom, (ZOOM_MAX + ZOOM_MIN)/2)
    leafletProxy('basemap', session) %>%
      flyTo('basemap', 
            lng = map_view$lng,
            lat = map_view$lat,
            zoom = map_view$zoom)
    output$linePlotVehicleCounts <- renderPlot({
      plotVehicleCountWithTime(current_cam$data, 
                               input$timeRange[1],
                               input$timeRange[2],
                               input$vehicleType)
      
    })
  })

}

shinyApp(ui = ui, server = server)
