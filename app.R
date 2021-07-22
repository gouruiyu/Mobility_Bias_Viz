library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(hms)

source("biz_data_clean.R")

# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")
bike_routes<-st_read("data/bikeroutes_in_4326.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326)

# Camera Icon asset
camIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/000000/camera--v1.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10
)
# highlighted icon style
cam_icon_highlight <- makeAwesomeIcon(icon = 'camera', markerColor = 'red')

# Other Constants

VEHICLE_TYPES <- c("car_count", "truck_count", "bus_count", "bicycle_count", "motorcycle_count", "person_count")
SURREY_LAT <- 49.15
SURREY_LNG <- -122.8
ZOOM_MIN = 10
ZOOM_MAX = 18

COLOR_LEGEND=c('#A6CEE3',"#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F")

LABELS=c("Bike Lanes","Boulevard Multi-Use Pathway",
         "Boulevard Separated Multi-Use Pathway",
         "Cycling-Permitted Sidewalk",
         "Neighbourhood Bike Route",
         "Protected Bike Lanes",
         "Shared Traffic")
pal<-colorFactor(palette="Paired", levels=c("Bike Lanes","Boulevard Multi-Use Pathway",
                                            "Boulevard Separated Multi-Use Pathway",
                                            "Cycling-Permitted Sidewalk",
                                            "Neighbourhood Bike Route",
                                            "Protected Bike Lanes",
                                            "Shared Traffic"))
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
                      "Services",
                      "Bike Route"),
    options=layersControlOptions(collapsed = TRUE))%>%
  hideGroup(c("Stores",
              "Food and Restaurants",
              "Liquor Stores",
              "Health and Medicine",
              "Business and Finance",
              "Services",
              "Bike Route"))%>%
  addMarkers(data=stores, popup = ~as.character(BusinessName),icon= storeIcon,group="Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=food.and.restaurant,popup = ~as.character(BusinessName), icon=restaurantIcon, group= "Food and Restaurants",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=alcohol,popup = ~as.character(BusinessName), icon= liquorIcon, group= "Liquor Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=health_medicine, popup = ~as.character(BusinessName),icon= healthIcon, group= "Health and Medicine",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=finances,popup = ~as.character(BusinessName),icon= bizIcon, group="Business and Finance",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=services,popup = ~as.character(BusinessName),icon= serviceIcon,group= "Services",
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))%>%
  addLegend("topleft",
            colors = COLOR_LEGEND,
            labels= LABELS,
            title= "Bike Route Type", group= "Bike Route")%>%
    addPolygons(data=bike_routes,weight = 4, color = ~pal(BIKE_INFRASTRUCTURE_TYPE), fill = FALSE, group= 'Bike Route')

  

plotVehicleCountWithTime <- function(df, dateRange, timeRange, vehicleType) {
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
  hourRange <- as_hms(with_tz(timeRange, "America/Vancouver"))
  df$time <- as.POSIXct(df$time)
  df$date <- as.POSIXct(format(df$time, "%Y-%m-%d"))
  df$station <- as.factor(df$station)
  df <- df %>% 
    filter(date %within% interval(dateRange[1], dateRange[2])) %>%
    filter(as_hms(time) >= hourRange[1] & as_hms(time) <= hourRange[2])
  myplot <- ggplot(data=df, aes_string(y=vehicleType, x="time", color="station")) +
    geom_point() +
    geom_smooth(method = 'loess', formula = 'y~x') +
    scale_x_datetime(date_breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M", limits = as.POSIXct(paste(dateRange, hourRange), format="%Y-%m-%d %H:%M")) +
    xlab("Time(hour)") +
    ylab(vehicleType) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="bottom")
  return(myplot)
}


ui <- dashboardPage(
  dashboardHeader(title = "Unbiased Mobility"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu( id = "sidemenu",
      menuItem("Cam Map", tabName = "basemap", icon = icon("camera")),
      menuItem("User Inputs", tabName = "userInputs", icon = icon("user")),
      hidden(
        sliderInput(
          "dateRange", label = "Choose Date Range:",
          min = as.POSIXct("2020-12-01 00:00:00"),
          max = as.POSIXct("2020-12-31 23:59:59"),
          value = c(as.POSIXct("2020-12-01 00:00:00"),as.POSIXct("2020-12-07 23:59:59")),
          timeFormat = "%F", ticks = F, animate = T
        ),
        sliderInput(
          "timeRange", label = "Choose Time Range:",
          min = as.POSIXct("2020-12-01 00:00:00"),
          max = as.POSIXct("2020-12-01 23:59:59"),
          value = c(as.POSIXct("2020-12-01 00:00:00"), as.POSIXct("2020-12-01 23:59:59")),
          timeFormat = "%T", ticks = F, animate = T, timezone = "-0800"
        ),
        selectInput(inputId = "camid",
                    label = "Camera ID",
                    choices = cams$station_name,
                    multiple = FALSE),
        radioButtons("vehicleType", "Vehicle Type:",
                     VEHICLE_TYPES)
        # checkboxInput("realtimeImg",label = "Display current traffic image", value = TRUE)
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
                                width = 500, height = "auto",
                                h3("Traffic explorer"),
                                plotOutput("linePlotVehicleCounts", height = "200"))),
              absolutePanel(id = "camera_img",
                            draggable = TRUE, top = "auto", left = "auto" , right = "auto", bottom = 20,
                            width = 300, height = "auto",
                            imageOutput("testImg", height="auto"))
    )
  )
))

########## Server ##########

server <- function(input, output, session) {
  output$basemap <- renderLeaflet({
    basemap
    })

  # current selected camera
  current_cam <- reactiveValues(id = NULL, data = NULL, lat = NULL, lng = NULL, selected = TRUE)
  selected_cams <- reactiveValues(ids = c(), data = c())
  
  # current map center
  map_view <- reactiveValues()
  
  # to keep track of previously selected marker
  prev_selected <- reactiveVal()
  # highlight current selected cam marker
  proxy <- leafletProxy('basemap')

  # dynamically show/hide userInputs in the sidebarMenu
  observeEvent(input$sidemenu, {
    if (input$sidemenu == "userInputs") {
      shinyjs::show("dateRange")
      shinyjs::show("timeRange")
      shinyjs::show("camid")
      shinyjs::show("vehicleType")



    } else {
      shinyjs::hide("dateRange")
      shinyjs::hide("timeRange")
      shinyjs::hide("camid")
      shinyjs::hide("vehicleType")

    }
  })
  
  # Update current camera according to selected input
  observeEvent(input$camid, {
    if (is.null(input$camid)) return()
    current_cam$id <- input$camid

    if (current_cam$id %notin% selected_cams$ids) {
      selected_cams$ids <- c(current_cam$id, selected_cams$ids)
    }

  })
  
  # Update current camera according to marker click
  observeEvent(input$basemap_marker_click, {
    marker <- input$basemap_marker_click
    if (is.null(marker$id)) return()
    current_cam$id <- marker$id
    isolate({ 
      updateSelectInput(session, 'camid', selected = marker$id)
    })
    
    # toggle selection state
    current_cam$selected <- !current_cam$selected
    if (current_cam$selected) {
      selected_cams$ids <- selected_cams$ids[selected_cams$ids != current_cam$id]
    } else {
      selected_cams$ids <- c(selected_cams$ids, current_cam$id)
    }
    

    if (!is.null(prev_selected())) {
      if (prev_selected()$selected) {
        # de-selecting previous camera
        proxy %>%
          addMarkers(popup=as.character(prev_selected()$id),
                     layerId = as.character(prev_selected()$id),
                     lng=prev_selected()$lng,
                     lat=prev_selected()$lat,
                     icon = camIcon)
      } else {
        # re-selecting on the same camera
        proxy %>%
          addAwesomeMarkers(popup=as.character(current_cam$id),
                            layerId = as.character(current_cam$id),
                            lng=current_cam$lng, 
                            lat=current_cam$lat,
                            icon = cam_icon_highlight)
      }

    } 
    else {
      selected_cams$ids <- c(current_cam$id, selected_cams$ids)
      proxy %>%
        addAwesomeMarkers(popup=as.character(current_cam$id),
                          layerId = as.character(current_cam$id),
                          lng=current_cam$lng,
                          lat=current_cam$lat,
                          icon = cam_icon_highlight)

    }
    prev_selected(current_cam)
  })
  
  # Smooth pan map view based on camera selected
  observeEvent(current_cam$id, {
    data <- cams %>% filter(station_name == current_cam$id)
    # Update current_cam data on id change
    current_cam$lng <- cams[which(cams$station_name == current_cam$id),]$longitude
    current_cam$lat <- cams[which(cams$station_name == current_cam$id),]$latitude
    current_cam$data <- cams_data %>% filter(station == current_cam$id)
    map_view$lng = data$longitude
    map_view$lat = data$latitude
    map_view$zoom = max(input$basemap_zoom, (ZOOM_MAX + ZOOM_MIN)/2)
    leafletProxy('basemap', session) %>%
      flyTo('basemap', 
            lng = map_view$lng,
            lat = map_view$lat,
            zoom = map_view$zoom)
    
    selected_cams$ids <- c(selected_cams$ids, current_cam$id)
    print(selected_cams$ids)

    proxy %>%
      addAwesomeMarkers(popup=as.character(current_cam$id),
                        layerId = as.character(current_cam$id),
                        lng=current_cam$lng, 
                        lat=current_cam$lat,
                        icon = cam_icon_highlight)
    
  })

  observeEvent(selected_cams$ids, {
    selected_cams$data <- cams_data %>% filter(station %in% selected_cams$ids)
    output$linePlotVehicleCounts <- renderPlot({
      plotVehicleCountWithTime(selected_cams$data, 
                               as.POSIXct(format(input$dateRange, "%Y-%m-%d")),
                               input$timeRange,
                               input$vehicleType)
    })
  })
  
  # Camera Image

  # output$testImg <- renderImage({
  #   filename <- normalizePath(file.path('data/example_cam.jpg'))
  #   # Return a list containing the filename and alt text
  #   list(src = filename, alt = "Camera Image", width = 300)
  # }, deleteFile = FALSE)
  output$image <- renderUI({
    img_URI = NULL
    if (!is.null(current_cam$id) & length(selected_cams$ids) == 1) {
      img_URI = fetchRealtimeImg(station = current_cam$id)
    }
    if (!is.null(img_URI)) {
      tags$div(tags$img(src = img_URI, width = 300))
    }
  })

}

shinyApp(ui = ui, server = server)

