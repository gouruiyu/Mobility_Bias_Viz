library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(sf)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(hms)
library(rlang)
library(DT)

source("biz_data_clean.R")
source("heatmap_function.R")
source("stats/ampm_comparison_model.R")
source("camera_img.R")
source("stats/undercount_model.R")
source("readBoundaries.R")
source("bikevars.R") #BIKE_COLOR_LEGEND, BIKE_LABLES & palBike  variables used


# Feature toggle
MULTI_SELECT_TOGGLE = TRUE

# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")
neighbourhood<-readBoundaries('data/surrey_city_boundary.json')
bike_routes <- st_read("data/bikeroutes_in_4326.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326)
heatmap_data<-render.daily(cams_data,cams)
#sort by neighbourhood
neighbourhood_names <- neighbourhood$NAME %>%
  as.character(.) %>%
  sort()

# Load undercount correction model
car_detected_vs_counted = read.csv("data/car_detected_vs_counted.csv")
uc_correction_model = fitModel(car_detected_vs_counted)

# Camera Icon asset
camIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/000000/camera--v1.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10
)
# highlighted icon style
cam_icon_highlight <- makeAwesomeIcon(icon = 'camera', markerColor = 'red')

# Other Constants
NEIGHBOURHOODS <- c("CITY CENTRE", "CLOVERDALE", "FLEETWOOD", "GUILDFORD",
                    "NEWTON", "SOUTH SURREY", "WHALLEY")
VEHICLE_TYPES_NAME <- c("Car", "Truck", "Bus", "Bicycle", "Motorcycle", "Person")
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
  addLegend("topleft",
            colors = BIKE_COLOR_LEGEND,
            labels= BIKE_LABELS,
            title= "Bike Lane Type",
            group="Bike Routes")%>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Stores",
                      "Food and Restaurants",
                      "Liquor Stores",
                      "Health and Medicine",
                      "Business and Finance",
                      "Services",
                      "Bike Routes"),
    options=layersControlOptions(collapsed = TRUE))%>%
  hideGroup(c("Stores",
              "Food and Restaurants",
              "Liquor Stores",
              "Health and Medicine",
              "Business and Finance",
              "Services",
              "Bike Routes"))%>%
  addMarkers(data=stores, popup = ~as.character(BusinessName),icon= storeIcon,group="Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=food.and.restaurant,popup = ~as.character(BusinessName), icon=restaurantIcon, group= "Food and Restaurants",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=alcohol,popup = ~as.character(BusinessName), icon= liquorIcon, group= "Liquor Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=health_medicine, popup = ~as.character(BusinessName),icon= healthIcon, group= "Health and Medicine",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=finances,popup = ~as.character(BusinessName),icon= bizIcon, group="Business and Finance",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=services,popup = ~as.character(BusinessName),icon= serviceIcon,group= "Services",
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))

baseHeatmap <- leaflet(options = leafletOptions(minZoom = ZOOM_MIN, maxZoom = ZOOM_MAX)) %>%
  setView(lng = SURREY_LNG, lat = SURREY_LAT, zoom = (ZOOM_MIN+ZOOM_MAX)/2) %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLegend("topleft",
            colors = BIKE_COLOR_LEGEND,
            labels= BIKE_LABELS,
            title= "Bike Lane Type",
            group="Bike Routes")%>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Stores",
                      "Food and Restaurants",
                      "Liquor Stores",
                      "Health and Medicine",
                      "Business and Finance",
                      "Services",
                      "Bike Routes"),
    options=layersControlOptions(collapsed = TRUE))%>%
  hideGroup(c("Stores",
              "Food and Restaurants",
              "Liquor Stores",
              "Health and Medicine",
              "Business and Finance",
              "Services",
              "Bike Routes"))%>%
  addMarkers(data=stores, popup = ~as.character(BusinessName),icon= storeIcon,group="Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=food.and.restaurant,popup = ~as.character(BusinessName), icon=restaurantIcon, group= "Food and Restaurants",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=alcohol,popup = ~as.character(BusinessName), icon= liquorIcon, group= "Liquor Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=health_medicine, popup = ~as.character(BusinessName),icon= healthIcon, group= "Health and Medicine",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=finances,popup = ~as.character(BusinessName),icon= bizIcon, group="Business and Finance",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=services,popup = ~as.character(BusinessName),icon= serviceIcon,group= "Services",
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))



plotVehicleCountWithTime <- function(df, dateRange, timeRange, vehicleType, weekdayOnly, displayCorrection=FALSE) {
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
  
  # Toggle off display correction if vehicle type if not car for now
  displayCorrection = (displayCorrection) & (vehicleType == 'car_count')
  
  hourRange <- as_hms(with_tz(timeRange, "America/Vancouver"))
  df$time <- as.POSIXct(df$time)
  df$date <- as.POSIXct(format(df$time, "%Y-%m-%d"))
  df$station <- as.factor(df$station)
  df <- df %>% 
    filter(date %within% interval(dateRange[1], dateRange[2])) %>%
    filter(as_hms(time) >= hourRange[1] & as_hms(time) <= hourRange[2])
  # Filter on weekdays only
  if (weekdayOnly == TRUE) df <- df[which(wday(df$date) %notin% c(6, 7)),]
  myplot <- ggplot(data=df, aes_string(y=vehicleType, x="time", color="station")) +
    geom_point() +
    geom_smooth(method = 'loess', formula = 'y~x') +
    scale_x_datetime(date_breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M", limits = as.POSIXct(paste(dateRange, hourRange), format="%Y-%m-%d %H:%M")) +
    xlab("Time(hour)") +
    ylab(vehicleType) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="bottom")
  
  if (!displayCorrection) {
    myplot <- myplot +
      geom_point()
  } else {
    # Bias corrected line
    pred <- predict(uc_correction_model, newdata=data.frame(detected = df[[vehicleType]]))
    myplot <- myplot + 
      geom_point(data=df, aes(y=pred))
  }
  myplot <- myplot + geom_smooth(method = 'loess', formula = 'y~x')
  return(myplot)
}

#fxn for heatmap input
filter_DT<-function(df,heatTime,heatDate){
  hourRange <- as_hms(with_tz(heatTime, "America/Vancouver"))
  df$time <- as.POSIXct(df$time)
  df$date <- as.POSIXct(format(df$time, "%Y-%m-%d"))
  df <- df %>% 
    filter(date %within% interval(heatDate[1], heatDate[2])) %>%
    filter(as_hms(time) >= hourRange[1] & as_hms(time) <= hourRange[2])
  
}



ui <- dashboardPage(
  dashboardHeader(title = "Unbiased Mobility"),
  dashboardSidebar(
    # useShinyjs(),
    sidebarMenu( id = "sidemenu",
                 menuItem("Cam Map", tabName = "basemap", icon = icon("camera")),
                 menuItem("Heatmap", tabName = "baseHeatmap", icon = icon("camera")),
                 hidden(
                 sliderInput(
                   "heatDate", label = "Choose Date Range:",
                   min = as.POSIXct("2020-12-01 00:00:00"),
                   max = as.POSIXct("2020-12-31 23:59:59"),
                   value = c(as.POSIXct("2020-12-01 00:00:00"),as.POSIXct("2020-12-07 23:59:59")),
                   timeFormat = "%F", ticks = F, animate = T
                 ),
                 sliderInput(
                   "heatTime", label = "Choose Time Range:",
                   min = as.POSIXct("2020-12-01 00:00:00"),
                   max = as.POSIXct("2020-12-01 23:59:59"),
                   value = c(as.POSIXct("2020-12-01 00:00:00"), as.POSIXct("2020-12-01 23:59:59")),
                   timeFormat = "%T", ticks = F, animate = T, timezone = "-0800"
                 )),
                 menuItem("Help", tabName = "help", icon = icon("question-circle")),
                 menuItem("User Inputs", tabName = "userInputs", icon = icon("user")),
                 hidden(
                 selectInput(
                   "neighbourhood_names",
                   label = "Select a Neighbourhood:",
                   choices=(neighbourhood_names),
                   selected= "SURREY"
                 ),
                 selectInput(inputId = "camid",
                             label = "Camera ID",
                             choices = cams$station_name,
                             multiple = FALSE),
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
                 actionBttn(inputId = "amHour", label = "AM Hours", color = "primary", style = "fill"),
                 actionBttn(inputId = "pmHour", label = "PM Hours", color = "primary", style = "fill"),
                 radioButtons("vehicleType", "Vehicle Type:", choiceNames = VEHICLE_TYPES_NAME,
                              choiceValues = VEHICLE_TYPES),
                 checkboxInput("displayCorrection", label = "Correct for undercounting (only effective for car type)", value = FALSE)
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
                                box(
                                  title = "Traffic Explorer",
                                  materialSwitch("weekdayOnly", label = "Weekdays Only", status = "primary", inline = TRUE, value = FALSE),
                                  materialSwitch("displayCorrection", label = "Correct for Undercount", status = "primary", inline = TRUE, value = FALSE),
                                  plotOutput("linePlotVehicleCounts", height = "200"),
                                  collapsible = T,
                                  width = "100%", height = "auto"
                                ),
                                box(
                                  title = "AM VS PM Biases (car count ONLY)",
                                  DT::dataTableOutput("ampmPairTable"),
                                  collapsible = T,
                                  width = "100%", height = "auto"
                                )
                  )
              ),
              absolutePanel(id = "camera_img",
                            draggable = TRUE, top = "auto", left = "auto" , right = "auto", bottom = 20,
                            width = 300, height = "auto",
                            uiOutput("image", height="auto"))
      ),
      tabItem(tabName = "help",
              includeMarkdown("help.md")),
      tabItem("baseHeatmap", div(class = "outer", leafletOutput("heatmap", width = "100%", height = "100%")))
    )
  ))

########## Server ##########

server <- function(input, output, session) {
  #load maps
  output$basemap <- renderLeaflet(basemap)
  output$heatmap <- renderLeaflet(baseHeatmap)
  
  # dynamically show/hide userInputs in the sidebarMenu
  observeEvent(input$sidemenu, {
    if (input$sidemenu == "userInputs") {
      shinyjs::hide("heatTime")
      shinyjs::hide("heatDate")
      shinyjs::show("neighbourhood_names")
      shinyjs::show("timeRange")
      shinyjs::show("dateRange")
      shinyjs::show("camid")
      shinyjs::show("vehicleType")
      shinyjs::show("amHour")
      shinyjs::show("pmHour")
      shinyjs::show("displayCorrection")
    } 
    else {
      shinyjs::show("heatTime")
      shinyjs::show("heatDate")
      shinyjs::hide("neighbourhood_names")
      shinyjs::hide("timeRange")
      shinyjs::hide("dateRange")
      shinyjs::hide("camid")
      shinyjs::hide("vehicleType")
      shinyjs::hide("amHour")
      shinyjs::hide("pmHour")
      shinyjs::hide("displayCorrection")

    }
  })
  
  
  #select boundaries
  observeEvent(input$neighbourhood_names,{
    req(input$neighbourhood_names)
    if(input$neighbourhood_names =="SURREY"){
      data <- neighbourhood %>%
        dplyr::filter(NAME %in% NEIGHBOURHOODS)
    } else {
      data <- neighbourhood %>% 
        dplyr::filter(NAME == input$neighbourhood_names)}
    
    leafletProxy("basemap", data= data) %>%
      clearShapes() %>%
      addPolygons(color = "#141722", weight = 3, smoothFactor = 0.5,
                  fillOpacity = 0, opacity = 0.2)%>%
      addPolygons(data=bike_routes,weight = 4, color = ~palBike(BIKE_INFRASTRUCTURE_TYPE), opacity=0.3,fill = FALSE,
                  group="Bike Routes")%>%
      setView(lng = ifelse(input$neighbourhood_names == "SURREY", -122.7953,  data$long),
              lat = ifelse(input$neighbourhood_names == "SURREY", 49.10714,  data$lat),
              zoom = ifelse(input$neighbourhood_names == "SURREY", 11, 12))
  })
  
  saved_camId <- reactiveVal(isolate(input$camid))
  update <- reactiveVal(TRUE)
  
  # current selected camera
  current_cam <- reactiveValues(id = NULL, data = NULL, lat = NULL, lng = NULL)
  selected_cams <- reactiveValues(ids = c(), data = c())
  
  # AM and PM's same-intersection/nearest-neighbor camera comparison results
  paired_am_res <- reactiveValues()
  paired_pm_res <- reactiveValues()
  
  # current map center
  map_view <- reactiveValues()
  
  # highlight current selected cam marker
  proxy <- leafletProxy('basemap')
  
  observeEvent(input$amHour, {
    updateSliderInput(session, "timeRange", value = c(as.POSIXct("2020-12-01 07:00:00"), as.POSIXct("2020-12-01 10:00:00")), timeFormat = "%T")
  })
  
  observeEvent(input$pmHour, {
    updateSliderInput(session, "timeRange", value = c(as.POSIXct("2020-12-01 16:00:00"), as.POSIXct("2020-12-01 19:00:00")), timeFormat = "%T")
  })
  
  # Update current camera according to selected input
  observeEvent(input$camid, {
    if (update()) saved_camId(input$camid) else update(TRUE)
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
    
    if (marker$id %in% selected_cams$ids) {
      selected_cams$ids <- selected_cams$ids[selected_cams$ids != marker$id]
      proxy %>%
        addMarkers(popup=as.character(marker$id),
                   layerId = as.character(marker$id),
                   lng=marker$lng,
                   lat=marker$lat,
                   icon = camIcon)
      if (length(selected_cams$ids) > 0) {
        current_cam$id <- selected_cams$ids[1]
      } else {
        current_cam$id <- NULL
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
    # Don't trigger camid select input's reactive event
    update(FALSE)
    updateSelectInput(session, 'camid', selected = current_cam$id)
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
    
    # print(selected_cams$ids)
    if (current_cam$id %in% selected_cams$ids) {
      proxy %>%
        addAwesomeMarkers(popup=as.character(current_cam$id),
                          layerId = as.character(current_cam$id),
                          lng=current_cam$lng,
                          lat=current_cam$lat,
                          icon = cam_icon_highlight)
    }
    # print(selected_cams$ids)
  })
  
  # Update data to display
  observeEvent(selected_cams$ids, {selected_cams$data <- cams_data %>% filter(station %in% selected_cams$ids)})
  
  # Update line plot             
  observe({
    output$linePlotVehicleCounts <- renderPlot({
      plotVehicleCountWithTime(selected_cams$data, 
                               as.POSIXct(format(input$dateRange, "%Y-%m-%d")),
                               input$timeRange,
                               input$vehicleType,
                               input$weekdayOnly,
                               input$displayCorrection)
    })
    output$ampmPairTable <- DT::renderDataTable({
      paired_am_res <- pair_analyze_am(selected_cams, as.POSIXct(format(input$dateRange, "%Y-%m-%d")), input$weekdayOnly)
      paired_pm_res <- pair_analyze_pm(selected_cams, as.POSIXct(format(input$dateRange, "%Y-%m-%d")), input$weekdayOnly)
      return(DT::datatable(rbind(data.frame(paired_am_res), data.frame(paired_pm_res)), rownames = FALSE, options = list(dom = 't', autoWidth = FALSE, scrollX = TRUE)))
    })
  })
  
  # Camera Image
  output$image <- renderUI({
    img_URI = NULL
    if (!is.null(current_cam$id) & input$displayImage) {
      # TODO: can be optimize to cache URI when switching displayImage
      img_URI = fetchRealtimeImg(station = current_cam$id)
    }
    if (!is.null(img_URI)) {
      tags$div(tags$img(src = img_URI, width = 300))
    }
  })
  #heatmap values based on user's input 
  filtered_hm <- reactive({
    filter_DT(heatmap_data,input$heatTime,as.POSIXct(format(input$dateRange, "%Y-%m-%d")))
    
  })
  
  #heatmap
  observeEvent({
    input$heatTime
    input$heatDate},
    {                    
      hmdff <- filtered_hm()  #
      leafletProxy("heatmap", data = hmdff) %>%
        clearHeatmap() %>%
        addHeatmap(
          lng = ~hmdff$longitude,
          lat = ~hmdff$latitude,
          max =15,
          radius = 5,
          blur = 3,
          intensity = ~hmdff$number_instances,
          gradient = "OrRd")
    })
}

shinyApp(ui = ui, server = server)