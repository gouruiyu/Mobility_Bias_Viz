library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyjqui)
library(leaflet)
library(sf)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(hms)
library(rlang)
library(DT)
library(nngeo)
library(purrr)
library(TTR)
library(shinyvalidate)

source("biz_data_clean.R")
source("stats/ampm_comparison_model.R")
source("camera_img.R")
source("stats/undercount_model.R")
source("readBoundaries.R")
source("bikevars.R") #BIKE_COLOR_LEGEND, BIKE_LABLES & palBike  variables used
source('heatmap_function.R')

# Feature toggle
MULTI_SELECT_TOGGLE = TRUE

# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")
cams_sf <- cams %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
neighbourhood<-readBoundaries('data/surrey_city_boundary.json')
bike_routes <- st_read("data/bikeroutes_in_4326.geojson", quiet = TRUE) %>%
  st_transform(crs = 4326)
heatmap_df<-render.daily(cams_data,cams)
heatmap_boundaries<-st_read('data/surrey.geojson')%>%
  st_transform(crs = 4326)
hm_default.plot<-render.daily(cams_data,cams)%>%
  filter(time >= '2020-12-15 17:00:00' & time<= '2020-12-15 18:00:00')
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
K_MIN <- 0
K_MAX <- 1

########## UI ##########

basemap <- leaflet(data = cams, options = leafletOptions(minZoom = ZOOM_MIN, maxZoom = ZOOM_MAX)) %>%
  setView(lng = SURREY_LNG, lat = SURREY_LAT, zoom = (ZOOM_MIN+ZOOM_MAX)/2) %>%
  addMarkers(~longitude, ~latitude, layerId = ~as.character(station_name), label = ~as.character(station_name), icon = camIcon) %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  leaflet::addLegend("topleft",
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
                      "Bike Routes",
                      "Nearby Cams"),
    options=layersControlOptions(collapsed = TRUE))%>%
  hideGroup(c("Stores",
              "Food and Restaurants",
              "Liquor Stores",
              "Health and Medicine",
              "Business and Finance",
              "Services",
              "Bike Routes",
              "Nearby Cams")) %>%
  addMarkers(data=stores, layerId = ~CompanyID, label = ~as.character(BusinessName),icon= storeIcon,group="Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=food.and.restaurant, layerId = ~CompanyID, label = ~as.character(BusinessName), icon=restaurantIcon, group= "Food and Restaurants",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=alcohol, layerId = ~CompanyID, label = ~as.character(BusinessName), icon= liquorIcon, group= "Liquor Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=health_medicine, layerId = ~CompanyID, label = ~as.character(BusinessName),icon= healthIcon, group= "Health and Medicine",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=finances, layerId = ~CompanyID, label = ~as.character(BusinessName),icon= bizIcon, group="Business and Finance",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=services, layerId = ~CompanyID, label = ~as.character(BusinessName),icon= serviceIcon,group= "Services",
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))

baseHeatmap <- leaflet(options = leafletOptions(minZoom = ZOOM_MIN, maxZoom = ZOOM_MAX)) %>%
  setView(lng = SURREY_LNG, lat = SURREY_LAT, zoom = 10) %>%
  addProviderTiles(providers$CartoDB.DarkMatter)%>%
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
              "Bike Routes")) %>%
  addMarkers(data=stores, layerId = ~CompanyID, label = ~as.character(BusinessName),icon= storeIcon,group="Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=food.and.restaurant, layerId = ~CompanyID, label = ~as.character(BusinessName), icon=restaurantIcon, group= "Food and Restaurants",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=alcohol, layerId = ~CompanyID, label = ~as.character(BusinessName), icon= liquorIcon, group= "Liquor Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=health_medicine, layerId = ~CompanyID, label = ~as.character(BusinessName),icon= healthIcon, group= "Health and Medicine",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=finances, layerId = ~CompanyID, label = ~as.character(BusinessName),icon= bizIcon, group="Business and Finance",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=services, layerId = ~CompanyID, label = ~as.character(BusinessName),icon= serviceIcon,group= "Services",
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))%>%
  addPolygons(data=bike_routes,weight = 4, color = ~palBike(BIKE_INFRASTRUCTURE_TYPE), opacity=0.3,fill = FALSE,
              group="Bike Routes")%>%
  addHeatmap(
    data=hm_default.plot,
    lng = ~hm_default.plot$longitude,
    lat = ~hm_default.plot$latitude,
    max = max(hm_default.plot$car_count),
    radius = 5,
    blur = 3,
    intensity = ~hm_default.plot$car_count,
    gradient = "OrRd")%>%
  addPolygons(data= heatmap_boundaries,color = "#ffffff", weight = 3, smoothFactor = 0.5,
              fillOpacity = 0, opacity = 0.2)




plotVehicleCountWithTime <- function(df, dateRange, timeRange, vehicleType, weekdayOnly, kSmooth, displayCorrection=FALSE) {
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
  # Adding moving average smoother with window size k
  # df$ma <- runMean(df[, vehicleType], kSmooth)
  myplot <- ggplot(data=df, aes_string(x="time", y=vehicleType, color="station")) + 
    scale_x_datetime(date_breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M", limits = as.POSIXct(paste(dateRange, hourRange), format="%Y-%m-%d %H:%M")) +
    xlab("Time(hour)") +
    ylab(vehicleType) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="bottom")
  
  if (!displayCorrection) {
    myplot <- myplot +
      # geom_line(aes(y=ma)) + 
      geom_point() +
      geom_smooth(method = "loess", formula = "y ~ x", span = kSmooth)
  } else {
    # Bias corrected line
    pred <- predict(uc_correction_model, newdata=data.frame(detected = df[[vehicleType]]))
    # pred_ma <- runMean(pred, kSmooth)
    myplot <- myplot + 
      geom_point(data=df, aes(y=pred)) +
      geom_smooth(method = "loess", formula = "y ~ x", span = kSmooth)
      # geom_line(aes(y=pred_ma))
  }
  return(myplot)
}

ui <- dashboardPage(
  dashboardHeader(title = "Unbiased Mobility"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu( id = "sidemenu",
                 menuItem("Cam Map", tabName = "basemap", icon = icon("camera")),
                 menuItem("Heatmap (Car Count Only)", tabName='base_Heatmap',icon=icon("fire")),
                 hidden(
                   sliderInput(
                     "heatDateTime", label = "Choose Date Range:",
                     min = as.POSIXct("2020-12-01 00:00:00"),
                     max = as.POSIXct("2020-12-31 23:59:59") - hours(1),
                     value = as.POSIXct("2020-12-15 17:00:00"),
                     timeFormat = "%Y-%m-%d %H:%M", ticks = T, step=3600, animate = T
                   )),
                 menuItem("Help", tabName = "help", icon = icon("question-circle")),
                 menuItem("User Inputs", tabName = "userInputs", icon = icon("user")),
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
                 materialSwitch("displayImage", label = "Display Real-time Image", value = TRUE, width = "100%", status = "primary")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "basemap",
              div(class = "outer", 
                  tags$head(includeCSS("styles.css")),
                  leafletOutput("basemap", width = "100%", height = "100%"),
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 60, left = "auto", right = 60, bottom = "auto",
                                width = "fit-content", height = "auto",
                                box(
                                  title = "Traffic Explorer",
                                  materialSwitch("weekdayOnly", label = "Weekdays Only", status = "primary", inline = TRUE, value = FALSE),
                                  materialSwitch("displayCorrection", label = "Correct for Undercount", status = "primary", inline = TRUE, value = FALSE),
                                  jqui_resizable(plotOutput("linePlotVehicleCounts", height = "200")),
                                  numericInput(inputId = 'kSmooth',
                                            label = 'Choose your span size for smoothing((0,1]):',
                                            value = 0.75,
                                            min = K_MIN,
                                            max = K_MAX,
                                            step = 0.05,
                                            width = "300px"),
                                  collapsible = T,
                                  width = "fit-content", height = "auto"
                                )),
                  jqui_resizable(absolutePanel(id = "comparison", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 450, left = "auto", right = 60, bottom = "auto",
                                width = "300px", height = "auto",
                                box(
                                  title = "Same-intersection & Nearest-neighbor Cameras' Car Count Comparisons(AM VS PM)",
                                  DT::dataTableOutput("ampmPairTable"),
                                  collapsible = T,
                                  width = "100%", height = "auto"

                                )))
                  ),
              absolutePanel(id = "camera_img",
                            draggable = TRUE, top = "auto", left = "auto" , right = "auto", bottom = 20,
                            width = 300, height = "auto",
                            uiOutput("image", height="auto")),
              absolutePanel(id = "bizs_cams_radius",
                            draggable = TRUE, top = 55, left = "auto" , right = "auto", bottom = "auto",
                            width = 300, height = "auto",
                            dropdownButton(
                              actionButton(inputId = "recalc_radius", label = "Go"),
                              sliderInput(inputId = 'cams_radius',
                                          label = 'Radius',
                                          value = 1000,
                                          min = 0,
                                          max = 3000),
                              circle = TRUE, status = "danger",
                              icon = icon("gear"), width = "300px",
                              tooltip = tooltipOptions(title = "Specify radius for nearby cameras")
                            ),
              )
      ),
      tabItem(tabName = "help",
              includeMarkdown("help.md")),
      tabItem("base_Heatmap", div(class = "outer", leafletOutput("heatmap", width = "100%", height = "100%")))
    )
  ))

########## Server ##########

server <- function(input, output, session) {
  output$basemap <- renderLeaflet(basemap)
  output$heatmap <- renderLeaflet(baseHeatmap)
  
  # dynamically show/hide userInputs in the sidebarMenu
  observeEvent(input$sidemenu, {
    if (input$sidemenu == "userInputs" | input$sidemenu=='basemap') {
      shinyjs::hide("heatDateTime")
      shinyjs::show("neighbourhood_names")
      shinyjs::show("timeRange")
      shinyjs::show("dateRange")
      shinyjs::show("camid")
      shinyjs::show("vehicleType")
      shinyjs::show("amHour")
      shinyjs::show("pmHour")
      shinyjs::show("displayCorrection")
      shinyjs::show("displayImage")
    } 
    else if (input$sidemenu== 'help'){
      shinyjs::hide("heatDateTime")
      shinyjs::hide("neighbourhood_names")
      shinyjs::hide("timeRange")
      shinyjs::hide("dateRange")
      shinyjs::hide("camid")
      shinyjs::hide("vehicleType")
      shinyjs::hide("amHour")
      shinyjs::hide("pmHour")
      shinyjs::hide("displayCorrection")
      shinyjs::hide("displayImage")
      
    }
    else {
      shinyjs::show("heatDateTime")
      shinyjs::hide("neighbourhood_names")
      shinyjs::hide("timeRange")
      shinyjs::hide("dateRange")
      shinyjs::hide("camid")
      shinyjs::hide("vehicleType")
      shinyjs::hide("amHour")
      shinyjs::hide("pmHour")
      shinyjs::hide("displayCorrection")
      shinyjs::hide("displayImage")
      
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
  
  iv <- InputValidator$new()
  iv$add_rule("kSmooth", sv_between(K_MIN, K_MAX, inclusive = c(FALSE, TRUE)))
  iv$enable()
  kSmooth <- reactiveVal(isolate(input$kSmooth))
  
  # current selected business
  current_biz <- reactiveValues(id = NULL, data = NULL, lat = NULL, lng = NULL)
  
  # current selected camera
  current_cam <- reactiveValues(id = NULL, data = NULL, lat = NULL, lng = NULL)
  selected_cams <- reactiveValues(ids = c(), data = c())
  nn_cameras <- reactiveValues(ids = c(), data = c())
  
  # AM and PM's same-intersection/nearest-neighbor camera comparison results
  paired_am_res <- reactiveValues()
  paired_pm_res <- reactiveValues()
  
  # current map center
  map_view <- reactiveValues()
  
  # highlight current selected cam marker
  proxy <- leafletProxy('basemap')
  proxy_business <- leafletProxy('basemap')
  
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
  
  observeEvent(input$recalc_radius, {
    if (is.null(current_biz$data)) return()
    nn_cams <- st_nn(current_biz$data, cams_sf, k = 3, maxdist = input$cams_radius)
    nn_cams <- lapply(nn_cams, function(x) cams_sf[x,])
    nn_cams_df <- ldply(nn_cams, data.frame)
    nn_cams_df <- nn_cams_df %>%
      mutate(lng = unlist(map(nn_cams_df$geometry, 1)),
             lat = unlist(map(nn_cams_df$geometry, 2)))
    
    req(nn_cams_df)
    nn_cameras$ids <- nn_cams_df$station_name
    
    proxy_business %>%
      clearGroup("Nearby Cams")
    if (nrow(nn_cams_df) == 0) {
      return()
    }
    proxy_business %>%
      addCircles(group = "Nearby Cams",
                 lng = current_biz$lng,
                 lat = current_biz$lat,
                 radius = input$cams_radius) %>%
      addAwesomeMarkers(data = nn_cams_df, lat = ~lat, lng = ~lng, group = "Nearby Cams")
  })
  
  # Update current camera according to marker click
  observeEvent(input$basemap_marker_click, {
    marker <- input$basemap_marker_click
    if (is.null(marker$id)) return()
    
    # On selecting a business marker
    if (marker$id %in% bizs_ids) {
      current_biz$id <- marker$id
      current_biz$data <- bizs %>% filter(CompanyID == current_biz$id)
      current_biz$lat <- marker$lat
      current_biz$lng <- marker$lng
      nn_cams <- st_nn(current_biz$data, cams_sf, k = 3, maxdist = input$cams_radius)
      nn_cams <- lapply(nn_cams, function(x) cams_sf[x,])
      nn_cams_df <- ldply(nn_cams, data.frame)
      nn_cams_df <- nn_cams_df %>%
        mutate(lng = unlist(map(nn_cams_df$geometry, 1)),
               lat = unlist(map(nn_cams_df$geometry, 2)))
      
      req(nn_cams_df)
      nn_cameras$ids <- nn_cams_df$station_name
      
      if (nrow(nn_cams_df) == 0) return()
      proxy_business %>%
        clearGroup("Nearby Cams") %>%
        addCircles(group = "Nearby Cams",
                   lng = marker$lng,
                   lat = marker$lat,
                   radius = input$cams_radius) %>%
        addAwesomeMarkers(data = nn_cams_df, lat = ~lat, lng = ~lng, group = "Nearby Cams")
      return()
    }
    
    current_cam$id <- marker$id
    
    if (marker$id %in% selected_cams$ids) {
      selected_cams$ids <- selected_cams$ids[selected_cams$ids != marker$id]
      proxy %>%
        addMarkers(label=as.character(marker$id),
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
        addAwesomeMarkers(label=as.character(current_cam$id),
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
        addAwesomeMarkers(label=as.character(current_cam$id),
                          layerId = as.character(current_cam$id),
                          lng=current_cam$lng,
                          lat=current_cam$lat,
                          icon = cam_icon_highlight)
    }
    # print(selected_cams$ids)
  })
  
  # Update data to display
  observeEvent(selected_cams$ids, {selected_cams$data <- cams_data %>% filter(station %in% selected_cams$ids)})
  
  # Tracking previously selected groups
  prev_selected_groups <- reactiveVal(NULL)
  # Toggle on/off the line plot for Nearby Cams
  show_nn_cameras <- reactiveVal(FALSE)
  
  # Layer Control for Nearby Cams
  observeEvent(input$basemap_groups, {
    selected_groups <- req(input$basemap_groups)

    # print(is.null(prev_selected_groups()))
    # print(prev_selected_groups())
    # print(selected_groups)

    if (!is.null(prev_selected_groups())) {
      if (length(selected_groups) > 1 && "Nearby Cams" %in% selected_groups && (prev_selected_groups() == c("Nearby Cams") | "Nearby Cams" %notin% prev_selected_groups())) {
        print("Nearby Cams" %in% selected_groups)
        show_nn_cameras(TRUE)
        prev_selected_groups(selected_groups)
        return()
      }
    }
    print("Nearby Cams" %in% selected_groups)
    proxy_business %>%
      clearGroup("Nearby Cams")
    nn_cameras$ids <- NULL
    show_nn_cameras(FALSE)
    prev_selected_groups(selected_groups)
  })
  
  # Update line plot             
  observeEvent(input$kSmooth, {
    if (!is.na(input$kSmooth) & input$kSmooth <= K_MAX & input$kSmooth > K_MIN) {
      kSmooth(input$kSmooth)
    }
    output$linePlotVehicleCounts <- renderPlot({
      if (show_nn_cameras() & !is.null(nn_cameras$ids)) {
        nn_cameras$data <- cams_data %>% filter(station %in% nn_cameras$ids)
        plotVehicleCountWithTime(nn_cameras$data,
                                 as.POSIXct(format(input$dateRange, "%Y-%m-%d")),
                                 input$timeRange,
                                 input$vehicleType,
                                 input$weekdayOnly,
                                 kSmooth(),
                                 input$displayCorrection)
      } else {
        plotVehicleCountWithTime(selected_cams$data, 
                                 as.POSIXct(format(input$dateRange, "%Y-%m-%d")),
                                 input$timeRange,
                                 input$vehicleType,
                                 input$weekdayOnly,
                                 kSmooth(),
                                 input$displayCorrection)
      }
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
    heatmap_df[heatmap_df$time >= input$heatDateTime[1] & heatmap_df$time <= input$heatDateTime[1] + hours(1), ]
  })
  
  #heatmap
  observeEvent(input$heatDateTime,
               {
                 hmdff <- filtered_hm()
                 leafletProxy("heatmap", data = hmdff) %>%
                   clearHeatmap() %>%
                   addHeatmap(
                     lng = ~hmdff$longitude,
                     lat = ~hmdff$latitude,
                     max = max(hmdff$car_count),
                     radius = 5,
                     blur = 3,
                     intensity = ~hmdff$car_count,
                     gradient = "OrRd")
               })
  
  
  
}

shinyApp(ui = ui, server = server)
