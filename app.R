library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(hms)

source("biz_data_clean.R")
source("heatmap_function.R")

# Feature toggle
MULTI_SELECT_TOGGLE = TRUE

# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")
heatmap_data<-render.daily(cams_data,cams)

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

########## UI ##########

basemap <- leaflet(data = cams, options = leafletOptions(minZoom = ZOOM_MIN, maxZoom = ZOOM_MAX)) %>%
  setView(lng = SURREY_LNG, lat = SURREY_LAT, zoom = (ZOOM_MIN+ZOOM_MAX)/2) %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Stores",
                      "Food and Restaurants",
                      "Liquor Stores",
                      "Health and Medicine",
                      "Business and Finance",
                      "Services"),
    baseGroups = c("Cameras","Heatmap"),
    options=layersControlOptions(collapsed = TRUE))%>%
  hideGroup(c("Stores",
              "Food and Restaurants",
              "Liquor Stores",
              "Health and Medicine",
              "Business and Finance",
              "Services"))%>%
  addMarkers(~longitude, ~latitude, layerId = ~as.character(station_name), popup = ~as.character(station_name), icon = camIcon, group="Cameras") %>%
  addMarkers(data=stores, popup = ~as.character(BusinessName),icon= storeIcon,group="Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=food.and.restaurant,popup = ~as.character(BusinessName), icon=restaurantIcon, group= "Food and Restaurants",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=alcohol,popup = ~as.character(BusinessName), icon= liquorIcon, group= "Liquor Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=health_medicine, popup = ~as.character(BusinessName),icon= healthIcon, group= "Health and Medicine",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=finances,popup = ~as.character(BusinessName),icon= bizIcon, group="Business and Finance",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=services,popup = ~as.character(BusinessName),icon= serviceIcon,group= "Services",
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))

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

#fxn for heatmap input
filter_DT<-function(df,timeRange,dateRange,vehicleType){
  
  df <- pivot_longer(
    df,
    cols = c(
      "bicycle_count",
      "bus_count",
      "car_count",
      "motorcycle_count",
      "person_count",
      "truck_count"
    ),
    names_to = "vehicle_type",
    values_to = "number_instances"
  )
  
  hourRange <- as_hms(with_tz(timeRange, "America/Vancouver"))
  df$time <- as.POSIXct(df$time)
  df$date <- as.POSIXct(format(df$time, "%Y-%m-%d"))
  df <- df %>% 
    filter(date %within% interval(dateRange[1], dateRange[2])) %>%
    filter(as_hms(time) >= hourRange[1] & as_hms(time) <= hourRange[2])%>%
    filter(vehicle_type==vehicleType)
}



#UI 

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
          value = c(as.POSIXct("2020-12-01 00:00:00"), as.POSIXct("2020-12-01 01:00:00")),
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
  current_cam <- reactiveValues(id = NULL, data = NULL, lat = NULL, lng = NULL)
  selected_cams <- reactiveValues(ids = c(), data = c())
  
  # current map center
  map_view <- reactiveValues()

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
    selected_cams$ids <- unique(c(current_cam$id, selected_cams$ids))
  })
  
  # Update current camera according to marker click
  observeEvent(input$basemap_marker_click, {
    marker <- input$basemap_marker_click
    if (is.null(marker$id)) return()
    current_cam$id <- marker$id


    # Leave out the following update to prevent double triggering event
    # isolate({
    #   updateSelectInput(session, 'camid', selected = marker$id)
    # })
    
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
      selected_cams$ids <- unique(c(marker$id, selected_cams$ids))
      proxy %>%
        addAwesomeMarkers(popup=as.character(current_cam$id),
                          layerId = as.character(current_cam$id),
                          lng=current_cam$lng,
                          lat=current_cam$lat,
                          icon = cam_icon_highlight)
    }
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
  output$testImg <- renderImage({
    filename <- normalizePath(file.path('data/example_cam.jpg'))
    # Return a list containing the filename and alt text
    list(src = filename, alt = "Camera Image", width = 300)
  }, deleteFile = FALSE)
  
  
  #heatmap values based on user's input 
  filtered_hm <- reactive({
    filter_DT(heatmap_data,input$timeRange,as.POSIXct(format(input$dateRange, "%Y-%m-%d")),
              input$vehicleType)

  })
  
  #heatmap
  observeEvent({
    input$timeRange
    input$dateRange
    input$vehicleType},
    {                    
      hmdff <- filtered_hm()  #
        leafletProxy("basemap", data = hmdff) %>%
        clearHeatmap() %>%
        addHeatmap(
          lng = ~hmdff$longitude,
          lat = ~hmdff$latitude,
          max = max(hmdff$number_instances),
          radius = 5,
          blur = 3,
          intensity = ~hmdff$number_instances,
          group="Heatmap",
          gradient = "OrRd")
    })
  
  
  
  
}




shinyApp(ui = ui, server = server)
