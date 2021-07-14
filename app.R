library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)

# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")
bizs<-read.csv('data/all_businessess.csv')%>%
  st_as_sf(coords = c("lon_pc", "lat_pc"), crs = 4326)

######Sorting Business Data By Sector #####
stores<- bizs%>%
  filter(LicenceDescription %in% c("Retail Merchant - 0 to 2 Employees",
                                   "Retail Merchant - 10 to 19 Employees",
                                   "Retail Merchant - 20 or More Employees",
                                   "Retail Merchant - 3 to 5 Employees",
                                   "Retail Merchant - 6 to 9 Employees",
                                   "Wholesale",
                                   "Flea Market"))



food.and.restaurant<-bizs%>%
  filter(LicenceDescription %in% c("Restaurant-No Alcohol",
                                   "Food Primary-Class B Dining Lounge",
                                   "Food Primary-Class B Dining Room",
                                   "Farm Produce Sales",
                                   "Bakery",
                                   "Bread & Breakfast",
                                   "Caterer",
                                   "Concession Stand"))

alcohol<-bizs%>%
  filter(LicenceDescription %in% c("Liquor Licensee Retail Store",
                                   "Liquor Primary-Class C Cabaret",
                                   "Liquor Primary-Class D Neighbourhood Pub",
                                   "Liquor Primary-Class E Stadium",
                                   "Liquor Primary-Class F Marine Pub",
                                   "Liquor Primary- Class A Pub"))

health_medicine<-bizs%>%
  filter(LicenceDescription %in% c("Medical Laboratory",
                                   "Methadone Dispensary",
                                   "Nursery",
                                   "Professional Practitioner-Chiropractor",
                                   "Professional Practitioner-Dentist",
                                   "Professional Practitioner-Medical",
                                   "Professional Practitioner-Optometrist",
                                   "Professional Practitioner-Psychiatry",
                                   "Professional Practitioner-Veterinarian",
                                   "Counselling Service",
                                   "Dental Lab",
                                   "Denture Clinic",
                                   "Health Care Consultant",
                                   "Part Time Medical Practitioner",
                                   "Fitness Personal Trainer"))

finances<-bizs%>%
  filter(LicenceDescription %in% c("Financial Agent",
                                   "Financial Planning/Consultant",
                                   "Pawn Broker",
                                   "Income Tax Service/Buyer",
                                   "Investment Services",
                                   "Investment Consultant",
                                   "Consultant",
                                   "Sales/Marketing Office",
                                   "Bank",
                                   "Bankruptcy Trustee",
                                   "Project Management",
                                   "Planning Consultant",
                                   "Collection Agent",
                                   "Cheque Cashing Centre",
                                   "Customs Broker",
                                   "Currency Exchange",
                                   "Business Services Office"))

`%notin%` <- purrr::negate(`%in%`)
services<-bizs%>%
  filter(LicenceDescription %notin% c("Liquor Licensee Retail Store",
                                      "Liquor Primary-Class C Cabaret",
                                      "Liquor Primary-Class D Neighbourhood Pub",
                                      "Liquor Primary-Class E Stadium",
                                      "Liquor Primary-Class F Marine Pub",
                                      "Liquor Primary - Class A Pub",
                                      "Restaurant - No Alcohol",
                                      "Food Primary-Class B Dining Lounge",
                                      "Food Primary-Class B Dining Room",
                                      "Farm Produce Sales",
                                      "Bakery",
                                      "Bread & Breakfast",
                                      "Caterer",
                                      "Concession Stand",
                                      "Medical Laboratory",
                                      "Methadone Dispensary",
                                      "Nursery",
                                      "Professional Practitioner-Chiropractor",
                                      "Professional Practitioner-Dentist",
                                      "Professional Practitioner-Medical",
                                      "Professional Practitioner-Optometrist",
                                      "Professional Practitioner-Psychiatry",
                                      "Professional Practitioner-Veterinarian",
                                      "Counselling Service",
                                      "Dental Lab",
                                      "Denture Clinic",
                                      "Health Care Consultant",
                                      "Part Time Medical Practitioner",
                                      "Financial Agent",
                                      "Financial Planning/Consultant",
                                      "Pawn Broker",
                                      "Income Tax Service/Buyer",
                                      "Investment Services",
                                      "Investment Consultant",
                                      "Consultant",
                                      "Sales/Marketing Office",
                                      "Bank",
                                      "Bankruptcy Trustee",
                                      "Project Management",
                                      "Planning Consultant",
                                      "Collection Agent",
                                      "Cheque Cashing Centre",
                                      "Customs Broker",
                                      "Currency Exchange",
                                      "Business Services Office",
                                      "Retail Merchant - 0 to 2 Employees",
                                      "Retail Merchant - 10 to 19 Employees",
                                      "Retail Merchant - 20 or More Employees",
                                      "Retail Merchant - 3 to 5 Employees",
                                      "Retail Merchant - 6 to 9 Employees",
                                      "Wholesale",
                                      "Flea Market"))

# Icon assets
camIcon <- makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/000000/camera--v1.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10
)
restaurantIcon<-makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/restaurant.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)
storeIcon<-makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/buy--v1.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)
liquorIcon<-makeIcon(
  iconUrl = "https://img.icons8.com/cotton/64/4a90e2/liquor-shelf--v1.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)

healthIcon<-makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/stethoscope.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)

bizIcon<-makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/sell-property.png",
  iconWidth = 20, iconHeight= 20,
  iconAnchorX = 10, iconAnchorY = 10
)

serviceIcon<-makeIcon(
  iconUrl = "https://img.icons8.com/plasticine/100/4a90e2/service.png",
  iconWidth = 20, iconHeight= 20,
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
  addMarkers(data=stores, icon= storeIcon,group="Stores",clusterOptions = markerClusterOptions(maxClusterRadius = 30,showCoverageOnHover = FALSE))%>%
  addMarkers(data=food.and.restaurant, icon=restaurantIcon, group= "Food and Restaurants")%>%
  addMarkers(data=alcohol, icon= liquorIcon, group= "Liquor Stores")%>%
  addMarkers(data=health_medicine, icon= healthIcon, group= "Health and Medicine")%>%
  addMarkers(data=finances,icon= bizIcon, group="Business and Finance")%>%
  addMarkers(data=services,icon= serviceIcon,group= "Services",
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
