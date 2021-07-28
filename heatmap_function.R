library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(hms)
library(RColorBrewer)
library(classInt)
library(xts)

## render data for heatmap

render.daily <- function(surrey_data, surrey_desc) {
  merged_df <-
    merge(surrey_data, surrey_desc, by.x = 'station', by.y = 'station_name') %>%
    mutate(time = as.POSIXct(time),
           time = with_tz(time, tzone = "America/Los_Angeles"))
  

  return(merged_df)
  
}
