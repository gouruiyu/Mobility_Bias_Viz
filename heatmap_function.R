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

## render data for heatmap

render.daily <- function(surrey_data, surrey_desc) {
  merged_df <-
    merge(surrey_data, surrey_desc, by.x = 'station', by.y = 'station_name') %>%
    mutate(time = as.POSIXct(time),
           time = with_tz(time, tzone = "America/Los_Angeles"))
  
  breaks_qt <-
    classIntervals(
      merged_df$car_count,
      n = 4,
      style = "fixed",
      fixedBreaks = c(-1, 5, 10, 15, c(max(
        merged_df$car_count))),
      intervalClosure = c("right")
    )
  
  merged_df <-
    mutate(merged_df,
           car_count_cat = cut(
             car_count,
             breaks = breaks_qt$brks,
             labels = c("No traffic", "Low traffic", "Medium traffic", "Busy traffic")
           ))
  return(merged_df)
  
}
