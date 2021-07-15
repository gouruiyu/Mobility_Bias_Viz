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


render_heatmap <- function(data) {
  dat1<-read_csv(data)
  dat2<-read_csv("data/Surrey_desc.csv")
  to_merge<-merge(dat1,dat2,by.x="station",by.y="station_name")
  to_merge2<-to_merge
  
  surrey_avg_df<-to_merge%>% #take the mean of counts
    group_by(station)%>%
    summarize(
      car_count=mean(car_count),
      bicycle_count=mean(bicycle_count),
      bus_count=mean(bus_count),
      truck_count=mean(truck_count),
      motorcycle_count=mean(motorcycle_count),
      person_count=mean(person_count)
      
    )
  merged.df<-merge(to_merge2,surrey_avg_df, by.x= 'station', by.y='station')%>%
    select(-car_count.x, -bicycle_count.x, -bus_count.x, -truck_count.x, -motorcycle_count.x,
           -person_count.x)%>%
    rename(car_count=car_count.y)%>%
    mutate(car_count=round(car_count))%>%
    mutate(hours=as.POSIXct(time))%>%
    mutate(date= as.Date(date, "%m/%d/%Y"))%>%
    mutate(get_hours=format(as.POSIXct(hours), "%H:%M:%S"))%>%
    mutate(get_hours=as.hms(get_hours))%>%
    mutate(get_hours=format(as.POSIXct(get_hours), "%H:%M:%S")) #merging and formatting hours
  
  merged.df<-mutate(merged.df,time=with_tz(merged.df$time,tzone="America/Los_Angeles"))
  
  
  breaks_qt <- classIntervals(merged.df$car_count, n = 4, style = "fixed", fixedBreaks=c(-1, 0, 3, 5, c(max(merged.df$car_count))), intervalClosure = c("right"))
  
  merged.df <- mutate(merged.df, car_count_cat = cut(car_count, breaks=breaks_qt$brks, labels = c("No traffic", "Low traffic", "Medium traffic", "Busy traffic")))
  
  basemap <- leaflet(data = merged.df, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>%
    setView(lng = -122.8, lat = 49.15, zoom = 12) %>%
    addProviderTiles(providers$CartoDB.Positron)%>%
    addHeatmap(lng = merged.df$longitude,lat=merged.df$latitude, intensity= merged.df$car_count_cat, max=10,radius=3,blur=9, gradient= "OrRd")
  
  return(basemap)
  
  
  
}

##############Render Data##############

render_data <- function(data) {
  dat1<-read_csv(data)
  dat2<-read_csv("data/Surrey_desc.csv")
  to_merge<-merge(dat1,dat2,by.x="station",by.y="station_name")
  to_merge2<-to_merge
  
  surrey_avg_df<-to_merge%>% #take the mean of counts
    group_by(station)%>%
    summarize(
      car_count=mean(car_count),
      bicycle_count=mean(bicycle_count),
      bus_count=mean(bus_count),
      truck_count=mean(truck_count),
      motorcycle_count=mean(motorcycle_count),
      person_count=mean(person_count)
      
    )
  merged.df<-merge(to_merge2,surrey_avg_df, by.x= 'station', by.y='station')%>%
    select(-car_count.x, -bicycle_count.x, -bus_count.x, -truck_count.x, -motorcycle_count.x,
           -person_count.x)%>%
    rename(car_count=car_count.y)%>%
    mutate(car_count=round(car_count))%>%
    mutate(hours=as.POSIXct(time))%>%
    mutate(date= as.Date(date, "%m/%d/%Y"))%>%
    mutate(get_hours=format(as.POSIXct(hours), "%H:%M:%S"))%>%
    mutate(get_hours=as.hms(get_hours))%>%
    mutate(get_hours=format(as.POSIXct(get_hours), "%H:%M:%S")) #merging and formatting hours
  
  merged.df<-mutate(merged.df,time=with_tz(merged.df$time,tzone="America/Los_Angeles"))
  
  
  breaks_qt <- classIntervals(merged.df$car_count, n = 4, style = "fixed", fixedBreaks=c(-1, 0, 3, 5, c(max(merged.df$car_count))), intervalClosure = c("right"))
  
  merged.df <- mutate(merged.df, car_count_cat = cut(car_count, breaks=breaks_qt$brks, labels = c("No traffic", "Low traffic", "Medium traffic", "Busy traffic")))
  
  
  return(merged.df)
  
  
  
}

rendered_data<-render_data('data/Surrey_data.csv')