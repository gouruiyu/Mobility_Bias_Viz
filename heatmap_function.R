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


##############Render Data##############

render_heatmap_df <- function(data) {
  dat2<-read_csv("data/Surrey_desc.csv")
  to_merge<-merge(data,dat2,by.x="station",by.y="station_name")
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

##### Rener Daily df#####


render.daily<- function(surrey_data,surrey_desc){
  
  merged_df<-merge(surrey_data,surrey_desc,by.x='station',by.y='station_name')%>%
    mutate(time=as.POSIXct(time),
           time=with_tz(time,tzone="America/Los_Angeles"))
  
  breaks_qt <- classIntervals(merged_df$car_count, n = 4, style = "fixed", fixedBreaks=c(-1, 5, 10, 15, c(max(merged_df$car_count))), intervalClosure = c("right"))
  
  merged_df <- mutate(merged_df, car_count_cat = cut(car_count, breaks=breaks_qt$brks, labels = c("No traffic", "Low traffic", "Medium traffic", "Busy traffic")))
  # merged_df<-merged_df%>%
  #   mutate(time=as.character(time))


}