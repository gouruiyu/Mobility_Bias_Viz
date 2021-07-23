library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)

read.surreyBoundaries<-function(path){
  df <- st_read(path, quiet = TRUE) %>%
    st_transform(crs = 4326) %>%
    select(NAME,geometry)%>%
    mutate(lat=c(49.19029449763036,
                 49.12504880273621,
                 49.193749825907005,
                 49.15,
                 49.05998794267178,
                 49.16512240696166,
                 49.190888730841664,
                 49.113518390901305))%>%
    mutate(long= c(-122.84450893861613,
                   -122.86468748038969,
                   -122.84749467762953,
                   -122.8,
                   -122.79820675637552,
                   -122.79253629367624,
                   -122.7996948131293,
                   -122.75001299344743))
}