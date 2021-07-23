library(htmltools)

BIKE_COLOR_LEGEND=c('#A6CEE3',"#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F")

BIKE_LABELS=c("Bike Lanes","Boulevard Multi-Use Pathway",
         "Boulevard Separated Multi-Use Pathway",
         "Cycling-Permitted Sidewalk",
         "Neighbourhood Bike Route",
         "Protected Bike Lanes",
         "Shared Traffic")
palBike<-colorFactor(palette="Paired", levels=c("Bike Lanes","Boulevard Multi-Use Pathway",
                                            "Boulevard Separated Multi-Use Pathway",
                                            "Cycling-Permitted Sidewalk",
                                            "Neighbourhood Bike Route",
                                            "Protected Bike Lanes",
                                            "Shared Traffic"))

adjust_bike_legend<-browsable(
  tagList(
    list(
      tags$head(
        tags$style(
          ".leaflet .legend {
                 line-height: 10px;
                 font-size: 10px;
                 }",
          ".leaflet .legend i{
                width: 10px;
                height: 10px;
                 }"
        )
      ),
      data)))