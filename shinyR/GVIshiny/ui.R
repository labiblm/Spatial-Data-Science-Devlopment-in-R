library(raster)
library(rgdal)
library(mapview)
library(leaflet)
library(leafem)
library(shiny)


r <- raster("Data/gvi-test-4.tif")

pal <- colorNumeric(c("Greens"), values(r),
                    na.color = "transparent")


ui <- fluidPage(
  titlePanel("Green Visibility Index"),
  
  sidebarPanel("This is a pilot test for the study area, for which GVI index has been estimated using Viewshed approch
               The GVI index value could be seen on the map with mouse click at any point within the study area"),
  
  mainPanel(leafletOutput("map")),
  
  
  absolutePanel(top = 300, left = 100, width = 150, draggable = TRUE,
                selectInput("bmap", "Base map tile provider", choices = c("Stamen.Toner", 
                                                                          "CartoDB.Positron",
                                                                          "Esri.WorldImagery",
                                                                          "OpenStreetMap",
                                                                          "Stamen.Watercolor"), 
                            selected = "OpenStreetMap"),
                actionButton("update", "Update Base Map"))
  
  
  )