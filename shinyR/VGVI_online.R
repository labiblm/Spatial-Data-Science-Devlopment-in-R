library(raster)
library(mapview)
library(leaflet)
library(leafem)
library(shiny)


r <- raster("F:/Data/gvi-test-4.tif")

pal <- colorNumeric(c("Greens"), values(r),
                    na.color = "transparent")


ui <- fluidPage(
  titlePanel("Green Visibility Index"),
  sidebarPanel("This is a pilot test for the study area, for which GVI index has been estimated using Viewshed approch
               The GVI index value could be seen on the map with mouse click at any point within the study area"),
  mainPanel(leafletOutput("map"))
  
)

server <- function(input, output){
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addRasterImage(r, colors = pal, opacity = 0.5, layerId = "values") %>% 
      addLegend(pal = pal, values = values(r), title = "GVI") %>%
      addMouseCoordinates() %>%
      addImageQuery(r, type="click", project = TRUE, layerId = "values", position = "bottomright")
  })
}

shinyApp(ui, server)



