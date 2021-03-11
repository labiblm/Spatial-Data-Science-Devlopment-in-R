library(mapview)
library(leaflet)
library(leafem)
library(shiny)
library(rgdal)

r <- raster("Data/gvi-test-4.tif")

pal <- colorNumeric(c("Greens"), values(r),
                    na.color = "transparent")


server <- function(input, output){
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(input$bmap)%>%
      #addTiles() %>% 
      addRasterImage(r, colors = pal, opacity = 0.5, layerId = "values") %>% 
      addLegend(pal = pal, values = values(r), title = "GVI") %>%
      addMouseCoordinates() %>%
      addImageQuery(r, type="click", project = TRUE, layerId = "values", position = "bottomright")
  })
}