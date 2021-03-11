library(raster)
library(mapview)
library(leaflet)
library(shiny)

f <- system.file("F:/Data/gvi-test-4.tif", package="raster")
r <- raster(f)
pal <- colorNumeric(c("Greens"), values(r),
                    na.color = "transparent")

ui <- fluidPage(
  leafletOutput("map", width = "100%", height = 400)
)

server <- function(input, output){
  output$map <- renderLeaflet({
    leaflet() %>% 
      addRasterImage(r, layerId = "values") %>% 
      addLegend(pal = pal, values = values(r)
                addMouseCoordinates() %>%
                  addImageQuery(r, type="mousemove", layerId = "values")
  })
}

shinyApp(ui, server)