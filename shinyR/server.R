server <- function(input, output){
  output$map <- renderLeaflet({
    leaflet() %>% 
      addRasterImage(r, layerId = "values") %>% 
      addLegend(pal = pal, values = values(r)
                addMouseCoordinates() %>%
                  addImageQuery(r, type="mousemove", layerId = "values")
  })
}