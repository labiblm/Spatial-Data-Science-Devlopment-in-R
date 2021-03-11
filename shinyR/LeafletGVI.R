library(raster)
library(rgdal)
library(leaflet)

r <- raster("F:/Data/gvi-test-4.tif")

pal <- colorNumeric(c("Greens"), values(r),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(r, colors = pal, opacity = 0.6) %>%
  addLegend(pal = pal, values = values(r),
            title = "GVI")
  addMouseCoordinates() %>%
  addImageQuery(r, type="mousemove", layerId = "values")