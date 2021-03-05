
rm (list = ls())

library(osmdata)
library(osmextract)
library(sf)
library(rgdal)
library(ggmap)

setwd("C:/Users/S M Labib/Desktop/GSV_GLASST")

dsn <- "C:/Users/S M Labib/Desktop/GSV_GLASST"


#Read the city street data

cycleways = oe_get(
  "El Paso",
  quiet = FALSE,
  query = "SELECT * FROM 'lines' WHERE highway = 'cycleway'"
)
par(mar = rep(0.1, 4))
plot(sf::st_geometry(cycleways_england))

st_write(cycleways, dsn, "CycleTexas.shp", driver = "ESRI Shapefile", overwrite = TRUE)


cycleways_city = oe_get(
  "Greater Manchester",
  quiet = FALSE,
  query = "SELECT * FROM 'multipolygons' WHERE building in ('yes', 'house')"
)
par(mar = rep(0.1, 4))
plot(sf::st_geometry(cycleways_city))

st_write(cycleways_city, dsn, "CityDataOSM2.shp", driver = "ESRI Shapefile", overwrite = TRUE)

#query = "SELECT * FROM 'multipolygons' WHERE building in ('yes', 'house')" [selected buildings]
#query = "SELECT * FROM 'multipolygons' WHERE building  IS NOT NULL" [all buildings that has any value]



POIs = oe_get(
  "Rome",
  quiet = FALSE,
  query = "SELECT * FROM 'points' WHERE other_tags IS NOT NULL"
)
par(mar = rep(0.1, 4))
plot(sf::st_geometry(POIs))

st_write(POIs, dsn, "CycleTexas.shp", driver = "ESRI Shapefile", overwrite = TRUE)


city <- "Dhaka"

q <- getbb(city) %>%
  opq() %>%
  add_osm_feature("amenity", c("bar", "pub", "restaurant"))


pois <- osmdata_sf(q)

mad_map <- get_map(getbb(city), maptype = "toner-background")

ggmap(mad_map)+
  geom_sf(data =  pois$osm_points,
          inherit.aes = FALSE,
          colour = "red",
          fill = "red",
          alpha = .5,
          size = 1,
          shape = 1)+
  labs(x = "", y = "")

POIS <- as_Spatial(pois$osm_points)

writeOGR(POIS, dsn, "Dhaka_POIs", driver="ESRI Shapefile", overwrite = TRUE)




CityRoads <- st_read("rome-2897.gpkg", layer = 'edges')

#croad <- readOGR("ElPaso/EL_Paso_streets.shp")

croad <- readOGR("rome-2897.gpkg", layer = 'edges')

#bound<- gBoundary(CityRoads, byid=FALSE, id = NULL)


worldBuiltup <- raster("WGS84GHS_BUILT_LDS2014_GLOBE_R2018A_54009_250_V2_0.tif")


City_built <- crop(worldBuiltup, CityRoads)

City_built_grids <- rasterToPolygons(City_built, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

CbuitGrid <- st_as_sf (City_built_grids)

#Citybilt_on_roads <- intersect(CityRoads, City_built_grids)


CBuitRoads<- st_intersection (CbuitGrid, CityRoads)

CBR <- raster::intersect(City_built_grids, croad)

CBRx <- intersect (CbuitGrid, CityRoads)

CBRS <- st_as_sf (CBR)

st_write(CBRS, "RomeGrids.shp", overwrite = TRUE)




q2 <- getbb("Greater Manchester") %>%
  opq() %>%
  add_osm_feature("building")

GMdata <- osmdata_sf(q2)

GBBuilding <- as_Spatial(GMdata$osm_polygons)

GBBuildingS <- st_as_sf (GBBuilding)

writeOGR(GBBuilding, dsn, layer = GBBuildingS, driver="ESRI Shapefile", overwrite = TRUE)


city <- "Hamburg"

r2 <- getbb(city) %>%
  opq() %>%
  add_osm_feature("highway", value = c("cycleway"))
  #add_osm_feature("highway", value = c("motorway", "primary", "secondary", "tertiary"))

motorway <- osmdata_sf(r2)

#roadn <- as_Spatial(motorway$osm_lines)


mad_map <- get_map(getbb(city), maptype = "toner-background")


ggmap(mad_map)+
  geom_sf(data = motorway$osm_lines,
          inherit.aes = FALSE,
          colour = "red",
          fill = "red",
          alpha = .5,
          size = 1,
          shape = 1)+
  labs(x = "", y = "")



tbuff <- buffer(roadn, width=100, dissolve=TRUE)


#function
x <- opq(bbox = c(-0.27, 51.47, -0.20, 51.50)) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'name', value = "Thames", value_exact = FALSE) %>%
  osmdata_sf()
x



