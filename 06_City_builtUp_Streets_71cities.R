
#################################Description########################################

# This script is developed to identify built up area along street network in 71 cities
# These cities are selected to extract 
# Google Street view data along these streets and built up areas
# This script use data from OSMnx data source and Global Built Up Layers 
# Main data sources:
# GHSL data: https://ghsl.jrc.ec.europa.eu/datasets.php
# OSMnx data: https://dataverse.harvard.edu/dataverse/global-urban-street-networks

##############################Libraries##########################################
library(sf) # v ‘0.9.6’ 
library(sp) # v ‘1.4.4’
library(rgdal) # v ‘1.5.18’
library(rgeos) 
library(raster)
library(ggmap)

##############################Set Up ##########################################

rm (list = ls())

Globalpath<- "C:/Users/S M Labib/Desktop/GSV_GLASST/"

setwd("C:/Users/S M Labib/Desktop/GSV_GLASST")

########################## Read data ##########################################

#Read the city street data
#cityboundary <- readOGR("ElPaso/elpaso-shapefile.shp")
#CityRoads <- st_read("ElPaso/EL_Paso_streets.shp")
#CityRoads <- st_read("rome-2897.gpkg", layer = 'edges')

#croad <- readOGR("ElPaso/EL_Paso_streets.shp")
croad <- readOGR("rome-2897.gpkg", layer = 'edges')

#bound<- gBoundary(CityRoads, byid=FALSE, id = NULL)

#read raster
worldBuiltup <- raster("GLOBE_BUILT.tif")


######################### Main operation ######################################


#crop the raster to city road extent
City_built <- crop(worldBuiltup, croad)

#convert the corp grids to polygons
City_built_grids <- rasterToPolygons(City_built, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

#convert the polygons as sf object
CbuitGrid <- st_as_sf (City_built_grids)


#intersect the grid polygons with road lines
CBR <- raster::intersect(City_built_grids, croad)

#convert to sf object, as the intersect create SpatialPolygonsDataFrame
#CBRS <- st_as_sf (CBR)

#save the file
#st_write(CBRS, "RomeGrids2.shp", overwrite = TRUE)
#dsn <- paste0(Globalpath, "Cities", "/", "Rome")

dsn <- Globalpath

writeOGR(CBR, "dsn", layer= "RomeGrids", driver="ESRI Shapefile", overwrite_layer = TRUE) 


citynames <- read.csv(paste0(Globalpath, "cityfolders.csv"))

for (cn in 1:length(citynames$citynames)) {
  
  city <- as.character(citynames$citynames[cn])
  
  newfolder <- city
  
  dir.create(file.path(paste0(Globalpath, "Cities"), newfolder))

}



#########the loop for all cities##########

citynames <- read.csv(paste0(Globalpath, "cityfolders_up.csv"))

for (cn in 1:length(citynames$citynames)) {
  
  city <- as.character(citynames$citynames[cn])
  
  print(city)
  
  listcityfiles <- list.files(path = paste0(Globalpath, "Cities", "/", city), pattern = "*.gpkg", full.names = TRUE, recursive = TRUE)
  
  for (fl in listcityfiles){
    ctiyroad <- readOGR(fl, layer = 'edges')
    print("success!")
  }
  
  croad <- ctiyroad
  
  City_built <- crop(worldBuiltup, croad)
  
  City_built_grids <- rasterToPolygons(City_built, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
  
  CbuitGrid <- st_as_sf (City_built_grids)
  
  CBR <- raster::intersect(City_built_grids, croad)
  
  print(CBR)
  
  #CBR <- st_intersection (CbuitGrid, croad)
  
  #CBRS <- st_as_sf (CBR)
  
  dsn <- paste0(Globalpath, "CityGrids")
  
  gridname <- paste0(city, "_", "grid")
  
  writeOGR(CBR, dsn, layer = gridname, driver="ESRI Shapefile", overwrite_layer = TRUE)
  
  
}


