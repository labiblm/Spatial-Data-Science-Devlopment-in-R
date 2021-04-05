#Working on the bus route data for londo for LTN impact analysis

rm(list=ls())

library(tidyverse)
library(rgdal)
library(sf)
library(lwgeom)
library(sp)
library(maptools)

#Busroutelondon <- read_sf("C:/Users/S M Labib/Desktop/LTN_related/Bus_BNG.shp")


Route_1_1 <- read_sf("C:/Users/S M Labib/Desktop/LTN_related/Route_1_1Proj.shp")


Rout_1_QSI <- read_sf("C:/Users/S M Labib/Desktop/LTN_related/Route1.shp")

Rout1QSIs <-as_Spatial(Rout_1_QSI)

plot(Rout_1_QSI)



Route_1_1att <- Route_1_1 %>%
  select(route, ref, id)

Route_1_1atts <- as_Spatial(Route_1_1att)

QSI_snapBus = st_snap(Rout_1_QSI, Route_1_1att,tol=1e-9)

parts = st_collection_extract(st_split(Route_1_1att$geometry, QSI_snapBus$geometry),"LINESTRING")

plot(parts)

st_distance(Route_1_1att$geometry, QSI_snapBus$geometry)

QSINearpoints <- st_nearest_points(Route_1_1att, Rout_1_QSI)


#snap to line
pointsnear <- snapPointsToLines(Rout1QSIs, Route_1_1atts, maxDist = 50)

pointsnearsf <- st_as_sf(pointsnear)

plot(pointsnearsf)

x <- st_split(Route_1_1att$geometry, pointsnearsf$geometry)

parts2 = st_collection_extract(x,type = "LINESTRING")

plot(parts2)

c <- "C:/Users/S M Labib/Desktop/busintersect"

st_write(parts2, dsn = c, 'bussplit3', driver = "ESRI Shapefile")


hin<- st_split(Route_1_1att, pointsnearsf)

hins <- as_Spatial(hin)

writeOGR(parts2, dsn = c, 'bussplit3.shp', driver = "ESRI Shapefile")

