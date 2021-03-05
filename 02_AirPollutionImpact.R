
########### Import libraries, give gobal variables##########

rm(list=ls())

library(plyr)
library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(sp)

#Global path to the folders data stored
Globalpath<- "C:/Users/S M Labib/Desktop/METAHIT/mh-air-pollution/new_air_impact/_LASpecific/"

#setwd(Globalpath)

#Names of the LA folder
LAlist <- read.csv(paste0(Globalpath, "lafolders.csv")) 

LAlistF <- read.csv("../mh-execute/inputs/mh_regions_lad_lookup.csv")

LAwithCR <- left_join(LAlist, LAlistF, by = c("LANames" = "lad11nm"))

PRJc <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

############ if zipped files and folders #################################
#list all the files in a directory
zipF <- list.files(path = Globalpath, pattern = "*.zip", full.names = TRUE)
outDir<- Globalpath
# unzip the zipped files
ldply(.data = zipF, .fun = unzip, exdir = outDir)

############# local and non-local impact factor for each LA ###############

for (m in 1:length(LAlist$LANames)) {
  
  lahome <- as.character(LAlist$LANames[m])
  
  print(lahome)
  
  ascfilela <- list.files(path = paste0(Globalpath, lahome),pattern = ".asc$",full.names = TRUE, recursive = TRUE)
  
  #check the asc files listed has the same index number used in the following function
  print(ascfilela)
  
  LAtifStack <- stack(ascfilela)
  
  fun0 <- function(x) {x[2] + x[4]} #sum of NOx across agglomeration in each cell, the R0 files for petrol and diesel car
  AggNOxR0 <- calc(LAtifStack, fun0)
  crs(AggNOxR0) <- PRJc #project to BNG
  
  #local impact factor for in-square VKM and emission of NOx
  #The index numbers are layer index in the stack, here: 
  #x[1] is NOX_DieselCars_InSqConc.asc
  #x[3] is NOX_PetrolCars_InSqConc.asc
  #x[13] is s46_vkm_2020.asc
  fun1 <- function(x) {(x[1] + x[3])/((x[13] - x[14]) + x[15])} 
  LifNx <- calc(LAtifStack, fun1)
  crs(LifNx) <- PRJc
  print(LifNx)
  
  #non local authority LA impact factor
  #for total vkm without motor cycle
  fun2 <- function(x) {sum((x[13] - x[14]) + x[15])}
  VKMtotal <- calc (LAtifStack, fun2) #calculate both Petrol and Diesel VKM minus motor cycle vkm
  crs(VKMtotal) <- PRJc
  vkmvect <- sum(as.vector(VKMtotal)) #get the LA specific total vkm

  #function to estimate non local IF for each LA
  fun3 <- function(x) {((x[2] + x[4]) - (x[1] + x[3]))/((vkmvect - (x[13] - x[14]) + x[15]))} 
  #non local impact factor for NOx for each la
  NonLifNx <- calc (LAtifStack, fun3)
  crs(NonLifNx) <- PRJc
  
  
  
  #save the combined of R0 layer for each LA
  writeRaster(AggNOxR0, filename= file.path(Globalpath, lahome, "AggNOxR0.tif"), format="GTiff", overwrite=TRUE)
  
  #save the combined of VKM layer for each LA
  writeRaster(VKMtotal, filename= file.path(Globalpath, lahome, "VKMtotal_2020.tif"), format="GTiff", overwrite=TRUE)
  
  #save the impact factors
  #save the local impact factor as tif file
  writeRaster(LifNx, filename= file.path(Globalpath, lahome, "LIFNx.tif"), format="GTiff", overwrite=TRUE)
  
  #save the non local impact factor as tif file
  writeRaster(NonLifNx, filename= file.path(Globalpath, lahome, "NonLIFNx.tif"), format="GTiff", overwrite=TRUE)
  
  #empty the list for the next LA
  ascfilela <- NA

}


##################### changed concentration for all LAs #####################################

#Hypothetical change in concentration by LAs
LAwithCR_cocen <- LAwithCR %>%
  mutate(conchange = 0.9) #10% reduction in concentration
  #mutate(conchange = runif(84, 0.05, 0.5)) #create random concentration change for each LA. 
#In the main calculation this would come from scenarios

#LAwithCR_cocen2 <- LAwithCR %>%
  #mutate(conchange = runif(84, 0.05, 0.5)) #create random concentration change for each LA5

#loop in each LA to estimate the changed concentration for combined R0 layer. 
#this estimation process is conducted to add the change concentration of surrounding to a LA, surrounded by other LAs in city region. 

for (cla in 1:length(LAwithCR_cocen$LANames)) {
  
  lahomeC <- as.character(LAwithCR_cocen$LANames[cla])
  
  print(lahomeC)
  
  changecon <- LAwithCR_cocen$conchange[cla] #Scenario based change concentration of each LA
  
  print(changecon)
  
  #read the R0 concentration layer for each LA, that has concentration cell values beyond the LA boundary
  lanonlocalraster <- raster(paste0(Globalpath, lahomeC, '/','AggNOxR0.tif'))
  
  #multiply the base concentration layer with potential change values (e.g., 0.05, 0.1, 0.5)
  #Now this value is randomly generated but in main calculation this would be based on scenario
  changeconraster <- lanonlocalraster * changecon
  
  #Save the changed combined R0 concentration layer for each LA
  writeRaster(changeconraster, filename= file.path(Globalpath, lahomeC,  "changeconrasterR0.tif"), format="GTiff", overwrite=TRUE)

}

##############Adding Changed concentration from other LAs within a city region ####################

CityRegions <- list ('bristol', 'greatermanchester', 'leeds', 'liverpool', 'london', 'northeast','nottingham', 'sheffield', 'westmidlands')

#CityRegions <- list ('greatermanchester')

for (crh in 1:length(CityRegions)) {
  
  crigon <- as.character(CityRegions[crh])
  
  print(crigon)
  
  #subset the LAs within a city region
  citylahome <- subset (LAwithCR_cocen, cityregion == crigon)
  
  CRfilela <- list()
  
  for (clarg in 1:length(citylahome$LANames)) {
    
    crlahome <- as.character(citylahome$LAName[clarg])
    
    CRfilela [clarg] <- list.files(path = paste0(Globalpath, crlahome),pattern = "changeconrasterR0.tif$",full.names = TRUE, recursive = TRUE)
    
    print(crlahome)
  }
  
  CRLAfilesS <- unlist(CRfilela, recursive = TRUE)
  
  print(CRLAfilesS)
  
  CRLAfilesStack <- stack(CRLAfilesS)
  
  
  for (clarg in 1:length(citylahome$LANames)) {
    
    print(clarg)

    crlahomeX <- as.character(citylahome$LAName[clarg])
    print(crlahomeX)
  
    
    funX <- function(x) {sum((x [-clarg]))}
    
    ConOthers <- calc (CRLAfilesStack, funX)
    
    writeRaster(ConOthers, filename= file.path(Globalpath, crlahomeX,  "ChangeConOthers.tif"), format="GTiff", overwrite=TRUE)
  }
  
}

######### Estimating changed concentration for each LA based on impact factors and surrounding LAs in the city region #########

for (laname in 1:length(LAwithCR_cocen$LANames)) {
  
  lahomeName <- as.character(LAwithCR_cocen$LANames[laname])
  print(lahomeName)
  
  lahomeC <- as.character(LAwithCR_cocen$LANames[cla])
  
  #changecon <- LAwithCR_cocen$conchange[laname] #Scenario based change concentration of each LA
  changecon <- 0.9
  print(changecon)
  
  #read the total VKM for each LA and extract the sum of all cells
  TotalVKM <- raster(paste0(Globalpath, lahomeName, '/','VKMtotal_2020.tif'))
  vkmsum <- sum(as.vector(TotalVKM))
  
  print(vkmsum)
  
  #import local in-square impact factor
  LocalIF <- raster(paste0(Globalpath, lahomeName, '/','LIFNx.tif'))
  
  #import non-local impact factor
  NonLocalIF <- raster(paste0(Globalpath, lahomeName, '/','NonLIFNx.tif'))
  
  SorroundLAChangecon <- raster(paste0(Globalpath, lahomeName, '/','ChangeConOthers.tif'))
  
  LAchangedcon <- (((changecon * LocalIF * TotalVKM) + (changecon * NonLocalIF * (vkmsum - TotalVKM))) + SorroundLAChangecon)

  
  #Save the changed combined R0 concentration layer for each LA
  writeRaster(LAchangedcon, filename= file.path(Globalpath, lahomeName,  "LAchangedconNOx.tif"), format="GTiff", overwrite=TRUE)
  
}



#clean unnecessary files or other files that do not overwrite.

do.call(file.remove, list(list.files(path = paste0(Globalpath),pattern = "filename.tif$",full.names = TRUE, recursive = TRUE)))


