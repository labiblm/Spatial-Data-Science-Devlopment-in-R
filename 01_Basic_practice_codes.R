
##Work on my thesis data for basic scripting in R amd tidyverse

#### load libraries#######
library(tidyverse)
library(dplyr)
library(tidyr)
library(rworldmap)
library(DescTools) #function %like%
library(rgeos)
library(ggplot2)
library(viridis) 
library(tibble)
library(sf)

###### Data import ########

#import the thesis data
SMLT <- read.csv("input/MyThesisData_Prac.csv")
View (SMLT)
class(SMLT)


#### Indexing/Subset#####
#return the third row
SMLT [3,]

#index the 5th column for IMD
SMLT [ ,5]

#index first 5 rows for column 3 to 5
SMLT [1:5, 3:5]

#Index by variable name for first 50 rows
SMLT [1:50, "IMD_Decile"]

#index for not including first 1400 rows for columns 3 to 5
SMLT [c (-1:-1400), 3:5]

#creating a dataframe with positions for comulm five
SMLT_P <- data.frame(SMLT [,1:5], position= 1:length(SMLT[,5]))

#try split text, got it with tidy separate function
SMLT_p_name3 <- SMLT_P %>%
  separate ("lsoa11nm", into = c("AreaR", "code"), sep = " ")

#index for manchester and IMD decile <3
SMLT_p_name3 [SMLT_p_name3$AreaR == 'Manchester' & SMLT_p_name3$IMD_Decile < 3, 1:6]

SMLT_p_name3 [SMLT_p_name3$AreaR =='Manchester' & SMLT_p_name3$IMD_Score < 8, 1:6]

#index for Manchester, Bury and Oldham and IMD score < 8
SMLT_p_name3 [SMLT_p_name3$AreaR %in% c('Manchester', 'Bury', 'Oldham') & SMLT_p_name3$IMD_Score < 8, 1:6]


#dyplyr filter tried, same results
filter(SMLT_p_name3, AreaR == "Manchester", IMD_Score <= 8)
filter(SMLT_p_name3, AreaR %in% c("Manchester", "Bury", "Oldham"), IMD_Score <= 8)

SMLT_p_name4 <- filter(SMLT_p_name3, AreaR %in% c("Manchester", "Bury", "Oldham"), IMD_Score <= 8) %>%
  mutate(IMDSc10 = IMD_Score + 10)



####Data types and related loops ####
#vector
i.vec1<- c(2,3,6,8,10)
i.vec2 <- i.vec1* 5 #multiplying on vector
i.vec3 <- c("a", 7, "Labib") #vector with different values
i.vec4 <- paste0(i.vec1, "_", i.vec2) #use paste0
i.vec5 <- paste(i.vec1, "-", i.vec2) #use paste

#indexing vector
i.vec5[1:4] #elements from 1 to 4 vector
i.vec1[-4] #all except the number four element of the vector

#looping in vector to save in a list
n <- vector ()
o <- list()

for (i in i.vec1) {
  
  r <- 1
  m = r + i
  print(m)
  o [i] <- m
  
  for (j in m) {

    k = m+1
    print(k)
    n[j] <- k
    
    
  }
  
}

print(n)


######List#######

l.list1 <- list(1:50, "Labib", as.numeric (c(i.vec1, i.vec2, i.vec3)))

#access the second list component
l.list1[2]

#index the fifth element of first component of the list
l.list1 [[1]][5]

#reassign new value to list component
l.list1 [2] <- "mahin"

#reassign new value to element inside a list component
l.list1 [[3]][13] <- 100

l.list1 [[3]][11] <- 1000


#add new component to the list using $ sign and populate with simple operation
l.list1$numbersqr <- as.numeric(l.list1[[1]]^2)


for (k in l.list1[4]) print(k)

l.list2 <- list()

for (k in l.list1[4]) {
  
  print(k)
  
  x = k + i.vec1
  
  l.list2 [k] <- x
  
}



####Loops####

#loop over a matrix
y <- matrix(1:9, nrow = , ncol = 3)

#   [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8
#[3,]    3    6    9
#

#loop for each row from 1 to end
for (r in 1:nrow(y)) print (y)

#loop for each column from 1 to end, so for this matrix it is 9
for (l in 1:ncol(y)) print (y)

#loop for the lenght of the matrix (as it is 9)
for (z in 1:length(y)) print (z^2)




b0 <- c(2.65, 1.28, 3.29)
b1 <- c(0.9, 1.1, 1.2)
volumes = c(1.6, 3, 8)
masses <- vector(mode="numeric", length=length(volumes))

for (i in seq_along(volumes)){
  mass <- b0[i] * volumes[i] ^ b1[i]
  masses[i] <- mass
  print (masses[i])
}







imdscr <- list()
imdscrdf <- data.frame()

for (im in 1:nrow(SMLT)) {
  
  #print (SMLT$lsoa11nm)
  
  IMD_Sc <- SMLT
  
  imdscr$LSOA <- SMLT$lsoa11cd
  
  imdscr$imd <- SMLT$IMD_Score
  
  imdscr$comgreen = (SMLT$AvailComN + SMLT$GVILSOA + SMLT$AvailComN + SMLT$Compositem) / 4
  
  #print (imdscr$comgreen)
  
  if (imdscr$comgreen [im] > 0.5) {
    
    print("high")
    
  } else (print("low"))
  
}


imdscrdf <- as.data.frame(imdscr)





imdscr2 <- list()
imdscrdf2 <- data.frame()

for (im in 1:nrow(SMLT)) {
  
  #print (SMLT$lsoa11nm)
  
  IMD_Sc <- SMLT
  
  imdscr2$LSOA <- SMLT$lsoa11cd
  
  imdscr2$imd <- SMLT$IMD_Score
  
  imdscr2$comgreen = (SMLT$AvailComN + SMLT$GVILSOA + SMLT$AvailComN + SMLT$Compositem) / 4
  
  #print (imdscr$comgreen)
  
  if (imdscr2$comgreen [im] > 0.7) {
    
    print("High green")
    imdscr2$glevel [im] <- "High"
    
  } else if (imdscr2$comgreen [im] < 0.7 & imdscr2$comgreen [im] > 0.4){
    
    print("low green")
    imdscr2$glevel[im] <- "mid"
    
  } else {
    imdscr2$glevel[im] <- "low"
    }
  
}


imdscrdf2 <- as.data.frame(imdscr2)




#####loop on column and conditions ######
imdscrc <- list()
imdscrdfc <- data.frame()

for (im in SMLT[,1]) {
  
  #print (SMLT$lsoa11nm)
  
  IMD_Sc <- SMLT
  
  imdscrc$LSOA <- SMLT$lsoa11cd
  
  imdscrc$imd <- SMLT$IMD_Score
  
  imdscrc$comgreen = (SMLT$AvailComN + SMLT$GVILSOA + SMLT$AvailComN + SMLT$Compositem) / 4
  
  
  if (imdscrc$comgreen [im] > 0.7) {
    
    print("High green")
    imdscrc$glevel [im] <- "High"
    
  } else if (imdscrc$comgreen [im] < 0.7 & imdscr2$comgreen [im] > 0.4){
    
    print("mid green")
    imdscrc$glevel[im] <- "mid"
    
  } else {
    print("low green")
    imdscrc$glevel[im] <- "low"
  }
  
}


imdscrdfc <- as.data.frame(imdscrc)


#same work as loop above but using dplyr

SMLT8 <- SMLT %>%
  select (lsoa11cd, lsoa11nm, IMD_Decile, IMD_Score, GVILSOA, AccessS, AvailComN, Compositem) %>%
  mutate(comgreen2 = (GVILSOA + AccessS + AvailComN + Compositem)/4) %>%
  mutate(glevel2 = ifelse(comgreen2 > 0.7, "High", ifelse(comgreen2< 0.7 & comgreen2 > 0.4, "mid", "low"))) #%>%
  #filter(glevel2 == c('low'))



mnx <- c(2,3,4)
gnx <-c(8,9, 10)

cnx <- cbind(mnx, gnx)
rnx <- rbind(mnx, gnx)



#### dyplr basic #####

#rename the column name in the dataframe 
imdscrdf3 <- imdscrdf2 %>%
  rename (lsoa11cd = LSOA)

#join with other data and select the columns from that data
imdscrdf4 <- left_join(imdscrdf3, SMLT%>% select(lsoa11cd, IMD_Decile, no2_mean_a, lsoa11nm), by = "lsoa11cd")


imdscrdf4 %>%
  summarise(avg.comgreen = mean(comgreen))

imdscrdf4 %>%
  group_by(glevel) %>%
  summarise(avg.comgreen = mean(comgreen))

imdscrdf5 <- imdscrdf4 %>%
  mutate(intcgreenNO2= comgreen*no2_mean_a)


imdscrdf6 <- imdscrdf5 %>%
  filter(no2_mean_a < 10)
  print (imdscrdf6$comgreen)



