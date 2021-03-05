
##Google Mobility Data between Feb 2020 to Dec 2020

#####Load the libraries####
library(tidyverse)
library(lubridate) #date to month covert
library(zoo) # moving averages  
library(ggridges)
library(gganimate)
library(countrycode) #converting country name to ISO
library(rworldmap)
library(DescTools) 
library(ggplot2)
library(viridis) 
library(tibble)
library(sf)
library(GADMTools)
library(gifski)
library(av)


##### Data ######
  
GoogleMobility <- read.csv("input/Global_Mobility_Report.csv")
  
UKMobilityGM <- GoogleMobility %>%
  filter(country_region == "United Kingdom", sub_region_1 == "Greater Manchester") %>%
  group_by (sub_region_2)

UKMobilityGM_timeroll <- GoogleMobility %>%
  filter(country_region == "United Kingdom", sub_region_1 == "Greater Manchester") %>%
  group_by (sub_region_2) %>%
  mutate (avgPark = rollmean(parks_percent_change_from_baseline, k = 30, fill = NA))

UKMobilityGM_tparkavg <- UKMobilityGM %>% 
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  select(sub_region_2, parks_percent_change_from_baseline, iso_3166_2_code, month) %>%
  group_by(month, sub_region_2) %>%
  summarise(parkusechange = mean(parks_percent_change_from_baseline, na.rm = TRUE))

UKMobilityGM_tparkavgISO <- merge (UKMobilityGM_tparkavg, ISO3166_2codes, by.x=c("iso_3166_2_code"), by.y = c("code"))

  #group_by(sub_region_2) %>%
  #summarise(result2 = mean(result, na.rm = TRUE))
  
monthgroup <- UKMobility %>% 
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  group_by(month)

##### USA states based pattern #####

USAMobility <- GoogleMobility %>%
  filter(country_region == "United States")

write.csv(USAMobility, "USAGlobal_Mobility_Report.csv")


CAMobility <- GoogleMobility %>%
  filter(country_region == "Canada")

ISO3166_2codes <- read.csv("input/IP2LOCATION-ISO3166-2.CSV")

GoogleMobilityJoinISO3662 <- merge(GoogleMobility,ISO3166_2codes, by.x=c("sub_region_1"), by.y = c("subdivision_name"))


USMobilityStates_park <- USAMobility %>% 
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  select(sub_region_1, parks_percent_change_from_baseline, iso_3166_2_code, month) %>%
  group_by(month, sub_region_1) %>%
  summarise(parkusechange = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>%
  filter(sub_region_1 != "") %>%
  mutate(Monthn = month.abb[month]) %>%
  mutate(uselevel = ifelse (parkusechange > 100, "Very High", ifelse (parkusechange <= 100 & parkusechange >50, "Medium", ifelse (parkusechange <= 50, "low", 0))))


#for months
ggplot(USMobilityStates_park, aes(x = parkusechange, y= Monthn)) +
  geom_density_ridges2(height ="parkusechange", scale = 3, alpha = 0.5) +
  scale_y_discrete (limits = month.abb) + theme(panel.grid.major = element_line(linetype = "dotted"), 
    panel.grid.minor = element_line(linetype = "dashed"), 
    panel.background = element_rect(fill = "antiquewhite"))

#aes(fill = Monthn) in color needed, just add this after alpha

#for state
ggplot(USMobilityStates_park, aes(x = parkusechange, y= fct_rev (sub_region_1))) +
  geom_density_ridges (height ="parkusechange", scale = 3, alpha = 0.7, fill = "#00AFBB") +
  scale_x_continuous(expand = c(0, 0)) + ylab('States') + xlab('%Park use change from baseline') + theme(panel.background = element_rect(fill = NA)) +
  geom_vline(xintercept = 0, color = "red")


ggplot(USMobilityStates_park, aes(x = parkusechange, y= fct_rev (sub_region_1))) +
  geom_density_ridges_gradient (height ="parkusechange", scale = 3, alpha = 0.7, aes(fill = parkusechange)) +
  scale_x_continuous(expand = c(0, 0)) 
  

#for month variation
ggplot(USMobilityStates_park, aes(x = parkusechange, y= Monthn)) +
  geom_density_ridges (scale = 3, alpha = 0.7, aes(fill = Monthn)) + scale_y_discrete (limits = month.abb) +
  scale_x_continuous(expand = c(0, 0)) + geom_vline(xintercept = 0, color = "red") + ylab('Month') + 
  xlab('%Park use change from baseline') + theme(panel.background = element_rect(fill = NA)) +
  labs(fill = "Month") + theme(legend.title = element_text(colour="black", size=12, face="bold")) 


###### UK city regions ######

cityregionuk <- c("Greater Manchester", "Greater London", "Bristol City","Merseyside", "West Midlands", "West Yorkshire", "Edinburgh") 

UKMobility_park <- UKMobility %>% 
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  select(sub_region_1, parks_percent_change_from_baseline, iso_3166_2_code, month) %>%
  group_by(month, sub_region_1) %>%
  summarise(parkusechange = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>%
  filter(sub_region_1 != "") %>%
  mutate(Monthn = month.abb[month])


UKMobility_park2 <- UKMobility_park %>% filter(sub_region_1 %in% cityregionuk)

ggplot(UKMobility_park2) +
  aes(x = month, y = parkusechange) +
  geom_area(size = 1L, fill = "#00AFBB") +
  scale_fill_hue() +
  theme_minimal() +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  facet_wrap(vars(sub_region_1))

ggplot(UKMobility_park2) +
  aes(x = month, y = parkusechange, colour = parkusechange) +
  geom_line(size = 1L) +
  scale_color_gradient(low = "red", high = "green") +
  scale_x_continuous(breaks=c(2,4,6,8,10)) +
  theme_minimal() +
  facet_wrap(vars(sub_region_1))


colSums(is.na(UKMobility_park2))



##### mapping the trend in park use change ######

#USA FIPS code to lat long and then point
UScountyFIPS_coordinates <- read.csv("input/UScountyFIPS_coordinates.csv")

UScountyFIPS_coordinatesshort <- UScountyFIPS_coordinates %>%
  select(GEOID, NAME, INTPTLAT, INTPTLONG)

USGoogleMobilityshort <- USAMobility %>%
  select(country_region, sub_region_1, sub_region_2, census_fips_code, date, parks_percent_change_from_baseline)

USAGMobilityMergeISO3662 <- merge(USGoogleMobilityshort,UScountyFIPS_coordinatesshort, by.x=c("census_fips_code"), by.y = c("GEOID"))

colSums(is.na(USMobilityStates_park))


SubReg2USMobilityStates_park <- USAGMobilityMergeISO3662 %>% 
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  group_by(month, sub_region_2) %>%
  summarise(parkusechange = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>%
  filter(sub_region_2 != "") %>%
  filter(!is.na(parkusechange)) %>%
  mutate(Monthn = month.abb[month])


colSums(is.na(GoogleMobility))


GlobalParkMobility <- GoogleMobility %>% 
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  select(country_region, sub_region_1, parks_percent_change_from_baseline, iso_3166_2_code, month) %>%
  group_by(month, sub_region_1, country_region, iso_3166_2_code) %>%
  summarise(parkusechange = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>%
  filter(sub_region_1 != "") %>%
  mutate(Monthn = month.abb[month])


GlobalParkMobilitybyCountry <- GoogleMobility  %>%
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  select(country_region, sub_region_1, parks_percent_change_from_baseline, iso_3166_2_code, month) %>%
  group_by(month, country_region) %>%
  summarise(parkusechangeC = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(Monthn = month.abb[month]) %>%
  mutate(uselevel = ifelse (parkusechangeC > 100, "high", ifelse (parkusechangeC <= 100 & parkusechangeC >50, "mid", ifelse (parkusechangeC <= 50 & parkusechangeC >0, "low", ifelse (parkusechangeC <= 0, "vlow", 0)))))



high <- ("#008000")
mid <- ("#40E0D0")
low <- ("#1E90FF")
vlow <- ("#DC143C")

movingdots <- ggplot(GlobalParkMobilitybyCountry, aes(country_region, parkusechangeC, color = factor(uselevel))) +
  geom_point(alpha = 0.7, size = 2, show.legend = TRUE) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = 50, color = "green") +
  scale_colour_manual(values = c(high, low, mid, vlow))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #animation setting
  labs(
    title = 'Month: {frame_time}',
    x = 'Country', 
    y = 'Park use change'
  ) +
  transition_time(as.integer(month)) + #show the month as integer
  shadow_mark(alpha = 0.3, size = 0.5) +
  ease_aes('linear')

userate <- animate(movingdots, width= 2000, height=800, nframes = 400, fps = 20,  renderer = gifski_renderer())
#renderer = gifski_renderer() if want to render as gif
#ffmpeg_renderer() form mp4

anim_save('testparkrate16.gif', animation=userate)




####animate for date#####

GlobalParkMobilitybyCountry2 <- GoogleMobility  %>%
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  select(country_region, day, sub_region_1, parks_percent_change_from_baseline, month) %>%
  group_by(country_region, month, day) %>%
  summarise(parkusechangeC = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(uselevel = ifelse (parkusechangeC > 100, "high", ifelse (parkusechangeC <= 100 & parkusechangeC >50, "mid", ifelse (parkusechangeC <= 50 & parkusechangeC >0, "low", ifelse (parkusechangeC <= 0, "vlow", 0)))))


high <- ("#008000")
mid <- ("#40E0D0")
low <- ("#1E90FF")
v.low <- ("#DC143C")

movingdots2 <- ggplot(GlobalParkMobilitybyCountry2, aes(country_region, parkusechangeC, color = factor(uselevel))) +
  geom_point(alpha = 0.7, size = 2, show.legend = TRUE) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = 50, color = "green") +
  scale_colour_manual(values = c(high, low, mid, v.low))+
  theme(legend.title = element_text(colour="black", size=12, face="bold"), plot.title = element_text(colour="#D55E00", size = 16, face = "bold", hjust = 0.5, vjust = -10))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=12, face="bold")) +
  #animation setting
  labs(
    title = 'Day: {frame_time}',
    x = 'Country', 
    y = '% Park use change'
  ) +
  transition_time(day) +
  shadow_mark(alpha = 0.4, size = 0.5) +
  ease_aes('linear',interval = 0.001) +
  annotate(geom = "text", x=-Inf,y=50,hjust=0,vjust=0, label = "50% increase")

userate2 <- animate(movingdots2, width= 2000, height=600, nframes = 600, fps = 20,  renderer = ffmpeg_renderer())
#renderer = gifski_renderer() if want to render as gif
#ffmpeg_renderer() form mp4

anim_save('testparkrateday13.mp4', animation=userate2)


### animating on world####

wmap <- getMap(resolution="low")
wmap <-   subset(wmap, !(NAME %like% "Antarctica")) # Remove Antarctica

wmapshape <- writeOGR(wmap, ".", layer = "wmapG", driver="ESRI Shapefile")

wmap_df <- fortify(wmap, region = "ISO_A2")

wmap_dfbound <- subset(wmap_df, !(id %like% "-99"))

GlobalParkMobilitybyCountryMap <- GoogleMobility  %>%
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  select(country_region_code, country_region, day, sub_region_1, parks_percent_change_from_baseline, month) %>%
  group_by(country_region_code,month) %>%
  summarise(parkusechangeC = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(MonthName = month.abb[month]) %>%
  mutate (
    COVIDinfo = case_when(
      month == 2 ~ "Early spread of COVID-19",
      month == 3 ~ "Strict Lockdown introduced",
      month == 4 ~ "Lockdown continue",
      month == 5 ~ "Relax Lockdown continue",
      month == 6 ~ "Relax Lockdown continue",
      month == 7 ~ "Relax Lockdown continue",
      month == 8 ~ "Relax Lockdown continue",
      month == 9 ~ "Relax Lockdown continue",
      month == 10 ~ "Possible second Wave, new lockdown rules",
      month == 11 ~ "Possible second Wave, new lockdown rules",
      month == 12 ~ "Possible second Wave, strict lockdown rules")) %>%
  mutate(uselevel = ifelse (parkusechangeC > 100, "high", ifelse (parkusechangeC <= 100 & parkusechangeC >50, "mid", ifelse (parkusechangeC <= 50 & parkusechangeC >0, "low", ifelse (parkusechangeC <= 0, "vlow", 0)))))


GlobalParkMobilitybyCountryMapdf <- as.data.frame(GlobalParkMobilitybyCountryMap)

countryISO <- read.csv("input/CountryISO3all.csv")

GlobalParkMobilitybyCountryISOmap <- merge(GlobalParkMobilitybyCountryMapdf, countryISO, by.x=c("country_region_code"), by.y = c("alpha.2"))


GlobalParkMobilityforjoin <- GlobalParkMobilitybyCountryISOmap %>%
  select(month, MonthName, parkusechangeC, country_region_code, COVIDinfo, alpha.3) %>%
  mutate(id = country_region_code)


GlobalParkMobilityforjoindf <- as.data.frame(GlobalParkMobilityforjoin)

vmax <- max(GlobalParkMobilityforjoindf$parkusechangeC, na.rm=T)
vmin <- min(GlobalParkMobilityforjoindf$parkusechangeC, na.rm=T)



wmap_dfJ <- left_join(wmap_df, GlobalParkMobilityforjoindf, by = c('id'='id'))  

wmap_dfjna <- na.omit(wmap_dfJ)

o <- ggplot(data=wmap_dfjna) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=parkusechangeC), color="white") +
  scale_fill_viridis(name="use change", limits = c(vmin,vmax), na.value="red") +
  labs(
    title = 'Park use change', subtitle = 'Month: {frame_time}'
  ) +
  transition_time(as.integer(month)) +
  ease_aes('linear', interval = 0.01)

plot(o)
mapGIF <- animate(o, width= 1200, height=600, fps = 20, renderer = gifski_renderer())
anim_save("MPCmap7.gif", animation=mapGIF)

ObyMonthstate <- ggplot(data=wmap_dfjna) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=parkusechangeC), color="white") +
  scale_fill_gradientn (name="% Use change", limits = c(vmin,vmax), colors = c("#fc9272", "#ffffcc", "#ffffcc", "#31a354", "#006837")) +
  geom_polygon(data = wmap_dfbound, aes(x=long, y=lat, group=group), color="gray90", fill=NA) +
  theme_void () +
  labs(
    title = 'Park use change', subtitle = 'Month: {closest_state}'
  ) +
  theme(legend.title = element_text(colour="black", size=12, face="bold"), plot.title = element_text(colour="#D55E00", size = 16, face = "bold", hjust = 0.5, vjust = -5))+
  theme(plot.subtitle = element_text(color = "black", size =  14, face = "bold", hjust = 0.5, vjust = -6)) +
  geom_text(aes(y = 90, label = COVIDinfo),
            x = 0, check_overlap = TRUE) +
  transition_states (month, transition_length = 2, state_length = 2) +
  ease_aes('linear')

plot(ObyMonthstate)


mapGIFstate <- animate(ObyMonthstate, width= 1600, height=820, fps = 25, renderer = ffmpeg_renderer(),duration = 25)

anim_save("State_month16.mp4", animation=mapGIFstate)

USA <- gadm_sf_loadCountries ("USA", level=0, basefile="./")


###### for each day ###################


GlobalParkMobilitybyCountryMap2 <- GoogleMobility  %>%
  mutate(day = ymd(date),Year = year(date), month = month(date)) %>%
  select(country_region_code, country_region, day, sub_region_1, parks_percent_change_from_baseline, month) %>%
  group_by(country_region_code, day) %>%
  summarise(parkusechangeC = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(uselevel = ifelse (parkusechangeC > 100, "high", ifelse (parkusechangeC <= 100 & parkusechangeC >50, "mid", ifelse (parkusechangeC <= 50 & parkusechangeC >0, "low", ifelse (parkusechangeC <= 0, "vlow", 0)))))


GlobalParkMobilitybyCountryMapdf2 <- as.data.frame(GlobalParkMobilitybyCountryMap2)

countryISO <- read.csv("input/CountryISO3all.csv")

GlobalParkMobilitybyCountryISOmap2 <- merge(GlobalParkMobilitybyCountryMapdf2, countryISO, by.x=c("country_region_code"), by.y = c("alpha.2"))


GlobalParkMobilityforjoin2 <- GlobalParkMobilitybyCountryISOmap2 %>%
  select(day, parkusechangeC, country_region_code, alpha.3) %>%
  mutate(id = country_region_code)


GlobalParkMobilityforjoindf2 <- as.data.frame(GlobalParkMobilityforjoin2)

vmax2 <- max(GlobalParkMobilityforjoindf2$parkusechangeC, na.rm=T)
vmin2 <- min(GlobalParkMobilityforjoindf2$parkusechangeC, na.rm=T)


wmap_dfJ2 <- left_join(wmap_df, GlobalParkMobilityforjoindf2, by = c('id'='id'))  

wmap_dfjna2 <- na.omit(wmap_dfJ2)


o2 <- ggplot() +
  geom_polygon(data=wmap_dfjna2, aes(x = long, y = lat, group = group, fill=parkusechangeC), color="white") +
  scale_fill_viridis(name="use change", limits = c(vmin2,vmax2), na.value="red") +
  theme_void() +
  labs(
    title = 'Park use change', subtitle = 'Day: {frame_time}'
  ) +
  geom_polygon(data = wmap_dfbound, aes(x=long, y=lat, group=group), color="gray50", fill=NA) +
  transition_time(day) + 
  ease_aes('linear', interval = 0.005)

plot(o2)

mapGIF2 <- animate(o2, width= 1600, height=820, fps = 20, renderer = gifski_renderer(), end_pause = 3)

anim_save("MPCmap9.gif", animation=mapGIF2)



