#############################################################################################################
###  FIGURE 13: Mean Vertical Structure in Seasonal Temperature, Salinity, Oxygen for HL6, LL7 vs. Gully  ###
#############################################################################################################

# Last updated: 31-AUG-2021
# Author: Lindsay Beazley
# Modified by: Jeff Jackson

#Load packages

library(oce)
library(tidyr)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(sp)
library(tibble)
library(fs)
library(lubridate) #for month function


#The .ctdH.rds and .ctdL.rds files contain CTD casts that were extracted near AZMP core stations HL6 and LL7.
#The data cover a wide portion of the slope around these stations and must be filtered so that only those
#casts within close proximity of the nominal station coordinates are used for analysis.

#The data must also be filtered to extract only the months and year of interest for
#this report (April = spring, Sept/Oct = fall, from 1999 onward). Once the casts of interest were extracted
#using this code, the data were reviewed spatially using ArcMap, and additional spatial filtering was
#applied. This is detailed in the methodology of the technical report.


#Read the .RDS files from Jeff containing the data for HL6 and LL7.
#View data and you can see the temporal range. We want this to match our Gully data, so filtering is required

projpath <- getwd()
datapath <- paste0(projpath, "/data/1_PhysicalData/")
HL_06_ctd <- readRDS(paste0(datapath, "ctdH.rds"))
LL_07_ctd <- readRDS(paste0(datapath, "ctdL.rds"))


# Extract each of the start date time values for the CTD profiles at each monitoring station.
HL6_Time <- lapply(HL_06_ctd, function(x) x@metadata$startTime)
LL7_Time <- lapply(LL_07_ctd, function(x) x@metadata$startTime)


# Convert the lists of datetime values to vectors.
dt1 <- do.call("c", HL6_Time)
dt2 <- do.call("c", LL7_Time)


# Get the list of months from each vector.
mdt1 <- month(dt1)
mdt2 <- month(dt2)

year1 <- year(dt1)
year2 <- year(dt2)


#Select spring and fall months (I am not sure why you can do mdt = 4 but it doesn't work)
spring_HL6 <- (mdt1!=1) & (mdt1!=2) & (mdt1!=3) & (mdt1<5) & (year1 >1999)
fall_HL6 <- (mdt1>8) & (mdt1<11) & (year1 >1999)

spring_LL7 <- (mdt2!=1) & (mdt2!=2) & (mdt2!=3) & (mdt2<5) & (year2 >1999)
fall_LL7 <- (mdt2>8) & (mdt2<11) & (year2 >1999)


#Extract casts in spring and fall from each station

sprmonthsHL6 <- HL_06_ctd[spring_HL6]  #80 objects
fallmonthsHL6 <- HL_06_ctd[fall_HL6]   #93 objects

sprmonthsLL7 <- LL_07_ctd[spring_LL7]  #60 objects
fallmonthsLL7 <- LL_07_ctd[fall_LL7]   #52 objects

#Check to see that it worked, selecting only spring or fall casts > 1999. It worked
ts <- lapply(sprmonthsHL6, function(x) x@metadata$startTime)


#Spring HL_06

sprmonthsHL6

sprmonthsHL6df <- NULL

for (i in 1:length(sprmonthsHL6)){
  d <- sprmonthsHL6[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  dfadd1 <- data.frame(cruise = cruise,
                       event = event,
                       lon = lon,
                       lat = lat,
                       pressure = pressure,
                       temperature = temperature,
                       salinity = salinity,
                       oxygen = oxygen)
  if(is.null(sprmonthsHL6df)){
    sprmonthsHL6df <- dfadd1
  } else {
    sprmonthsHL6df <- rbind(sprmonthsHL6df,
                          dfadd1)
  }
}


#Fall HL_06

fallmonthsHL6

fallmonthsHL6df <- NULL

for (i in 1:length(fallmonthsHL6)){
  d <- fallmonthsHL6[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  if (cruise == "HUD2008037")

  {

    dfadd2 <- data.frame(cruise = cruise,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = NA)

  } else {

    dfadd2 <- data.frame(cruise = cruise,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = oxygen)
  }

  if(is.null(fallmonthsHL6df)){
    fallmonthsHL6df <- dfadd2
  } else {
    fallmonthsHL6df <- rbind(fallmonthsHL6df,
                        dfadd2)
  }
}


#Spring LL7

sprmonthsLL7

sprmonthsLL7df <- NULL

for (i in 1:length(sprmonthsLL7)){
  d <- sprmonthsLL7[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  dfadd3 <- data.frame(cruise = cruise,
                       event = event,
                       lon = lon,
                       lat = lat,
                       pressure = pressure,
                       temperature = temperature,
                       salinity = salinity,
                       oxygen = oxygen)
  if(is.null(sprmonthsLL7df)){
    sprmonthsLL7df <- dfadd3
  } else {
    sprmonthsLL7df <- rbind(sprmonthsLL7df,
                            dfadd3)
  }
}


#Fall LL_07

fallmonthsLL7

fallmonthsLL7df <- NULL

for (i in 1:length(fallmonthsLL7)){
  d <- fallmonthsLL7[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  if (cruise == "HUD2008037")

  {

    dfadd4 <- data.frame(cruise = cruise,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = NA)

  } else {

    dfadd4 <- data.frame(cruise = cruise,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = oxygen)
  }

  if(is.null(fallmonthsLL7df)){
    fallmonthsLL7df <- dfadd4
  } else {
    fallmonthsLL7df <- rbind(fallmonthsLL7df,
                             dfadd4)
  }
}


#Look at resulting data

sprmonthsHL6df
fallmonthsHL6df

sprmonthsLL7df
fallmonthsLL7df


####The data required spatial filtering as it included all data from the last few HL/LL stations, plus the extended
#line. Some other datasets were included as far over as the Stone Fence near LL.


#Spatial filtering was done in ArcMap and the files were saved as .csv. Now reload data:

sprmonthsLL7_filtered <- read.csv(file=paste0(datapath, "sprmonthsLL7df_spatialfilter.csv"), header=T)   #16 profiles from 15 missions. Coriolis 2017 has duplicate casts - 100, 101. Check which one didn't go full depth.
fallmonthsLL7_filtered <- read.csv(file=paste0(datapath, "fallmonthsLL7df_spatialfilter.csv"), header=T) #16 profiles from 16 missions. Event 55 used twice in 2 different yrs
sprmonthsHL6_filtered <- read.csv(file=paste0(datapath, "sprmonthsHL6df_spatialfilter.csv"), header=T)   #11 profiles from 11 missions.
fallmonthsHL6_filtered <- read.csv(file=paste0(datapath, "fallmonthsHL6df_spatialfilter.csv"), header=T) #16 profiles from 16 missions. Event 47 used twice in 2 different yrs



#Some selected profiles are deeper than 200 m above nominal station depth (1100 for HL6, 760 for LL7)

#LL spring

sprLL7max <- sprmonthsLL7_filtered %>%
  group_by(cruise, event) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  slice(which.max(pressure))
sprLL7max
max(sprmonthsLL7_filtered$pressure)

#COR2017001 does not go full depth. Remove

sprmonthsLL7_filtered_v2<-subset(sprmonthsLL7_filtered, event != "101")
nrow(sprmonthsLL7_filtered_v2)

#LL fall

fallLL7max <- fallmonthsLL7_filtered %>%
  group_by(cruise, event) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  slice(which.max(pressure))
fallLL7max

max(fallmonthsLL7_filtered$pressure)   #remove HUD2015030. Too deep

fallmonthsLL7_filtered_v2<-subset(fallmonthsLL7_filtered, cruise != "HUD2015030")
nrow(fallmonthsLL7_filtered_v2)


#HL spring

sprHL6max <- sprmonthsHL6_filtered %>%
  group_by(cruise, event) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  slice(which.max(pressure))
sprHL6max
max(sprmonthsHL6_filtered$pressure)

#COR2017001 does not go full depth. Remove event 61

sprmonthsHL6_filtered_v2<-subset(sprmonthsHL6_filtered, event != "61")
nrow(sprmonthsHL6_filtered_v2)



#HL fall

fallHL6max <- fallmonthsHL6_filtered %>%
  group_by(cruise, event) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  slice(which.max(pressure))
fallHL6max

max(fallmonthsLL7_filtered$pressure)   #This is good


#Final datasets:

sprmonthsLL7_filtered_v2
sprmonthsLL7_filtered_v2
sprmonthsHL6_filtered_v2
fallmonthsHL6_filtered



#Plot data to remove erroneous values. These plots were used for analysis only; not for publication.

# plot(sprmonthsLL7_filtered_v2$temperature~sprmonthsLL7_filtered_v2$pressure, col=sprmonthsLL7_filtered_v2$event)
# plot(sprmonthsLL7_filtered_v2$salinity~sprmonthsLL7_filtered_v2$pressure, col=sprmonthsLL7_filtered_v2$event)
# plot(sprmonthsLL7_filtered_v2$oxygen~sprmonthsLL7_filtered_v2$pressure, col=sprmonthsLL7_filtered_v2$event)    #Zero values
#
#
# plot(fallmonthsLL7_filtered_v2$temperature~fallmonthsLL7_filtered_v2$pressure, col=fallmonthsLL7_filtered_v2$event)
# plot(fallmonthsLL7_filtered_v2$salinity~fallmonthsLL7_filtered_v2$pressure, col=fallmonthsLL7_filtered_v2$event)  #Zero values
# plot(fallmonthsLL7_filtered_v2$oxygen~fallmonthsLL7_filtered_v2$pressure, col=fallmonthsLL7_filtered_v2$event)    #Zero values
#
#
# plot(sprmonthsHL6_filtered_v2$temperature~sprmonthsHL6_filtered_v2$pressure, col=sprmonthsHL6_filtered_v2$event)
# plot(sprmonthsHL6_filtered_v2$salinity~sprmonthsHL6_filtered_v2$pressure, col=sprmonthsHL6_filtered_v2$event)  #Zero values
# plot(sprmonthsHL6_filtered_v2$oxygen~sprmonthsHL6_filtered_v2$pressure, col=sprmonthsHL6_filtered_v2$event)    #Zero values
#
#
# plot(fallmonthsHL6_filtered$temperature~fallmonthsHL6_filtered$pressure, col=fallmonthsHL6_filtered$event)
# plot(fallmonthsHL6_filtered$salinity~fallmonthsHL6_filtered$pressure, col=fallmonthsHL6_filtered$event)  #Zero values
# plot(fallmonthsHL6_filtered$oxygen~fallmonthsHL6_filtered$pressure, col=fallmonthsHL6_filtered$event)    #Zero values



#Examine the means of each profile, starting with the fall data

fallLL7mean <- fallmonthsLL7_filtered_v2 %>%
  group_by(cruise, event) %>%  #Group by cruise and event
  summarise_at(vars(c("temperature", "salinity", "oxygen")), list(mean = mean, sd = sd), na.rm = TRUE)
fallLL7mean

#Set zero values to NA in HUD2012042. Mean Salinity is zero.
HUD2008037<- subset(fallmonthsLL7_filtered_v2, fallmonthsLL7_filtered_v2$cruise =="HUD2008037")
# plot(HUD2008037$temperature~HUD2008037$pressure)    #This is ok. The oxygen will be removed since it's pre-2013


springLL7mean <- sprmonthsLL7_filtered_v2 %>%
  group_by(cruise, event) %>%  #Group by cruise and event
  summarise_at(vars(c("temperature", "salinity", "oxygen")), list(mean = mean, sd = sd), na.rm = TRUE)
springLL7mean


springHL6mean <- sprmonthsHL6_filtered_v2 %>%
  group_by(cruise, event) %>%  #Group by cruise and event
  summarise_at(vars(c("temperature", "salinity", "oxygen")), list(mean = mean, sd = sd), na.rm = TRUE)
springHL6mean


fallHL6mean <- fallmonthsHL6_filtered %>%
  group_by(cruise, event) %>%  #Group by cruise and event
  summarise_at(vars(c("temperature", "salinity", "oxygen")), list(mean = mean, sd = sd), na.rm = TRUE)
fallHL6mean


sprmonthsLL7_filtered_v2[sprmonthsLL7_filtered_v2 == 0] <- NA
fallmonthsLL7_filtered_v2[fallmonthsLL7_filtered_v2 == 0] <- NA
sprmonthsHL6_filtered_v2[sprmonthsHL6_filtered_v2 == 0] <- NA
fallmonthsHL6_filtered[fallmonthsHL6_filtered == 0] <- NA


#Write the data to .csv for a more permanent archival

write.csv(sprmonthsLL7_filtered_v2, file = paste0(datapath, "sprmonthsLL7_filtered_v2.csv"))
write.csv(fallmonthsLL7_filtered_v2, file = paste0(datapath, "fallmonthsLL7_filtered_v2.csv"))
write.csv(sprmonthsHL6_filtered_v2, file = paste0(datapath, "sprmonthsHL6_filtered_v2.csv"))
write.csv(fallmonthsHL6_filtered, file = paste0(datapath, "fallmonthsHL6_filtered.csv"))


#Now take the mean of all casts across years at each pressure. Only do temp/sal. Oxygen > 2013 done separately

MeansSpringHL6 <- sprmonthsHL6_filtered_v2  %>%
 group_by(pressure) %>%
 summarise_at(vars(c("temperature", "salinity")), funs(mean), na.rm = TRUE)
MeansSpringHL6
MeansSpringHL6subset <- subset(MeansSpringHL6, MeansSpringHL6$pressure>=10)

MeansFallHL6 <- fallmonthsHL6_filtered  %>%
  group_by(pressure) %>%
  summarise_at(vars(c("temperature", "salinity")), funs(mean), na.rm = TRUE)
MeansFallHL6
MeansFallHL6subset <- subset(MeansFallHL6, MeansFallHL6$pressure>=10)

MeansSpringLL7 <- sprmonthsLL7_filtered_v2  %>%
  group_by(pressure) %>%
  summarise_at(vars(c("temperature", "salinity")), funs(mean), na.rm = TRUE)
MeansSpringLL7
MeansSpringLL7subset <- subset(MeansSpringLL7, MeansSpringLL7$pressure>=10)

MeansFallLL7 <- fallmonthsLL7_filtered_v2 %>%
  group_by(pressure) %>%
  summarise_at(vars(c("temperature", "salinity")), funs(mean), na.rm = TRUE)
MeansFallLL7
MeansFallLL7subset <- subset(MeansFallLL7, MeansFallLL7$pressure>=10)


# For oxygen

sprmonthsHL6_filtered_v2$Year <- (substr(sprmonthsHL6_filtered_v2$cruise, 4, 7))
fallmonthsHL6_filtered$Year <- (substr(fallmonthsHL6_filtered$cruise, 4, 7))

sprmonthsLL7_filtered_v2$Year <- (substr(sprmonthsLL7_filtered_v2$cruise, 4, 7))
fallmonthsLL7_filtered_v2$Year <- (substr(fallmonthsLL7_filtered_v2$cruise, 4, 7))


sprmonthsHL6_2013 <- subset(sprmonthsHL6_filtered_v2, sprmonthsHL6_filtered_v2$Year >= "2013")
fallmonthsHL6_2013 <- subset(fallmonthsHL6_filtered, fallmonthsHL6_filtered$Year >= "2013")
sprmonthsLL7_2013 <- subset(sprmonthsLL7_filtered_v2, sprmonthsLL7_filtered_v2$Year >= "2013")
fallmonthsLL7_2013 <- subset(fallmonthsLL7_filtered_v2, fallmonthsLL7_filtered_v2$Year >= "2013")


MeansSpringHL6oxy <- sprmonthsHL6_2013  %>%
  group_by(pressure) %>%
  summarise_at(vars(c("oxygen")), funs(mean), na.rm = TRUE)
MeansSpringHL6oxy
MeansSpringHL6oxysubset <- subset(MeansSpringHL6oxy, MeansSpringHL6oxy$pressure>=10)

MeansFallHL6oxy <- fallmonthsHL6_2013  %>%
  group_by(pressure) %>%
  summarise_at(vars(c("oxygen")), funs(mean), na.rm = TRUE)
MeansFallHL6oxy
MeansFallHL6oxysubset <- subset(MeansFallHL6oxy, MeansFallHL6oxy$pressure>=10)

MeansSpringLL7oxy <- sprmonthsLL7_2013  %>%
  group_by(pressure) %>%
  summarise_at(vars(c("oxygen")), funs(mean), na.rm = TRUE)
MeansSpringLL7oxy
MeansSpringLL7oxysubset <- subset(MeansSpringLL7oxy, MeansSpringLL7oxy$pressure>=10)

MeansFallLL7oxy <- fallmonthsLL7_2013  %>%
  group_by(pressure) %>%
  summarise_at(vars(c("oxygen")), funs(mean), na.rm = TRUE)
MeansFallLL7oxy
MeansFallLL7oxysubset <- subset(MeansFallLL7oxy, MeansFallLL7oxy$pressure>=10)

# The following plots were not used in the report.
# plot(MeansFallLL7oxysubset$oxygen ~ MeansFallLL7oxysubset$pressure)  #one profile contributing to mean in deeper waters. Data are real
# plot(MeansFallHL6oxysubset$oxygen ~ MeansFallHL6oxysubset$pressure)  # one profile contributing to mean in deeper waters. Data are real
# plot(MeansSpringLL7oxysubset$oxygen ~ MeansSpringLL7oxysubset$pressure)
# plot(MeansSpringHL6oxysubset$oxygen ~ MeansSpringHL6oxysubset$pressure)


#Cut off deeper data of HL6 and LL7 fall. The data are real, but when you take the average of the surface above (based on multiple casts)
#it creates a discontinuous profile from the data below (based on a single cast)

MeansFallHL6oxysubset2 <- subset(MeansFallHL6oxysubset, MeansFallHL6oxysubset$pressure < 1090)
# plot(MeansFallHL6oxysubset2$oxygen~MeansFallHL6oxysubset2$pressure)  #Good

MeansFallLL7oxysubset2 <- subset(MeansFallLL7oxysubset, MeansFallLL7oxysubset$pressure < 790)
# plot(MeansFallLL7oxysubset2$oxygen~MeansFallLL7oxysubset2$pressure)  #Good

#Now combine temperature/salinity and oxygen means from 2013 onward

Means_HL_06spr_allvars <- merge(MeansSpringHL6subset, MeansSpringHL6oxysubset, by = "pressure")
Means_HL_06fall_allvars <- merge(MeansFallHL6subset, MeansFallHL6oxysubset2, by = "pressure")
Means_LL_07spr_allvars <- merge(MeansSpringLL7subset, MeansSpringLL7oxysubset, by="pressure")
Means_LL_07fall_allvars <- merge(MeansFallLL7subset, MeansFallLL7oxysubset2, by = "pressure")


#create a single column containing data for all three variables

HL_06_springmeans <- gather(Means_HL_06spr_allvars, variable, smean, -pressure)
HL_06_springmeans$Season <- "Spring"
HL_06_springmeans$station <- "HL_06"


HL_06_fallmeans <- gather(Means_HL_06fall_allvars, variable, smean, -pressure)
HL_06_fallmeans$Season <- "Fall"
HL_06_fallmeans$station <- "HL_06"

LL_07_springmeans <- gather(Means_LL_07spr_allvars, variable, smean, -pressure)
LL_07_springmeans$Season <- "Spring"
LL_07_springmeans$station <- "LL_07"

LL_07_fallmeans <- gather(Means_LL_07fall_allvars, variable, smean, -pressure)
LL_07_fallmeans$Season <- "Fall"
LL_07_fallmeans$station <- "LL_07"


#Combine data into a single dataframe

Combine_HL6_LL7 <- rbind.fill(HL_06_springmeans, HL_06_fallmeans, LL_07_springmeans, LL_07_fallmeans)


Combine_HL6_LL7 <- transform(Combine_HL6_LL7, variable=revalue(variable, c("temperature"="Temperature",
                                                                           "salinity" = "Salinity",
                                                                           "oxygen" = "Oxygen")))


#Load combined profile means for the 4 Gully stations:

GullyProfiles_AllSeasons <- readRDS(paste0(datapath, "Combined_allseasons_GullyProfiles_ForComparison.RDS"))


#Combine HL6/LL7 and Gully data

Combined_Gully_HL6_LL7 <- rbind.data.frame(Combine_HL6_LL7, GullyProfiles_AllSeasons)


#Re-order variables for plotting

Combined_Gully_HL6_LL7$Season <- ordered(Combined_Gully_HL6_LL7$Season, levels = c("Spring", "Fall"))

Combined_Gully_HL6_LL7$variable <- ordered(Combined_Gully_HL6_LL7$variable, levels = c("Temperature", "Salinity", "Oxygen"))

Combined_Gully_HL6_LL7$station <- ordered(Combined_Gully_HL6_LL7$station, levels = c("HL_06",
                                                                                     "LL_07",
                                                                                     "GULD_03",
                                                                                     "SG_28",
                                                                                     "GULD_04",
                                                                                     "SG_23"))


Combined_Gully_HL6_LL7$variable <- factor(Combined_Gully_HL6_LL7$variable, levels = c("Temperature", "Salinity","Oxygen"),
                                    labels = c("Temperature~(degree*C)", "Salinity", "Oxygen~(ml~L^{-1})"))


#Subset so that only the top 100 m are shown (better depicts patterns in shallow, comparable depth intervals)

Combined_Gully_HL6_LL7_1000 <- subset(Combined_Gully_HL6_LL7, Combined_Gully_HL6_LL7$pressure <= 1000)


fig13 <- ggplot(Combined_Gully_HL6_LL7_1000, aes(x = pressure, y = smean, colour=station, group=station)) +
  geom_line(size=1) +
  coord_flip() +
  scale_x_reverse() +
  scale_colour_manual(labels = c("HL_06", "LL_07", "GULD_03", "SG_28", "GULD_04", "SG_23"),
                     values = c("lightblue3", "#E36C14", "dodgerblue2", "gray35", "orange", "gray65")) +
  xlab("\nDepth (m)")  +
  labs(color='Station') +
  theme_bw() +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=15)) +
  theme(legend.text=element_text(size=13)) +
  theme(legend.title=element_text(size=14)) +
  theme(axis.title.y=element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x=element_blank())

fig13 <- fig13 + facet_grid(Season ~ variable, scales="free", labeller = label_parsed) +
  theme(strip.text.x = element_text(size=14, face="bold")) +
  theme(strip.text.y = element_text(size=14, face="bold")) +
  guides(fill=FALSE)

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure13.png"), units = "in", width = 11, height = 6, res = 400)

print(fig13)

dev.off()
