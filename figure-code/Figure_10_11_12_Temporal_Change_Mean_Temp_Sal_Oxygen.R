#########################################################################################################
##### FIGURES 10, 11, 12: Temporal Changes in Mean Temp, Sal, Oxygen by Station/Season ##################
#########################################################################################################

# Last updated: 31-AUG-2021
# Author: Lindsay Beazley
# Modified by: Jeff Jackson


#Load packages

library(oce)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)


#Load data provided by Jeff Jackson

projpath <- getwd()
datapath <- paste0(projpath, "/data/1_PhysicalData/")
load(paste0(datapath, "monitoringSitesCtdForAnalysis.RData"))
load(paste0(datapath, "monitoringSitesCtdForAnalysisPrograms.RData"))


#Extract CTD casts by station

datasetSpringGuld03 <- guld03_ctd[guld03_programs == "Spring AZMP"] # 12 Profiles
datasetFallGuld03 <- guld03_ctd[guld03_programs == "Fall AZMP"]  # 15 Profiles

datasetSpringGuld04 <- guld04_ctd[guld04_programs == "Spring AZMP"] # 10 Profiles
datasetFallGuld04 <- guld04_ctd[guld04_programs == "Fall AZMP"]  # 11 Profiles

datasetSpringSG23 <- sg23_ctd[sg23_programs == "Spring AZMP"] # 7 Profiles
datasetFallSG23 <- sg23_ctd[sg23_programs == "Fall AZMP"] # 9 Profiles

datasetSpringSG28 <- sg28_ctd[sg28_programs == "Spring AZMP"] # 8 Profiles
datasetFallSG28 <- sg28_ctd[sg28_programs == "Fall AZMP"] # 10 Profiles



#Parse the CTD data into individual dataframes (df) per station/season


#Spring GULD_03

guld03spring <- guld03_ctd[guld03_programs == "Spring AZMP"] # 12 Profiles

guld03springdf <- NULL

for (i in 1:length(guld03spring)){
  d <- guld03spring[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  station <- d[['originalStation']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  dfadd1 <- data.frame(cruise = cruise,
                      station = station,
                      event = event,
                      lon = lon,
                      lat = lat,
                      pressure = pressure,
                      temperature = temperature,
                      salinity = salinity,
                      oxygen = oxygen)
  if(is.null(guld03springdf)){
    guld03springdf <- dfadd1
  } else {
    guld03springdf <- rbind(guld03springdf,
                            dfadd1)
  }
}


#Fall GULD_03

guld03fall <- guld03_ctd[guld03_programs == "Fall AZMP"]  # 15 Profiles

# If the current file being processed is a HUD2008037 mission profile then
# make the oxygen data channel null since no oxygen sensor data was collected.

guld03falldf <- NULL

for (i in 1:length(guld03fall)) {
  d <- guld03fall[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  station <- d[['originalStation']]
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
                      station = station,
                      event = event,
                      lon = lon,
                      lat = lat,
                      pressure = pressure,
                      temperature = temperature,
                      salinity = salinity,
                      oxygen = NA)

  } else {

  dfadd2 <- data.frame(cruise = cruise,
                         station = station,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = oxygen)
  }

  if(is.null(guld03falldf)) {
    guld03falldf <- dfadd2
  } else {
    guld03falldf <- rbind(guld03falldf,
                            dfadd2)
  }
}




#GULD_04 Spring

guld04spring <- guld04_ctd[guld04_programs == "Spring AZMP"] # 10 Profiles

guld04springdf <- NULL

for (i in 1:length(guld04spring)){
  d <- guld04spring[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  station <- d[['originalStation']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  dfadd3 <- data.frame(cruise = cruise,
                      station = station,
                      event = event,
                      lon = lon,
                      lat = lat,
                      pressure = pressure,
                      temperature = temperature,
                      salinity = salinity,
                      oxygen = oxygen)
  if(is.null(guld04springdf)){
    guld04springdf <- dfadd3
  } else {
    guld04springdf <- rbind(guld04springdf,
                            dfadd3)
  }
}



#GULD_04 Fall

# If the current file being processed is a HUD2008037 mission profile then
# make the oxygen data channel null since no oxygen sensor data was collected.

guld04fall <- guld04_ctd[guld04_programs == "Fall AZMP"]  # 11 Profiles

guld04falldf <- NULL

for (i in 1:length(guld04fall)){
  d <- guld04fall[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  station <- d[['originalStation']]
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
                         station = station,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = NA)

  } else {

    dfadd4 <- data.frame(cruise = cruise,
                         station = station,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = oxygen)
  }


  if(is.null(guld04falldf)){
    guld04falldf <- dfadd4
  } else {
    guld04falldf <- rbind(guld04falldf,
                            dfadd4)
  }
}


#SG_23 Spring

SG23spring <- sg23_ctd[sg23_programs == "Spring AZMP"] # 7 Profiles

SG23springdf <- NULL

for (i in 1:length(SG23spring)){
  d <- SG23spring[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  station <- d[['originalStation']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  dfadd5 <- data.frame(cruise = cruise,
                       station = station,
                       event = event,
                       lon = lon,
                       lat = lat,
                       pressure = pressure,
                       temperature = temperature,
                       salinity = salinity,
                       oxygen = oxygen)
  if(is.null(SG23springdf)){
    SG23springdf <- dfadd5
  } else {
    SG23springdf <- rbind(SG23springdf,
                            dfadd5)
  }
}



#SG_23 Fall

# If the current file being processed is a HUD2008037 mission profile then
# make the oxygen data channel null since no oxygen sensor data was collected.

SG23fall <- sg23_ctd[sg23_programs == "Fall AZMP"] # 9 Profiles

SG23falldf <- NULL

for (i in 1:length(SG23fall)){
  d <- SG23fall[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  station <- d[['originalStation']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  if (cruise == "HUD2008037")

  {

    dfadd6 <- data.frame(cruise = cruise,
                         station = station,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = NA)

  } else {

    dfadd6 <- data.frame(cruise = cruise,
                         station = station,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = oxygen)
  }


  if(is.null(SG23falldf)){
    SG23falldf <- dfadd6
  } else {
    SG23falldf <- rbind(SG23falldf,
                          dfadd6)
  }
}



#SG_28 Spring

SG28spring <- sg28_ctd[sg28_programs == "Spring AZMP"] # 8 Profiles

SG28springdf <- NULL

for (i in 1:length(SG28spring)){
  d <- SG28spring[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  station <- d[['originalStation']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  dfadd7 <- data.frame(cruise = cruise,
                       station = station,
                       event = event,
                       lon = lon,
                       lat = lat,
                       pressure = pressure,
                       temperature = temperature,
                       salinity = salinity,
                       oxygen = oxygen)
  if(is.null(SG28springdf)){
    SG28springdf <- dfadd7
  } else {
    SG28springdf <- rbind(SG28springdf,
                          dfadd7)
  }
}



#SG_28 Fall

# If the current file being processed is a HUD2008037 mission profile then
# make the oxygen data channel null since no oxygen sensor data was collected.

SG28fall <- sg28_ctd[sg28_programs == "Fall AZMP"] # 10 Profiles

SG28falldf <- NULL

for (i in 1:length(SG28fall)){
  d <- SG28fall[[i]]
  # get data out of ctd object
  cruise <- d[['cruiseNumber']]
  station <- d[['originalStation']]
  event <- d[['station']]
  lon <- d[['longitude']]
  lat <- d[['latitude']]
  pressure <- d[['pressure']]
  temperature <- d[['temperature']]
  salinity <- d[['salinity']]
  oxygen <- d[['oxygen']]

  if (cruise == "HUD2008037")

  {

    dfadd8 <- data.frame(cruise = cruise,
                         station = station,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = NA)

  } else {

    dfadd8 <- data.frame(cruise = cruise,
                         station = station,
                         event = event,
                         lon = lon,
                         lat = lat,
                         pressure = pressure,
                         temperature = temperature,
                         salinity = salinity,
                         oxygen = oxygen)
  }

  if(is.null(SG23falldf)){
    SG28falldf <- dfadd8
  } else {
    SG28falldf <- rbind(SG28falldf,
                        dfadd8)
  }
}


#Replace station names with the 'cleaner' station names

guld03springdf$station <- "GULD_03"
guld04springdf$station <- "GULD_04"
SG23springdf$station <- "SG_23"
SG28springdf$station <- "SG_28"

guld03falldf$station <- "GULD_03"
guld04falldf$station <- "GULD_04"
SG23falldf$station <- "SG_23"
SG28falldf$station <- "SG_28"


#Plot data to look for erroneous values. The following plots are not part of the publication.

# plot(guld03springdf$salinity~guld03springdf$pressure, col=guld03springdf$event)  #has 1 salinity value that is ~17 PSU
# plot(guld04springdf$salinity~guld04springdf$pressure, col=guld04springdf$event)
# plot(guld03falldf$salinity~guld03falldf$pressure, col=guld03falldf$event)
# plot(guld04falldf$salinity~guld04falldf$pressure, col=guld04falldf$event)
#
# plot(SG23springdf$salinity~SG23springdf$pressure, col=SG23springdf$event)
# plot(SG28springdf$salinity~SG28springdf$pressure, col=SG28springdf$event)
# plot(SG23falldf$salinity~SG23falldf$pressure, col=SG23falldf$event)
# plot(SG28falldf$salinity~SG28falldf$pressure, col=SG28falldf$event)
#
# plot(guld03springdf$temperature~guld03springdf$pressure, col=guld03springdf$event)
# plot(guld04springdf$temperature~guld04springdf$pressure, col=guld04springdf$event)
# plot(guld03falldf$temperature~guld03falldf$pressure, col=guld03falldf$event)
# plot(guld04falldf$temperature~guld04falldf$pressure, col=guld04falldf$event)
#
# plot(SG23springdf$temperature~SG23springdf$pressure, col=SG23springdf$event)
# plot(SG28springdf$temperature~SG28springdf$pressure, col=SG28springdf$event)
# plot(SG23falldf$temperature~SG23falldf$pressure, col=SG23falldf$event)
# plot(SG28falldf$temperature~SG28falldf$pressure, col=SG28falldf$event)


#Remove that erroneous salinity value in guld03springdf.
#The is.na(salinity) allows us to keep NA values
guld03springdf <- subset(guld03springdf, salinity > 17.1| is.na(salinity))

# The following plot is not part of the publication.

# plot(guld03springdf$salinity~guld03springdf$pressure, col=guld03springdf$event) #Data are correct


# Extract Vertical Layers of Spring Temperature & Salinity Data ------------------------------

#oxygen is extracted separately as only data from 2013 onward are used


Combined_spring <- rbind.fill(guld03springdf, guld04springdf, SG23springdf, SG28springdf)
Combined_spring$Year <- (substr(Combined_spring$cruise, 4, 7))  #extracts the year from cruise name. Characters 4 through 7


#Top 50 m  (matches layer of biological productivity)
Top50AVG_spring <- Combined_spring %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/event
  filter(pressure<=50) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Top50AVG_spring

Top50AVG_spring$DepthBin <- "0 - 50 m"


#50 to 100 m layer
Lyr50_100AVG_spring <- Combined_spring %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/event
  filter(pressure>50 & pressure<=100) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Lyr50_100AVG_spring

Lyr50_100AVG_spring$DepthBin <- "50 - 100 m"


#100 to 400 m layer
Lyr100_400AVG_spring <- Combined_spring %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/event
  filter(pressure>100 & pressure<=400) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Lyr100_400AVG_spring

Lyr100_400AVG_spring$DepthBin <- "100 - 400 m"


#400 to 750 layer
Lyr400_750AVG_spring <- Combined_spring %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/event
  filter(pressure>400 & pressure<=750) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Lyr400_750AVG_spring

Lyr400_750AVG_spring$DepthBin <- "400 - 750 m"


#750 to bottom layer
Lyr750_bottomAVG_spring <- Combined_spring %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/event
  filter(pressure>750 & max(pressure)) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Lyr750_bottomAVG_spring

Lyr750_bottomAVG_spring$DepthBin <- "750 m - near-bottom"




#Combine all 'layers' into one

SpringData_ByDepth<-rbind.fill(Top50AVG_spring, Lyr50_100AVG_spring, Lyr100_400AVG_spring,
                               Lyr400_750AVG_spring, Lyr750_bottomAVG_spring)


#Add a column that denotes the year of collection

SpringData_ByDepth$Year <- (substr(SpringData_ByDepth$cruise, 4, 7))  #extracts the year from cruise name. Characters 4 through 7



#To calculate mean of each depth interval for each station (if of interest):

#MeanDepthBins <- SpringData_ByDepth %>%
#  group_by(station, DepthBin) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
#  summarise_at(vars(c("temperature")), funs(mean, sd), na.rm = TRUE)
#MeanDepthBins





# Extract Vertical Layers of Fall Temperature & Salinity Data ------------------------------

#oxygen is extracted separately as only data from 2013 onward are used


Combined_fall <- rbind.fill(guld03falldf, guld04falldf, SG23falldf, SG28falldf)
Combined_fall$Year <- (substr(Combined_fall$cruise, 4, 7))  #extracts the year from cruise name. Characters 4 through 7



#Top 50 m  (matches layer of biological productivity)
Top50AVG_fall <- Combined_fall %>%
  group_by(cruise, station) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  filter(pressure<=50) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Top50AVG_fall

Top50AVG_fall$DepthBin <- "0 - 50 m"


#50 to 100 m layer
Lyr50_100AVG_fall <- Combined_fall %>%
  group_by(cruise, station) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  filter(pressure>50 & pressure<=100) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Lyr50_100AVG_fall

Lyr50_100AVG_fall$DepthBin <- "50 - 100 m"


#100 to 400 m layer
Lyr100_400AVG_fall <- Combined_fall %>%
  group_by(cruise, station) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  filter(pressure>100 & pressure<=400) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Lyr100_400AVG_fall

Lyr100_400AVG_fall$DepthBin <- "100 - 400 m"


#400 to 750 layer
Lyr400_750AVG_fall <- Combined_fall %>%
  group_by(cruise, station) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  filter(pressure>400 & pressure<=750) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Lyr400_750AVG_fall

Lyr400_750AVG_fall$DepthBin <- "400 - 750 m"


#750 to bottom layer
Lyr750_bottomAVG_fall <- Combined_fall %>%
  group_by(cruise, station) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
  filter(pressure>750 & max(pressure)) %>%
  summarise_at(vars(c("temperature", "salinity")), list(mean = mean), na.rm = TRUE)
Lyr750_bottomAVG_fall

Lyr750_bottomAVG_fall$DepthBin <- "750 m - near-bottom"




#Combine all 'layers' into one

fallData_ByDepth<-rbind.fill(Top50AVG_fall, Lyr50_100AVG_fall, Lyr100_400AVG_fall,
                              Lyr400_750AVG_fall, Lyr750_bottomAVG_fall)


#Add a column that denotes the year of collection

fallData_ByDepth$Year <- (substr(fallData_ByDepth$cruise, 4, 7))  #extracts the year from cruise name. Characters 4 through 7



#To calculate mean of each depth interval for each station (if of interest):

#MeanDepthBinsFall <- fallData_ByDepth %>%
#  group_by(station, DepthBin) %>%  #Group by cruise and event - find the data associated with the max pressure value within each cruise/event
# summarise_at(vars(c("temperature")), funs(mean, sd), na.rm = TRUE)
#MeanDepthBinsFall





# Create Plots of Temporal Changes in Seasonal Temp, Sal ------------------

#Add a column to denote season & combine

SpringData_ByDepth$Season <- "Spring"
fallData_ByDepth$Season <- "Fall"

springfall <- rbind.fill(SpringData_ByDepth, fallData_ByDepth)


#Make year numeric so it can be plotted as a continuous variable

springfall$Year <- as.numeric(springfall$Year)


#Re-order variables for plotting

springfall$Season <- ordered(springfall$Season, levels = c("Spring", "Fall"))

springfall$station <- ordered(springfall$station, levels = c("GULD_03",
                                                            "SG_28",
                                                             "GULD_04",
                                                             "SG_23"))

springfall$DepthBin <- ordered(springfall$DepthBin, levels = c("0 - 50 m",
                                                             "50 - 100 m",
                                                              "100 - 400 m",
                                                              "400 - 750 m",
                                                             "750 m - near-bottom"))


#TEMPERATURE

fig10 <- ggplot(springfall, aes(x=Year, y=temperature_mean), colour= DepthBin, group=DepthBin) +
  geom_point(aes(y = temperature_mean, colour= DepthBin, group=DepthBin), size=3) +
  geom_line(aes(y = temperature_mean, colour= DepthBin, group=DepthBin), lwd=1) +
  labs(color='Depth interval') +
  scale_colour_manual(values = c("orange", "dodgerblue2", "lightblue3", "gray65", "gray35")) +
  xlab("\nYear") +
  ylab(expression(paste("Temperature (", degree, "C)"))) +
  theme_bw() +
  scale_x_continuous(breaks=seq(2000,2018, by=4)) +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=14)) +
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.title.y=element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(legend.text=element_text(size=13)) +
  theme(legend.title=element_text(size=14)) +
  theme(legend.position="top")

fig10 <- fig10 + facet_grid(station ~ Season) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust=1)) +
  theme(strip.text.y = element_text(size=14, face="bold", vjust=1)) +
  guides(fill = "none")

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure10.png"), units = "in", width = 10, height = 12, res = 400)

print(fig10)

dev.off()


#SALINITY

fig11 <- ggplot(springfall, aes(x = Year, y = salinity_mean), colour = DepthBin, group = DepthBin) +
  geom_point(aes(y = salinity_mean, colour = DepthBin, group = DepthBin), size = 3) +
  geom_line(aes(y = salinity_mean, colour = DepthBin, group = DepthBin), lwd = 1) +
  labs(color='Depth interval') +
  scale_colour_manual(values = c("orange", "dodgerblue2", "lightblue3", "gray65", "gray35")) +
  xlab("\nYear") +
  ylab("Salinity") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2000,2018, by = 4)) +
  theme(axis.text.y=element_text(size = 13)) +
  theme(axis.text.x=element_text(size = 13)) +
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  theme(axis.title.y=element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(legend.text=element_text(size = 13)) +
  theme(legend.title=element_text(size = 14)) +
  theme(legend.position="top")

fig11 <- fig11 + facet_grid(station~Season) +
  theme(strip.text.x = element_text(size = 14, face = "bold", vjust = 1)) +
  theme(strip.text.y = element_text(size = 14, face = "bold", vjust = 1)) +
  guides(fill = "none")

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure11.png"), units = "in", width = 10, height = 12, res = 400)

print(fig11)

dev.off()


# Extract Vertical Layers of Spring Oxygen Data ------------------------------

#A note from Jeff's code: # Only use data after 2012 because oxygen data prior to 2013 was not corrected.

#Oxygen is a special case because only the data from 2013 were used in Jeff's vertical structure analyses as
#calibration of the sensor using bottle samples started in 2013. The data therefore needs to be parsed to
#only include those casts from 2013 onward.



#Subset spring data to include casts from 2013 onward

Combined_spr2013 <- subset(Combined_spring, Combined_spring$Year >= "2013")


#Top 50 m  (matches layer of biological productivity)
Top50AVG_springoxy <- Combined_spr2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure<=50) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Top50AVG_springoxy

Top50AVG_springoxy$DepthBin <- "0 - 50 m"


#50 to 100 m layer
Lyr50_100AVG_springoxy <- Combined_spr2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure>50 & pressure<=100) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Lyr50_100AVG_springoxy

Lyr50_100AVG_springoxy$DepthBin <- "50 - 100 m"


#100 to 400 m layer
Lyr100_400AVG_springoxy <- Combined_spr2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure>100 & pressure<=400) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Lyr100_400AVG_springoxy

Lyr100_400AVG_springoxy$DepthBin <- "100 - 400 m"


#400 to 750 layer
Lyr400_750AVG_springoxy <- Combined_spr2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure>400 & pressure<=750) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Lyr400_750AVG_springoxy

Lyr400_750AVG_springoxy$DepthBin <- "400 - 750 m"


#750 to bottom layer
Lyr750_bottomAVG_springoxy <- Combined_spr2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure>750 & max(pressure)) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Lyr750_bottomAVG_springoxy

Lyr750_bottomAVG_springoxy$DepthBin <- "750 m - near-bottom"


#Combine 'layers' into one

SpringOxygen_ByDepth<-rbind.fill(Top50AVG_springoxy, Lyr50_100AVG_springoxy,
                                 Lyr100_400AVG_springoxy, Lyr400_750AVG_springoxy,
                                 Lyr750_bottomAVG_springoxy)

#Create a column that denotes year

SpringOxygen_ByDepth$Year <- (substr(SpringOxygen_ByDepth$cruise, 4, 7))  #extracts the year from cruise name. Characters 4 through 7




# Extract Vertical Layers of Fall Oxygen Data ------------------------------


Combined_fall2013 <- subset(Combined_fall, Combined_fall$Year >= "2013")


#Top 50 m  (matches layer of biological productivity)
Top50AVG_falloxy <- Combined_fall2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure<=50) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Top50AVG_falloxy

Top50AVG_falloxy$DepthBin <- "0 - 50 m"


#50 to 100 m layer
Lyr50_100AVG_falloxy <- Combined_fall2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure>50 & pressure<=100) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Lyr50_100AVG_falloxy

Lyr50_100AVG_falloxy$DepthBin <- "50 - 100 m"


#100 to 400 m layer
Lyr100_400AVG_falloxy <- Combined_fall2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure>100 & pressure<=400) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Lyr100_400AVG_falloxy

Lyr100_400AVG_falloxy$DepthBin <- "100 - 400 m"


#400 to 750 layer
Lyr400_750AVG_falloxy <- Combined_fall2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure>400 & pressure<=750) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Lyr400_750AVG_falloxy

Lyr400_750AVG_falloxy$DepthBin <- "400 - 750 m"


#750 to bottom layer
Lyr750_bottomAVG_falloxy <- Combined_fall2013 %>%
  group_by(cruise, station) %>%  #Group by cruise and station - find the data associated with the max pressure value within each cruise/station
  filter(pressure>750 & max(pressure)) %>%
  summarise_at(vars(c("oxygen")), list(mean = mean), na.rm = TRUE)
Lyr750_bottomAVG_falloxy

Lyr750_bottomAVG_falloxy$DepthBin <- "750 m - near-bottom"


FallOxygen_ByDepth <- rbind.fill(Top50AVG_falloxy, Lyr50_100AVG_falloxy,
                                 Lyr100_400AVG_falloxy, Lyr400_750AVG_falloxy,
                                 Lyr750_bottomAVG_falloxy)

FallOxygen_ByDepth$Year <- (substr(FallOxygen_ByDepth$cruise, 4, 7))




# Create Plots of Temporal Changes in Seasonal Temp, Sal ------------------

#Add a column to denote season & combine

SpringOxygen_ByDepth$Season <- "Spring"
FallOxygen_ByDepth$Season <- "Fall"

springfalloxy <- rbind.fill(SpringOxygen_ByDepth, FallOxygen_ByDepth)


#Re-order variables for plotting

springfalloxy$Season <- ordered(springfalloxy$Season, levels = c("Spring", "Fall"))


springfalloxy$station <- ordered(springfalloxy$station, levels = c("GULD_03",
                                                                   "SG_28",
                                                                   "GULD_04",
                                                                   "SG_23"))

springfalloxy$DepthBin <- ordered(springfalloxy$DepthBin, levels = c("0 - 50 m",
                                                                     "50 - 100 m",
                                                                     "100 - 400 m",
                                                                     "400 - 750 m",
                                                                     "750 m - near-bottom"))

#Make year numeric so it can be plotted as a continuous variable
springfalloxy$Year <- as.numeric(springfalloxy$Year)


fig12 <- ggplot(springfalloxy, aes(x = Year, y = mean), colour= DepthBin, group=DepthBin) +
  geom_point(aes(y = mean, colour = DepthBin, group = DepthBin), size = 3) +
  geom_line(aes(y = mean, colour = DepthBin, group = DepthBin), lwd = 1)+
  labs(color='Depth interval')+
  scale_colour_manual(values = c("orange", "dodgerblue2", "lightblue3", "gray65", "gray35")) +
  xlab("\nYear") +
  ylab(expression(paste("Oxygen (ml L"^{-1}, ")"))) +
  theme_bw() +
  scale_x_continuous(breaks=seq(2013,2018, by=2)) +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=14)) +
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.title.y=element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(legend.text=element_text(size=13)) +
  theme(legend.title=element_text(size=14)) +
  theme(legend.position="top")

fig12 <- fig12 + facet_grid(station ~ Season) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust=1)) +
  theme(strip.text.y = element_text(size=14, face="bold", vjust=1)) +
  guides(fill = "none")

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure12.png"), units = "in", width = 10, height = 12, res = 400)

print(fig12)

dev.off()

#A warning will appear to indicate 1 row was removed. This is because the 0-50 m layer for spring GULD3
#(HUD2013004) had no data and is NaN.
