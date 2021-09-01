############################################################################################
##### FIGURE 24: Temporal Changes in Chlorophyll via Trapezoidal Numerical Integration #####
############################################################################################

# Last updated: 31-AUG-2021
# Author: Lindsay Beazley
# Modified by: Jeff Jackson

#Load packages

library(oce)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(tidyr)
library(pracma)
library(stringr)


#Load chemical data provided by Jeff Jackson
projpath <- getwd()
datapath <- paste0(projpath, "/data/2_ChemicalData/")
load(paste0(datapath, "monitoringSitesDIS-complete.RData"))


# Extract Chlorophyll Data by Season & Station -------------------------------

#This code extracts both nutrient and chl a ('chem') data for each station/season. The data are filtered later to include only chl.

chemSpringGuld03 <- subset(guld03_disData, month == 4)  #19 unique event_id. Only 12 CTD casts.
chemSpringGuld04 <- subset(guld04_disData, month == 4)  #11 unique event_id. Only 10 CTD casts.
chemSpringSG23 <- subset(sg23_disData, month == 4)      #9 unique event_id. Only 7 CTD casts.
chemSpringSG28 <- subset(sg28_disData, month == 4)      #10 unique event_id. Only 8 CTD casts.


chemFallGuld03 <- subset(guld03_disData, month > 8 & month < 11)  #16 unique event_id. Only 15 CTD casts.
chemFallGuld04 <- subset(guld04_disData, month > 8 & month < 11)  #12 unique event_id. Only 11 CTD casts.
chemFallSG23 <- subset(sg23_disData, month > 8 & month < 11)      #10 unique event_id. Only 9 CTD casts.
chemFallSG28 <- subset(sg28_disData, month > 8 & month < 11)      #10 unique event_id. 10 CTD casts.


#Add station name

chemSpringGuld03$Station <- "GULD_03"
chemSpringGuld04$Station <- "GULD_04"
chemSpringSG23$Station <- "SG_23"
chemSpringSG28$Station <- "SG_28"

chemFallGuld03$Station <- "GULD_03"
chemFallGuld04$Station <- "GULD_04"
chemFallSG23$Station <- "SG_23"
chemFallSG28$Station <- "SG_28"


#E.g. to find unique combination of cruise and events
#unique(SG28falldf[,c('cruise','event')])


#From Spring GULD_03, remove the following casts:

chemSpringGuld03_v2<-subset(chemSpringGuld03, event_id != "18PZ00002_2000002202" &
                                   event_id != "18HU06008_2006008027" &
                                   event_id != "18HU08004_2008004070" &
                                   event_id != "18HU06008_2006008028" &
                                   event_id != "18HU04009_2004009091" &
                                   event_id != "18HU06008_2006008029" &
                                   event_id != "18HU10006_2010006177")

#Evaluation of the data revealed that 18HU04009_2004009094 consists of only 1 entry, at the surface. This should be removed.
chemSpringGuld03_v3<-subset(chemSpringGuld03_v2, event_id != "18HU04009_2004009094")


#From Spring GULD_04, remove the following casts:
chemSpringGuld04_v2<-subset(chemSpringGuld04, event_id != "18OL17001_087") #surface cast

#From Spring SG_23, remove the following casts:
chemSpringSG23_v2<-subset(chemSpringSG23, event_id != "18OL17001_090" & #surface cast
                                 event_id != "18HU10006_2010006183") #not present in CTD data

#From Spring SG_28, remove the following casts:
chemSpringSG28_v2<-subset(chemSpringSG28, event_id != "18OL17001_080" & #surface cast
                                 event_id != "18HU10006_2010006187") #Not present in CTD data

#From Fall GULD_03, remove the following casts:
chemFallGuld03_v2<-subset(chemFallGuld03, event_id != "18HU00050_2000050289" &
                                 event_id != "18HU01061_2001061179")

#From Fall GULD_04, remove the following casts:
chemFallGuld04_v2<-subset(chemFallGuld04, event_id != "18HU00050_2000050289")

#From Fall SG_23, remove the following casts:
chemFallSG23_v2<-subset(chemFallSG23, event_id != "18HU07045_2007045130")

#Match between CTD and nutrient data for fall SG_28

chemFallSG28


#Collate data

springchem <- rbind.fill(chemSpringGuld03_v3, chemSpringGuld04_v2, chemSpringSG23_v2,
                         chemSpringSG28_v2)

springchem


#create a proper event ID field:

springchem$Event <- (str_sub(springchem$event_id, start=-3))

#Rename year to Year
names(springchem)[3] <- "Year"


#Collate data

fallchem <- rbind.fill(chemFallGuld03_v2, chemFallGuld04_v2, chemFallSG23_v2,
                       chemFallSG28)

fallchem


#create a proper event ID field:

library(stringr)

fallchem$Event <- (str_sub(fallchem$event_id, start=-3))

#Rename year to Year
names(fallchem)[3] <- "Year"


###################################################################
########## Trapezoidal Method for Numerical Integration ###########
###################################################################

#This code was provided by Benoit Casault in Dec. 2020 to calculate the depth integrated values of
#chl using the trapezoidal method for numerical integration. It is a way to calculate/interpolate
#chlorophyll concentration over a depth interval (0-50 m). This method is used by Benoit for AZMP reporting

#Benoit's code. Load custom functions first:

# custom functions

# wrapper function
DIS_Integrate_Profile <- function(depth, value, nominal_depth, depth_range) {

	# Last update: 20141101
	# Benoit.Casault@dfo-mpo.gc.ca

	# required package
	library(pracma)

	# order depth_range
	depth_range <- c(min(depth_range), max(depth_range))

	# check upper integration limit vs nominal depth
	if (depth_range[1] > nominal_depth) {
		return(NA)
	}
	# check lower integration limit vs nominal depth
	if (depth_range[2] > nominal_depth) {
		depth_range[2] <- nominal_depth
	}
	rm(nominal_depth)

	# remove NaN from data
	na_index <- is.na(value)
	if (any(na_index)) {
		depth <- depth[!na_index]
		value <- value[!na_index]
	}

	if (length(value)<=1) {
		return(NA)
	} else {
		# extrapolate profile
		# at the top
		index <- which.min(depth)
		min_depth <- depth[index]
		if (depth_range[2] < min_depth) {
			return(NA)
		} else  if (depth_range[1] < min_depth) {
			depth <- c(depth, depth_range[1])
			value <- c(value, value[index])
		}
		# at the bottom
		index <- which.max(depth)
		nominal_depth <- depth[index]
		if (depth_range[1] > nominal_depth) {
			return(NA)
		} else if (depth_range[2] > nominal_depth) {
			depth <- c(depth, depth_range[2])
			value <- c(value, value[index])
		}

		# interpolate profile
		index <- !(depth_range %in% depth)
		if (any(index)) {
			tmp_depth <- depth_range[index]
			tmp_value <- interp1(depth, value, tmp_depth, method="linear")
			depth <- c(depth, tmp_depth)
			value <- c(value, tmp_value)
		}

		# sort the profile
		index <- order(depth)
		depth <- depth[index]
		value <- value[index]

		# calculate integration
		index <- depth>=depth_range[1] & depth<=depth_range[2]
		return(Trapz(depth[index],value[index]))
	}
}

# trapezoidal integration function
Trapz <- function(x, y) {

	# Integral of Y with respect to X using the trapezoidal rule.
	#
	# Input: x: Sorted vector of x-axis values - numeric vector
	#        y:	Vector of y-axis values - numeric vector
	#
	# Output: Integral of Y with respect to X - numeric scalar
	#
	# Last update: 20141101
	# Benoit.Casault@dfo-mpo.gc.ca

	idx = 2:length(x)
	return(as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}

#-------------------------------------------------------------------------------


#Add fields to denote season:

springchem$Season <- "Spring"
fallchem$Season <- "Fall"


#Subset by station - spring:

springGULD3 <- subset(springchem, Station == "GULD_03")
springGULD4 <- subset(springchem, Station == "GULD_04")
springSG_23 <- subset(springchem, Station == "SG_23")
springSG_28 <- subset(springchem, Station == "SG_28")

#Subset by station - fall:

fallGULD3 <- subset(fallchem, Station == "GULD_03")
fallGULD4 <- subset(fallchem, Station == "GULD_04")
fallSG_23 <- subset(fallchem, Station == "SG_23")
fallSG_28 <- subset(fallchem, Station == "SG_28")


#GULD_03

#Combine GULD_03 spring/fall datasets:

springfallGULD3 <- rbind.fill(springGULD3, fallGULD3)

# this would be the bottom depth at the given location
nominal_depthGULD3 <- 585

# depth ranges for integration
depth_range0_100 <- c(0, 100)

GULD3_0_100 <-springfallGULD3 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(chlorophylla_0_100=DIS_Integrate_Profile(depth=depth, value=chlorophyll_a,
            nominal_depth=nominal_depthGULD3, depth_range=depth_range0_100))


#GULD_04

#Combine GULD_04 spring/fall datasets:

springfallGULD4 <- rbind.fill(springGULD4, fallGULD4)

# this would be the bottom depth at the given location
nominal_depthGULD4 <- 2400

# depth ranges for integration
depth_range0_100 <- c(0, 100)


GULD4_0_100 <-springfallGULD4 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(chlorophylla_0_100=DIS_Integrate_Profile(depth=depth, value=chlorophyll_a,
                               nominal_depth=nominal_depthGULD4, depth_range=depth_range0_100))


#SG_23

#Combine SG_23 spring/fall datasets:

springfallSG23 <- rbind.fill(springSG_23, fallSG_23)

# this would be the bottom depth at the given location
nominal_depthSG23 <- 1450

# depth ranges for integration
depth_range0_100 <- c(0, 100)


SG23_0_100 <-springfallSG23 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(chlorophylla_0_100=DIS_Integrate_Profile(depth=depth, value=chlorophyll_a,
                                                     nominal_depth=nominal_depthSG23, depth_range=depth_range0_100))


#SG_28

#Combine SG_28 spring/fall datasets:

springfallSG28 <- rbind.fill(springSG_28, fallSG_28)

# this would be the bottom depth at the given location
nominal_depthSG28 <- 1000

# depth ranges for integration
depth_range0_100 <- c(0, 100)


SG28_0_100 <- springfallSG28 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(chlorophylla_0_100=DIS_Integrate_Profile(depth=depth, value=chlorophyll_a,
                                                     nominal_depth=nominal_depthSG28, depth_range=depth_range0_100))


#Collate the datasets:

CHLInt_0_100 <- bind_rows(GULD3_0_100,
                     GULD4_0_100,
                     SG23_0_100,
                     SG28_0_100)

CHLInt_0_100$DepthInt0_100 <- '0-100 m'

CHLInt_0_100_scaled <- CHLInt_0_100

# CHLInt_0_100_scaled[,5] <- CHLInt_0_100[,5] / 100

CHLInt_0_100_complete <- CHLInt_0_100_scaled[complete.cases(CHLInt_0_100_scaled), ]  #remove NAs


CHLInt_0_100_complete$Season <- ordered(CHLInt_0_100_complete$Season, levels = c("Spring", "Fall"))

CHLInt_0_100_complete$Station <- ordered(CHLInt_0_100_complete$Station, levels = c("GULD_03",
                                                                   "SG_28",
                                                                   "GULD_04",
                                                                   "SG_23"))

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure24.png"), units = "in", width = 8, height = 8, res = 400)

fig24 <- ggplot(CHLInt_0_100_complete, aes(x=Year)) +
  geom_point(aes(y=chlorophylla_0_100, color = "dodgerblue2"), size=3) +  #Be careful. It orders the variables by color name
  geom_line(aes(y=chlorophylla_0_100, color = "dodgerblue2"), lwd=1) +
  scale_color_identity(guide = "legend", name=NULL, labels = c("Chlorophyll a")) +
  guides(colour = guide_legend(label.theme = element_text(size=15))) +
  theme_bw() +
  ylab(expression(paste("\nChlorophyll ", italic("a"), " concentration  ",  (mg~m^{-3})))) +
  xlab("Year\n") +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=14)) +
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)))+
  theme(axis.title.y.right = element_text(vjust=2.6))+
  theme(legend.position = 'none')

fig24 <- fig24 + facet_grid(Station ~Season) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust = 1)) +
  theme(strip.text.y = element_text(size=14, face="bold", vjust = 1)) +
  guides(fill=FALSE)

print(fig24)

dev.off()
