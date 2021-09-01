##############################################################################################
##### FIGURES 18, 19, 20, 21: Temporal Changes in Nutrients via Trapezoidal Integration ######
##############################################################################################

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
library(egg)

#Load .RData containing the nutrient data extracted by Jeff Jackson
projpath <- getwd()
datapath <- paste0(projpath, "/data/2_ChemicalData/")
load(paste0(datapath, "monitoringSitesDIS-complete.RData"))


# Load Nutrient Data from .RData file & Extract Spring/Fall Months --------------------------


# Extract Nutrient Data by Season & Station -------------------------------

#This code extracts both nutrient and chl a ('chem') data for each station/season. The data are filtered later to include only
#nutrients.

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


#From Spring GULD_03, remove the following casts (these do not match the CTD casts analyzed in the report)

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

fallchem$Event <- (str_sub(fallchem$event_id, start=-3))

#Rename year to Year
names(fallchem)[3] <- "Year"


###################################################################
########## Trapezoidal Method for Numerical Integration ###########
###################################################################

#This code was provided by Benoit Casault in Dec. 2020 to calculate the depth integrated values of
#nutrients using the trapezoidal method for numerical integration. It is a way to calculate/interpolate
#nutrient concentration over a depth interval (0-50 m). This method is used by Benoit for AZMP annual reporting


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
depth_range0_50 <- c(0, 50)
depth_range50_250 <- c(50, 250)
depth_range250_400 <- c(250, 400)


GULD3_0_50 <-springfallGULD3 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_0_50=DIS_Integrate_Profile(depth=depth, value=nitrate,
                         nominal_depth=nominal_depthGULD3, depth_range=depth_range0_50),

            phosphate_0_50=DIS_Integrate_Profile(depth=depth, value=phosphate,
                           nominal_depth=nominal_depthGULD3, depth_range=depth_range0_50),

            silicate_0_50=DIS_Integrate_Profile(depth=depth, value=silicate,
                           nominal_depth=nominal_depthGULD3, depth_range=depth_range0_50))


GULD3_50_250 <-springfallGULD3 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_50_250=DIS_Integrate_Profile(depth=depth, value=nitrate,
                           nominal_depth=nominal_depthGULD3, depth_range=depth_range50_250),

            phosphate_50_250=DIS_Integrate_Profile(depth=depth, value=phosphate,
                             nominal_depth=nominal_depthGULD3, depth_range=depth_range50_250),

            silicate_50_250=DIS_Integrate_Profile(depth=depth, value=silicate,
                            nominal_depth=nominal_depthGULD3, depth_range=depth_range50_250))


GULD3_250_400 <-springfallGULD3 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_250_400=DIS_Integrate_Profile(depth=depth, value=nitrate,
                            nominal_depth=nominal_depthGULD3, depth_range=depth_range250_400),

            phosphate_250_400=DIS_Integrate_Profile(depth=depth, value=phosphate,
                             nominal_depth=nominal_depthGULD3, depth_range=depth_range250_400),

            silicate_250_400=DIS_Integrate_Profile(depth=depth, value=silicate,
                            nominal_depth=nominal_depthGULD3, depth_range=depth_range250_400))


#GULD_04

#Combine GULD_04 spring/fall datasets:

springfallGULD4 <- rbind.fill(springGULD4, fallGULD4)

# this would be the bottom depth at the given location
nominal_depthGULD4 <- 2400

# depth ranges for integration
depth_range0_50 <- c(0, 50)
depth_range50_250 <- c(50, 250)
depth_range250_400 <- c(250, 400)


GULD4_0_50 <-springfallGULD4 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_0_50=DIS_Integrate_Profile(depth=depth, value=nitrate,
                        nominal_depth=nominal_depthGULD4, depth_range=depth_range0_50),

            phosphate_0_50=DIS_Integrate_Profile(depth=depth, value=phosphate,
                           nominal_depth=nominal_depthGULD4, depth_range=depth_range0_50),

            silicate_0_50=DIS_Integrate_Profile(depth=depth, value=silicate,
                          nominal_depth=nominal_depthGULD4, depth_range=depth_range0_50))


GULD4_50_250 <-springfallGULD4 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_50_250=DIS_Integrate_Profile(depth=depth, value=nitrate,
                           nominal_depth=nominal_depthGULD4, depth_range=depth_range50_250),

            phosphate_50_250=DIS_Integrate_Profile(depth=depth, value=phosphate,
                             nominal_depth=nominal_depthGULD4, depth_range=depth_range50_250),

            silicate_50_250=DIS_Integrate_Profile(depth=depth, value=silicate,
                            nominal_depth=nominal_depthGULD4, depth_range=depth_range50_250))



GULD4_250_400 <-springfallGULD4 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_250_400=DIS_Integrate_Profile(depth=depth, value=nitrate,
                            nominal_depth=nominal_depthGULD4, depth_range=depth_range250_400),

            phosphate_250_400=DIS_Integrate_Profile(depth=depth, value=phosphate,
                             nominal_depth=nominal_depthGULD4, depth_range=depth_range250_400),

            silicate_250_400=DIS_Integrate_Profile(depth=depth, value=silicate,
                            nominal_depth=nominal_depthGULD4, depth_range=depth_range250_400))


#SG_23

#Combine SG_23 spring/fall datasets:

springfallSG23 <- rbind.fill(springSG_23, fallSG_23)

# this would be the bottom depth at the given location
nominal_depthSG23 <- 1450

# depth ranges for integration
depth_range0_50 <- c(0, 50)
depth_range50_250 <- c(50, 250)
depth_range250_400 <- c(250, 400)

SG23_0_50 <-springfallSG23 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_0_50=DIS_Integrate_Profile(depth=depth, value=nitrate,
                        nominal_depth=nominal_depthSG23, depth_range=depth_range0_50),

            phosphate_0_50=DIS_Integrate_Profile(depth=depth, value=phosphate,
                        nominal_depth=nominal_depthSG23, depth_range=depth_range0_50),

            silicate_0_50=DIS_Integrate_Profile(depth=depth, value=silicate,
                        nominal_depth=nominal_depthSG23, depth_range=depth_range0_50))


SG23_50_250 <-springfallSG23 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_50_250=DIS_Integrate_Profile(depth=depth, value=nitrate,
                           nominal_depth=nominal_depthSG23, depth_range=depth_range50_250),

            phosphate_50_250=DIS_Integrate_Profile(depth=depth, value=phosphate,
                             nominal_depth=nominal_depthSG23, depth_range=depth_range50_250),

            silicate_50_250=DIS_Integrate_Profile(depth=depth, value=silicate,
                            nominal_depth=nominal_depthSG23, depth_range=depth_range50_250))



SG23_250_400 <-springfallSG23 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_250_400=DIS_Integrate_Profile(depth=depth, value=nitrate,
                            nominal_depth=nominal_depthSG23, depth_range=depth_range250_400),

            phosphate_250_400=DIS_Integrate_Profile(depth=depth, value=phosphate,
                             nominal_depth=nominal_depthSG23, depth_range=depth_range250_400),

            silicate_250_400=DIS_Integrate_Profile(depth=depth, value=silicate,
                             nominal_depth=nominal_depthSG23, depth_range=depth_range250_400))


#SG_28

#Combine SG_28 spring/fall datasets:

springfallSG28 <- rbind.fill(springSG_28, fallSG_28)

# this would be the bottom depth at the given location
nominal_depthSG28 <- 1000

# depth ranges for integration. Changing the depth bins so they are mutually exclusive (0-50, 51-250, 251-400) does not change the outcome.
depth_range0_50 <- c(0, 50)
depth_range50_250 <- c(50, 250)
depth_range250_400 <- c(250, 400)


SG28_0_50 <-springfallSG28 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_0_50=DIS_Integrate_Profile(depth=depth, value=nitrate,
                         nominal_depth=nominal_depthSG28, depth_range=depth_range0_50),

            phosphate_0_50=DIS_Integrate_Profile(depth=depth, value=phosphate,
                           nominal_depth=nominal_depthSG28, depth_range=depth_range0_50),

            silicate_0_50=DIS_Integrate_Profile(depth=depth, value=silicate,
                          nominal_depth=nominal_depthSG28, depth_range=depth_range0_50))


SG28_50_250 <-springfallSG28 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_50_250=DIS_Integrate_Profile(depth=depth, value=nitrate,
                           nominal_depth=nominal_depthSG28, depth_range=depth_range50_250),

            phosphate_50_250=DIS_Integrate_Profile(depth=depth, value=phosphate,
                             nominal_depth=nominal_depthSG28, depth_range=depth_range50_250),

            silicate_50_250=DIS_Integrate_Profile(depth=depth, value=silicate,
                            nominal_depth=nominal_depthSG28, depth_range=depth_range50_250))


SG28_250_400 <-springfallSG28 %>%
  group_by(Station, Year, event_id, Season) %>%
  summarise(nitrate_250_400=DIS_Integrate_Profile(depth=depth, value=nitrate,
                            nominal_depth=nominal_depthSG28, depth_range=depth_range250_400),

            phosphate_250_400=DIS_Integrate_Profile(depth=depth, value=phosphate,
                             nominal_depth=nominal_depthSG28, depth_range=depth_range250_400),

            silicate_250_400=DIS_Integrate_Profile(depth=depth, value=silicate,
                             nominal_depth=nominal_depthSG28, depth_range=depth_range250_400))


#Collate the datasets:

NutsInt_0_50 <- bind_rows(GULD3_0_50,
                     GULD4_0_50,
                     SG23_0_50,
                     SG28_0_50)

NutsInt_0_50$DepthInt0_50 <- '0-50 m'

NutsInt_0_50_scaled = NutsInt_0_50

NutsInt_0_50_scaled[,5:7] <-NutsInt_0_50[,5:7] / 50  #Divide by the width of the depth interval (0 - 50 m, so 50 m wide)

NutsInt_50_250 <- bind_rows(GULD3_50_250,
                           GULD4_50_250,
                           SG23_50_250,
                           SG28_50_250)

NutsInt_50_250$DepthInt50_250 <- '50-250 m'

NutsInt_50_250_scaled = NutsInt_50_250

NutsInt_50_250_scaled[,5:7] <-NutsInt_50_250[,5:7] / 200

NutsInt_250_400 <- bind_rows(GULD3_250_400,
                            GULD4_250_400,
                            SG23_250_400,
                            SG28_250_400)

NutsInt_250_400$DepthInt250_400 <- '250-400 m'

NutsInt_250_400_scaled = NutsInt_250_400

NutsInt_250_400_scaled[,5:7] <-NutsInt_250_400[,5:7] / 150

NutsInt <- cbind.data.frame(NutsInt_0_50_scaled, NutsInt_50_250_scaled, NutsInt_250_400_scaled)


#Need to rbind rows, but the column headers need to be consistent

NutsInt_0_50_scaled
NutsInt_50_250_scaled
NutsInt_250_400_scaled

colnames(NutsInt_0_50_scaled)[5] <- "Nitrate"
colnames(NutsInt_50_250_scaled)[5] <- "Nitrate"
colnames(NutsInt_250_400_scaled)[5] <- "Nitrate"

colnames(NutsInt_0_50_scaled)[6] <- "Phosphate"
colnames(NutsInt_50_250_scaled)[6] <- "Phosphate"
colnames(NutsInt_250_400_scaled)[6] <- "Phosphate"

colnames(NutsInt_0_50_scaled)[7] <- "Silicate"
colnames(NutsInt_50_250_scaled)[7] <- "Silicate"
colnames(NutsInt_250_400_scaled)[7] <- "Silicate"

colnames(NutsInt_0_50_scaled)[8] <- "DepthInterval"
colnames(NutsInt_50_250_scaled)[8] <- "DepthInterval"
colnames(NutsInt_250_400_scaled)[8] <- "DepthInterval"

IntegratNuts <- rbind.fill(NutsInt_0_50_scaled, NutsInt_50_250_scaled, NutsInt_250_400_scaled)

saveRDS(NutsInt_0_50_scaled, file = paste0(datapath, "NutsInt_0_50_scaled_Gully.rds"))
saveRDS(NutsInt_50_250_scaled, file = paste0(datapath, "NutsInt_50_250_scaled_Gully.rds"))
saveRDS(NutsInt_250_400_scaled, file = paste0(datapath, "NutsInt_250_400_scaled_Gully.rds"))


#Now, subset by station and generate plots separately for each station:

# GULD_03 Nutrients - Figure 18

GULD_03data <- subset(IntegratNuts, IntegratNuts$Station =="GULD_03")

GULD_03data_complete <-GULD_03data[complete.cases(GULD_03data), ]  #remove NAs

GULD_03data_complete$Season <- ordered(GULD_03data_complete$Season, levels = c("Spring", "Fall"))

GULD_03data_complete$DepthInterval <- ordered(GULD_03data_complete$DepthInterval, levels = c("0-50 m",
                                                                                          "50-250 m",
                                                                                          "250-400 m"))

fig18 <- ggplot(GULD_03data_complete, aes(x=Year)) +
  geom_point(aes(y=Nitrate, color = "dodgerblue2"), size=3) +  #Be careful. It orders the variables by color name
  geom_line(aes(y=Nitrate, color = "dodgerblue2"), lwd=1) +
  geom_point(aes(y=Phosphate*5, color = "gray65"), size=3) +
  geom_line(aes(y=Phosphate*5, color = "gray65"), lwd=1) +
  geom_point(aes(y=Silicate, color = "orange"), size=3) +
  geom_line(aes(y=Silicate, color = "orange"), lwd=1) +
  scale_color_identity(guide = "legend", name=NULL, labels = c("Nitrate", "Phosphate", "Silicate")) +
  guides(colour = guide_legend(label.theme = element_text(size=15))) +
  scale_y_continuous(sec.axis = sec_axis(~./5, name=expression(paste("\nPhosphate concentration  ", (mmol~m^{-3}))))) +
  theme_bw() +
  ylab(expression(paste("\nNitrate & silicate concentration  ", (mmol~m^{-3})))) +
  xlab("Year\n") +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=14)) +
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)))+
  theme(axis.title.y.right = element_text(vjust=2.6))+
  theme(legend.position = "top")

fig18 <- fig18 + facet_grid(DepthInterval ~Season) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust = 1))+
  theme(strip.text.y = element_blank())+
  guides(fill=FALSE)

#From library(egg)

my_tag <- c("0 - 50 m", "", "50 - 250 m", "", "250 - 400 m", "")

fig18 <- tag_facet(fig18,
                   x = -Inf, y = -Inf,
                   vjust = -14, hjust = -0.15,
                   open = "", close = "",
                   fontface = 4,
                   size = 5,
                   family = "serif",
                   tag_pool = my_tag)

fig18 <- fig18 + theme(strip.text = element_text(margin = margin(1,0,1,0, "mm")))

fig18 <- fig18 + theme(strip.background = element_rect(fill = "grey85"))

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure18.png"), units = "in", width = 8, height = 8, res = 400)

print(fig18)

dev.off()


# SG_28 Nutrients - Figure 19

SG_28data <- subset(IntegratNuts, IntegratNuts$Station =="SG_28")

SG_28data_complete <-SG_28data[complete.cases(SG_28data), ]  #remove NAs

SG_28data_complete$Season<-ordered(SG_28data_complete$Season, levels = c("Spring", "Fall"))

SG_28data_complete$DepthInterval<-ordered(SG_28data_complete$DepthInterval, levels = c("0-50 m",
                                                                    "50-250 m",
                                                                   "250-400 m"))

fig19 <- ggplot(SG_28data_complete, aes(x=Year)) +
  geom_point(aes(y=Nitrate, color = "dodgerblue2"), size=3) +  #Be careful. It orders the variables by color name
  geom_line(aes(y=Nitrate, color = "dodgerblue2"), lwd=1) +
  geom_point(aes(y=Phosphate*5, color = "gray65"), size=3) +
  geom_line(aes(y=Phosphate*5, color = "gray65"), lwd=1) +
  geom_point(aes(y=Silicate, color = "orange"), size=3) +
  geom_line(aes(y=Silicate, color = "orange"), lwd=1) +
  scale_color_identity(guide = "legend", name=NULL, labels = c("Nitrate", "Phosphate", "Silicate")) +
  guides(colour = guide_legend(label.theme = element_text(size=15))) +
  scale_y_continuous(sec.axis = sec_axis(~./5, name=expression(paste("\nPhosphate concentration  ", (mmol~m^{-3}))))) +
  theme_bw() +
  ylab(expression(paste("\nNitrate & silicate concentration  ", (mmol~m^{-3})))) +
  xlab("Year\n") +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=14)) +
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)))+
  theme(axis.title.y.right = element_text(vjust=2.6))+
  theme(legend.position = "top")

fig19 <- fig19 + facet_grid(DepthInterval ~Season) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust = 1))+
  theme(strip.text.y = element_blank())+
  guides(fill=FALSE)

my_tag <- c("0 - 50 m", "", "50 - 250 m", "", "250 - 400 m", "")

fig19 <- tag_facet(fig19,
                   x = -Inf, y = -Inf,
                   vjust = -14, hjust = -0.15,
                   open = "", close = "",
                   fontface = 4,
                   size = 5,
                   family = "serif",
                   tag_pool = my_tag)

fig19 <- fig19 + theme(strip.text = element_text(margin = margin(1,0,1,0, "mm")))

fig19 <- fig19 + theme(strip.background = element_rect(fill = "grey85"))

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure19.png"), units = "in", width = 8, height = 8, res = 400)

print(fig19)

dev.off()


# GULD_04 Nutrients - Figure 20

GULD_04data <- subset(IntegratNuts, IntegratNuts$Station == "GULD_04")

GULD_04data_complete <-GULD_04data[complete.cases(GULD_04data), ]  #remove NAs

GULD_04data_complete$Season<-ordered(GULD_04data_complete$Season, levels = c("Spring", "Fall"))

GULD_04data_complete$DepthInterval<-ordered(GULD_04data_complete$DepthInterval, levels = c("0-50 m",
                                                                                          "50-250 m",
                                                                                          "250-400 m"))

fig20 <- ggplot(GULD_04data_complete, aes(x=Year)) +
  geom_point(aes(y=Nitrate, color = "dodgerblue2"), size=3) +  #Be careful. It orders the variables by color name
  geom_line(aes(y=Nitrate, color = "dodgerblue2"), lwd=1) +
  geom_point(aes(y=Phosphate*5, color = "gray65"), size=3) +
  geom_line(aes(y=Phosphate*5, color = "gray65"), lwd=1) +
  geom_point(aes(y=Silicate, color = "orange"), size=3) +
  geom_line(aes(y=Silicate, color = "orange"), lwd=1) +
  scale_color_identity(guide = "legend", name=NULL, labels = c("Nitrate", "Phosphate", "Silicate")) +
  guides(colour = guide_legend(label.theme = element_text(size=15))) +
  scale_y_continuous(sec.axis = sec_axis(~./5, name=expression(paste("\nPhosphate concentration  ", (mmol~m^{-3}))))) +
  theme_bw() +
  ylab(expression(paste("\nNitrate & silicate concentration  ", (mmol~m^{-3})))) +
  xlab("Year\n") +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=14)) +
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)))+
  theme(axis.title.y.right = element_text(vjust=2.6))+
  theme(legend.position = "top")

fig20 <- fig20 + facet_grid(DepthInterval ~Season) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust = 1))+
  theme(strip.text.y = element_blank())+
  guides(fill=FALSE)

my_tag <- c("0 - 50 m", "", "50 - 250 m", "", "250 - 400 m", "")

fig20 <- tag_facet(fig20,
                   x = -Inf, y = -Inf,
                   vjust = -14, hjust = -0.15,
                   open = "", close = "",
                   fontface = 4,
                   size = 5,
                   family = "serif",
                   tag_pool = my_tag)
fig20 <- fig20 + theme(strip.text = element_text(margin = margin(1,0,1,0, "mm")))
fig20 <- fig20 + theme(strip.background = element_rect(fill = "grey85"))

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure20.png"), units = "in", width = 8, height = 8, res = 400)

print(fig20)

dev.off()


# SG_23 Nutrients - Figure 21

SG_23data <- subset(IntegratNuts, IntegratNuts$Station =="SG_23")

SG_23data_complete <-SG_23data[complete.cases(SG_23data), ]  #remove NAs

SG_23data_complete$Season<-ordered(SG_23data_complete$Season, levels = c("Spring", "Fall"))

SG_23data_complete$DepthInterval<-ordered(SG_23data_complete$DepthInterval, levels = c("0-50 m",
                                                                                       "50-250 m",
                                                                                       "250-400 m"))

fig21 <- ggplot(SG_23data_complete, aes(x=Year)) +
  geom_point(aes(y=Nitrate, color = "dodgerblue2"), size=3) +  #Be careful. It orders the variables by color name
  geom_line(aes(y=Nitrate, color = "dodgerblue2"), lwd=1) +
  geom_point(aes(y=Phosphate*5, color = "gray65"), size=3) +
  geom_line(aes(y=Phosphate*5, color = "gray65"), lwd=1) +
  geom_point(aes(y=Silicate, color = "orange"), size=3) +
  geom_line(aes(y=Silicate, color = "orange"), lwd=1) +
  scale_color_identity(guide = "legend", name=NULL, labels = c("Nitrate", "Phosphate", "Silicate")) +
  guides(colour = guide_legend(label.theme = element_text(size=15))) +
  scale_y_continuous(sec.axis = sec_axis(~./5, name=expression(paste("\nPhosphate concentration  ", (mmol~m^{-3}))))) +
  theme_bw() +
  ylab(expression(paste("\nNitrate & silicate concentration  ", (mmol~m^{-3})))) +
  xlab("Year\n") +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=14)) +
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)))+
  theme(axis.title.y.right = element_text(vjust=2.6))+
  theme(legend.position = "top")

fig21 <- fig21 + facet_grid(DepthInterval ~Season) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust = 1))+
  theme(strip.text.y = element_blank())+
  guides(fill=FALSE)

my_tag <- c("0 - 50 m", "", "50 - 250 m", "", "250 - 400 m", "")

fig21 <- tag_facet(fig21,
                   x = -Inf, y = -Inf,
                   vjust = -14, hjust = -0.15,
                   open = "", close = "",
                   fontface = 4,
                   size = 5,
                   family = "serif",
                   tag_pool = my_tag)
fig21 <- fig21 + theme(strip.text = element_text(margin = margin(1,0,1,0, "mm")))
fig21 <- fig21 + theme(strip.background = element_rect(fill = "grey85"))

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure21.png"), units = "in", width = 8, height = 8, res = 400)

print(fig21)

dev.off()


# -------------------------------------------------------------------------

#Linear regression

IntegratNuts

#GULD3

IntNutsG3 <- subset(IntegratNuts, IntegratNuts$Station == "GULD_03")
IntNutsG3_250_400 <- subset(IntNutsG3, IntNutsG3$DepthInterval == "250-400 m")
IntNutsG3_250_400spr <- subset(IntNutsG3_250_400, IntNutsG3_250_400$Season == "Spring")
IntNutsG3_250_400fall <- subset(IntNutsG3_250_400, IntNutsG3_250_400$Season == "Fall")

spring250_400regressGULD3 = lm(Nitrate ~ Year, data=IntNutsG3_250_400spr)
summary(spring250_400regressGULD3)  #Significant t-value = 4.381, p = 0.00177, R2 multi = 0.6807.

fall250_400regressGULD3 = lm(Nitrate ~ Year, data=IntNutsG3_250_400fall)
summary(fall250_400regressGULD3)  #Significant  tvalue=2.484, P = 0.0379, R2 multiple = 0.4355


#GULD4

IntNutsG4 <- subset(IntegratNuts, IntegratNuts$Station == "GULD_04")
IntNutsG4_250_400 <- subset(IntNutsG4, IntNutsG4$DepthInterval == "250-400 m")
IntNutsG4_250_400spr <- subset(IntNutsG4_250_400, IntNutsG4_250_400$Season == "Spring")
IntNutsG4_250_400fall <- subset(IntNutsG4_250_400, IntNutsG4_250_400$Season == "Fall")

spring250_400regressGULD4 = lm(Nitrate ~ Year, data=IntNutsG4_250_400spr)
summary(spring250_400regressGULD4)  #Not significant

fall250_400regressGULD4 = lm(Nitrate ~ Year, data=IntNutsG4_250_400fall)
summary(fall250_400regressGULD4)  #Not significant


#SG28

IntNutsSG28 <- subset(IntegratNuts, IntegratNuts$Station == "SG_28")
IntNutsSG28_250_400 <- subset(IntNutsSG28, IntNutsSG28$DepthInterval == "250-400 m")
IntNutsSG28_250_400spr <- subset(IntNutsSG28_250_400, IntNutsSG28_250_400$Season == "Spring")
IntNutsSG28_250_400fall <- subset(IntNutsSG28_250_400, IntNutsSG28_250_400$Season == "Fall")

spring250_400regressSG28 = lm(Nitrate ~ Year, data=IntNutsSG28_250_400spr)
summary(spring250_400regressSG28)  #Not significant

fall250_400regressSG28 = lm(Nitrate ~ Year, data=IntNutsSG28_250_400fall)
summary(fall250_400regressSG28)  #Not significant
