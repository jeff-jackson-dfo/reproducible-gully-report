######################################################################################
##### FIGURE 22: Seasonal Patterns in Integrated Nutrients - HL6, LL7 vs. Gully ######
######################################################################################

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


#Load nutrient data for HL6 and LL7 that were filtered in Microsoft Access

projpath <- getwd()
datapath <- paste0(projpath, "/data/2_ChemicalData/")
HL6_LL7 <- read.csv(file=paste0(datapath, "Nutrients_Associated_HL_LL_AnalyzedCTDs.csv"), header = T)


# Extract Nutrient Data by Season & Station -------------------------------

nutrientsSpringHL6 <- subset(HL6_LL7, HL6_LL7$Season == "Spring"
                             & HL6_LL7$Station_CTD == "HL_06")
unique(nutrientsSpringHL6$event_id)  #10 unique event ID

nutrientsFallHL6 <- subset(HL6_LL7, HL6_LL7$Season == "Fall"
                             & HL6_LL7$Station_CTD == "HL_06")
unique(nutrientsFallHL6$event_id)  #16 unique event ID

nutrientsSpringLL7 <- subset(HL6_LL7, HL6_LL7$Season == "Spring"
                             & HL6_LL7$Station_CTD == "LL_07")
unique(nutrientsSpringLL7$event_id)  #13 unique event ID

nutrientsFallLL7 <- subset(HL6_LL7, HL6_LL7$Season == "Fall"
                           & HL6_LL7$Station_CTD == "LL_07")
unique(nutrientsFallLL7$event_id)  #14 unique event ID


###################################################################
########## Trapezoidal Method for Numerical Integration ###########
###################################################################

#This code was provided by Benoit Casault in Dec. 2020 to calculate the depth integrated values of
#nutrients using the trapezoidal method for numerical integration. It is a way to calculate/interpolate
#nutrient concentration over a depth interval (0-50 m). This method is used by Benoit for AZMP reporting

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


#HL_06

#Combine HL_06 spring/fall datasets:

springfallHL6 <- rbind.fill(nutrientsSpringHL6, nutrientsFallHL6)

# this would be the bottom depth at the given location
nominal_depthHL6 <- 1100

# depth ranges for integration
depth_range0_50 <- c(0, 50)
depth_range50_250 <- c(50, 250)
depth_range250_400 <- c(250, 400)

HL6_0_50 <-springfallHL6 %>%
  group_by(Station_CTD, Year, event_id, Season) %>%
  summarise(nitrate_0_50=DIS_Integrate_Profile(depth=depth, value=Nitrate,
                         nominal_depth=nominal_depthHL6, depth_range=depth_range0_50),

            phosphate_0_50=DIS_Integrate_Profile(depth=depth, value=Phosphate,
                           nominal_depth=nominal_depthHL6, depth_range=depth_range0_50),

            silicate_0_50=DIS_Integrate_Profile(depth=depth, value=Silicate,
                           nominal_depth=nominal_depthHL6, depth_range=depth_range0_50))

HL6_50_250 <-springfallHL6 %>%
  group_by(Station_CTD, Year, event_id, Season) %>%
  summarise(nitrate_50_250=DIS_Integrate_Profile(depth=depth, value=Nitrate,
                           nominal_depth=nominal_depthHL6, depth_range=depth_range50_250),

            phosphate_50_250=DIS_Integrate_Profile(depth=depth, value=Phosphate,
                             nominal_depth=nominal_depthHL6, depth_range=depth_range50_250),

            silicate_50_250=DIS_Integrate_Profile(depth=depth, value=Silicate,
                            nominal_depth=nominal_depthHL6, depth_range=depth_range50_250))

HL6_250_400 <-springfallHL6 %>%
  group_by(Station_CTD, Year, event_id, Season) %>%
  summarise(nitrate_250_400=DIS_Integrate_Profile(depth=depth, value=Nitrate,
                            nominal_depth=nominal_depthHL6, depth_range=depth_range250_400),

            phosphate_250_400=DIS_Integrate_Profile(depth=depth, value=Phosphate,
                             nominal_depth=nominal_depthHL6, depth_range=depth_range250_400),

            silicate_250_400=DIS_Integrate_Profile(depth=depth, value=Silicate,
                            nominal_depth=nominal_depthHL6, depth_range=depth_range250_400))


#LL_07

#Combine LL_07 spring/fall datasets:

springfallLL7 <- rbind.fill(nutrientsSpringLL7, nutrientsFallLL7)

# this would be the bottom depth at the given location
nominal_depthLL7 <- 760

# depth ranges for integration
depth_range0_50 <- c(0, 50)
depth_range50_250 <- c(50, 250)
depth_range250_400 <- c(250, 400)


LL7_0_50 <-springfallLL7 %>%
  group_by(Station_CTD, Year, event_id, Season) %>%
  summarise(nitrate_0_50=DIS_Integrate_Profile(depth=depth, value=Nitrate,
                                               nominal_depth=nominal_depthLL7, depth_range=depth_range0_50),

            phosphate_0_50=DIS_Integrate_Profile(depth=depth, value=Phosphate,
                                                 nominal_depth=nominal_depthLL7, depth_range=depth_range0_50),

            silicate_0_50=DIS_Integrate_Profile(depth=depth, value=Silicate,
                                                nominal_depth=nominal_depthLL7, depth_range=depth_range0_50))

LL7_50_250 <-springfallLL7 %>%
  group_by(Station_CTD, Year, event_id, Season) %>%
  summarise(nitrate_50_250=DIS_Integrate_Profile(depth=depth, value=Nitrate,
                                                 nominal_depth=nominal_depthLL7, depth_range=depth_range50_250),

            phosphate_50_250=DIS_Integrate_Profile(depth=depth, value=Phosphate,
                                                   nominal_depth=nominal_depthLL7, depth_range=depth_range50_250),

            silicate_50_250=DIS_Integrate_Profile(depth=depth, value=Silicate,
                                                  nominal_depth=nominal_depthLL7, depth_range=depth_range50_250))


LL7_250_400 <-springfallLL7 %>%
  group_by(Station_CTD, Year, event_id, Season) %>%
  summarise(nitrate_250_400=DIS_Integrate_Profile(depth=depth, value=Nitrate,
                                                  nominal_depth=nominal_depthLL7, depth_range=depth_range250_400),

            phosphate_250_400=DIS_Integrate_Profile(depth=depth, value=Phosphate,
                                                    nominal_depth=nominal_depthLL7, depth_range=depth_range250_400),

            silicate_250_400=DIS_Integrate_Profile(depth=depth, value=Silicate,
                                                   nominal_depth=nominal_depthLL7, depth_range=depth_range250_400))


#Collate the datasets:

NutsInt_0_50 <- bind_rows(HL6_0_50, LL7_0_50)
NutsInt_0_50$DepthInt0_50 <- '0-50 m'

NutsInt_0_50_scaled = NutsInt_0_50

NutsInt_0_50_scaled[,5:7] <-NutsInt_0_50[,5:7] / 50

NutsInt_50_250 <- bind_rows(HL6_50_250, LL7_50_250)
NutsInt_50_250$DepthInt50_250 <- '50-250 m'

NutsInt_50_250_scaled = NutsInt_50_250

NutsInt_50_250_scaled[,5:7] <-NutsInt_50_250[,5:7] / 200

NutsInt_250_400 <- bind_rows(HL6_250_400, LL7_250_400)
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


#0 - 50 m
StationAVG_0_50 <- NutsInt_0_50_scaled %>%
  group_by(Station_CTD, Season) %>%
  summarise_at(vars(c("Nitrate", "Phosphate", "Silicate")), funs(mean,sd), na.rm = TRUE)
StationAVG_0_50

#50 - 250 m
StationAVG_50_250 <- NutsInt_50_250_scaled %>%
  group_by(Station_CTD, Season) %>%
  summarise_at(vars(c("Nitrate", "Phosphate", "Silicate")), funs(mean,sd), na.rm = TRUE)
StationAVG_50_250

#250 - 400 m
StationAVG_250_400 <- NutsInt_250_400_scaled %>%
  group_by(Station_CTD, Season) %>%
  summarise_at(vars(c("Nitrate", "Phosphate", "Silicate")), funs(mean,sd), na.rm = TRUE)
StationAVG_250_400


#Use gather to create the mean of nutrient concentration across years within a station and season

Nuts_0_50_mean <- StationAVG_0_50 %>%
  gather(Variable, nutrient_mean, contains("mean")) %>%
  select(-c(Nitrate_sd, Phosphate_sd, Silicate_sd))
Nuts_0_50_mean
Nuts_0_50_mean$DepthInterval <- "0 - 50 m"

Nuts_50_250_mean <- StationAVG_50_250 %>%
  gather(Variable, nutrient_mean, contains("mean")) %>%
  select(-c(Nitrate_sd, Phosphate_sd, Silicate_sd))
Nuts_50_250_mean
Nuts_50_250_mean$DepthInterval <- "50 - 250 m"

Nuts_250_400_mean <- StationAVG_250_400 %>%
  gather(Variable, nutrient_mean, contains("mean")) %>%
  select(-c(Nitrate_sd, Phosphate_sd, Silicate_sd))
Nuts_250_400_mean
Nuts_250_400_mean$DepthInterval <- "250 - 400 m"

Nuts_0_50_sd <- StationAVG_0_50 %>%
  gather(Variable, nutrient_sd, contains("sd")) %>%
  select(-c(Nitrate_mean, Phosphate_mean, Silicate_mean))
Nuts_0_50_sd
Nuts_0_50_sd$DepthInterval <- "0 - 50 m"

Nuts_50_250_sd <- StationAVG_50_250 %>%
  gather(Variable, nutrient_sd, contains("sd")) %>%
  select(-c(Nitrate_mean, Phosphate_mean, Silicate_mean))
Nuts_50_250_sd
Nuts_50_250_sd$DepthInterval <- "50 - 250 m"

Nuts_250_400_sd <- StationAVG_250_400 %>%
  gather(Variable, nutrient_sd, contains("sd")) %>%
  select(-c(Nitrate_mean, Phosphate_mean, Silicate_mean))
Nuts_250_400_sd
Nuts_250_400_sd$DepthInterval <- "250 - 400 m"


Nuts_0_50_mean_sd <-cbind.data.frame(Nuts_0_50_mean$Station_CTD, Nuts_0_50_mean$Season,
                                    Nuts_0_50_mean$Variable, Nuts_0_50_mean$nutrient_mean,
                                    Nuts_0_50_sd$nutrient_sd, Nuts_0_50_mean$DepthInterval)

Nuts_50_250_mean_sd <-cbind.data.frame(Nuts_50_250_mean$Station_CTD, Nuts_50_250_mean$Season,
                                     Nuts_50_250_mean$Variable, Nuts_50_250_mean$nutrient_mean,
                                     Nuts_50_250_sd$nutrient_sd, Nuts_50_250_mean$DepthInterval)

Nuts_250_400_mean_sd <-cbind.data.frame(Nuts_250_400_mean$Station_CTD, Nuts_250_400_mean$Season,
                                       Nuts_250_400_mean$Variable, Nuts_250_400_mean$nutrient_mean,
                                       Nuts_250_400_sd$nutrient_sd, Nuts_250_400_mean$DepthInterval)

names(Nuts_0_50_mean_sd)[names(Nuts_0_50_mean_sd) == "Nuts_0_50_mean$Station_CTD"] <- "Station"
names(Nuts_0_50_mean_sd)[names(Nuts_0_50_mean_sd) == "Nuts_0_50_mean$Season"] <- "Season"
names(Nuts_0_50_mean_sd)[names(Nuts_0_50_mean_sd) == "Nuts_0_50_mean$Variable"] <- "Nutrient"
names(Nuts_0_50_mean_sd)[names(Nuts_0_50_mean_sd) == "Nuts_0_50_mean$nutrient_mean"] <- "Mean"
names(Nuts_0_50_mean_sd)[names(Nuts_0_50_mean_sd) == "Nuts_0_50_sd$nutrient_sd"] <- "SD"
names(Nuts_0_50_mean_sd)[names(Nuts_0_50_mean_sd) == "Nuts_0_50_mean$DepthInterval"] <- "DepthInterval"

names(Nuts_50_250_mean_sd)[names(Nuts_50_250_mean_sd) == "Nuts_50_250_mean$Station_CTD"] <- "Station"
names(Nuts_50_250_mean_sd)[names(Nuts_50_250_mean_sd) == "Nuts_50_250_mean$Season"] <- "Season"
names(Nuts_50_250_mean_sd)[names(Nuts_50_250_mean_sd) == "Nuts_50_250_mean$Variable"] <- "Nutrient"
names(Nuts_50_250_mean_sd)[names(Nuts_50_250_mean_sd) == "Nuts_50_250_mean$nutrient_mean"] <- "Mean"
names(Nuts_50_250_mean_sd)[names(Nuts_50_250_mean_sd) == "Nuts_50_250_sd$nutrient_sd"] <- "SD"
names(Nuts_50_250_mean_sd)[names(Nuts_50_250_mean_sd) == "Nuts_50_250_mean$DepthInterval"] <- "DepthInterval"

names(Nuts_250_400_mean_sd)[names(Nuts_250_400_mean_sd) == "Nuts_250_400_mean$Station_CTD"] <- "Station"
names(Nuts_250_400_mean_sd)[names(Nuts_250_400_mean_sd) == "Nuts_250_400_mean$Season"] <- "Season"
names(Nuts_250_400_mean_sd)[names(Nuts_250_400_mean_sd) == "Nuts_250_400_mean$Variable"] <- "Nutrient"
names(Nuts_250_400_mean_sd)[names(Nuts_250_400_mean_sd) == "Nuts_250_400_mean$nutrient_mean"] <- "Mean"
names(Nuts_250_400_mean_sd)[names(Nuts_250_400_mean_sd) == "Nuts_250_400_sd$nutrient_sd"] <- "SD"
names(Nuts_250_400_mean_sd)[names(Nuts_250_400_mean_sd) == "Nuts_250_400_mean$DepthInterval"] <- "DepthInterval"


AllDepthInts_HL6LL7 <- rbind.fill(Nuts_0_50_mean_sd, Nuts_50_250_mean_sd, Nuts_250_400_mean_sd)

#Remove the underscore and everything after in the Nutrient field
AllDepthInts_HL6LL7$Nutrient <-gsub("_.*", "", AllDepthInts_HL6LL7$Nutrient)
AllDepthInts_HL6LL7


###########GULLY DATA

#Now load in data from 4 Gully stations
Gully_NutsInt_0_50_scaled <- readRDS(file = paste0(datapath, "NutsInt_0_50_scaled_Gully.rds"))
Gully_NutsInt_50_250_scaled <- readRDS(file = paste0(datapath, "NutsInt_50_250_scaled_Gully.rds"))
Gully_NutsInt_250_400_scaled <- readRDS(file = paste0(datapath, "NutsInt_250_400_scaled_Gully.rds"))

colnames(Gully_NutsInt_0_50_scaled)[5] <- "Nitrate"
colnames(Gully_NutsInt_50_250_scaled)[5] <- "Nitrate"
colnames(Gully_NutsInt_250_400_scaled)[5] <- "Nitrate"

colnames(Gully_NutsInt_0_50_scaled)[6] <- "Phosphate"
colnames(Gully_NutsInt_50_250_scaled)[6] <- "Phosphate"
colnames(Gully_NutsInt_250_400_scaled)[6] <- "Phosphate"

colnames(Gully_NutsInt_0_50_scaled)[7] <- "Silicate"
colnames(Gully_NutsInt_50_250_scaled)[7] <- "Silicate"
colnames(Gully_NutsInt_250_400_scaled)[7] <- "Silicate"

colnames(Gully_NutsInt_0_50_scaled)[8] <- "DepthInterval"
colnames(Gully_NutsInt_50_250_scaled)[8] <- "DepthInterval"
colnames(Gully_NutsInt_250_400_scaled)[8] <- "DepthInterval"


#0 - 50 m
StationAVG_0_50Gully <- Gully_NutsInt_0_50_scaled %>%
  group_by(Station, Season) %>%
  summarise_at(vars(c("Nitrate", "Phosphate", "Silicate")), funs(mean,sd), na.rm = TRUE)
StationAVG_0_50Gully

#50 - 250 m
StationAVG_50_250Gully <- Gully_NutsInt_50_250_scaled %>%
  group_by(Station, Season) %>%
  summarise_at(vars(c("Nitrate", "Phosphate", "Silicate")), funs(mean,sd), na.rm = TRUE)
StationAVG_50_250Gully

#250 - 400 m
StationAVG_250_400Gully <- Gully_NutsInt_250_400_scaled %>%
  group_by(Station, Season) %>%
  summarise_at(vars(c("Nitrate", "Phosphate", "Silicate")), funs(mean,sd), na.rm = TRUE)
StationAVG_250_400Gully


#Use gather to create the mean of nutrient concentration across years within a station and season

Nuts_0_50_meanGully <- StationAVG_0_50Gully %>%
  gather(Variable, nutrient_mean, contains("mean")) %>%
  select(-c(Nitrate_sd, Phosphate_sd, Silicate_sd))
Nuts_0_50_meanGully
Nuts_0_50_meanGully$DepthInterval <- "0 - 50 m"

Nuts_50_250_meanGully <- StationAVG_50_250Gully %>%
  gather(Variable, nutrient_mean, contains("mean")) %>%
  select(-c(Nitrate_sd, Phosphate_sd, Silicate_sd))
Nuts_50_250_meanGully
Nuts_50_250_meanGully$DepthInterval <- "50 - 250 m"

Nuts_250_400_meanGully <- StationAVG_250_400Gully %>%
  gather(Variable, nutrient_mean, contains("mean")) %>%
  select(-c(Nitrate_sd, Phosphate_sd, Silicate_sd))
Nuts_250_400_meanGully
Nuts_250_400_meanGully$DepthInterval <- "250 - 400 m"

Nuts_0_50_sdGully <- StationAVG_0_50Gully %>%
  gather(Variable, nutrient_sd, contains("sd")) %>%
  select(-c(Nitrate_mean, Phosphate_mean, Silicate_mean))
Nuts_0_50_sdGully
Nuts_0_50_sdGully$DepthInterval <- "0 - 50 m"

Nuts_50_250_sdGully <- StationAVG_50_250Gully %>%
  gather(Variable, nutrient_sd, contains("sd")) %>%
  select(-c(Nitrate_mean, Phosphate_mean, Silicate_mean))
Nuts_50_250_sdGully
Nuts_50_250_sdGully$DepthInterval <- "50 - 250 m"

Nuts_250_400_sdGully <- StationAVG_250_400Gully %>%
  gather(Variable, nutrient_sd, contains("sd")) %>%
  select(-c(Nitrate_mean, Phosphate_mean, Silicate_mean))
Nuts_250_400_sdGully
Nuts_250_400_sdGully$DepthInterval <- "250 - 400 m"


Nuts_0_50_mean_sdGully <-cbind.data.frame(Nuts_0_50_meanGully$Station, Nuts_0_50_meanGully$Season,
                                     Nuts_0_50_meanGully$Variable, Nuts_0_50_meanGully$nutrient_mean,
                                     Nuts_0_50_sdGully$nutrient_sd, Nuts_0_50_meanGully$DepthInterval)

Nuts_50_250_mean_sdGully <-cbind.data.frame(Nuts_50_250_meanGully$Station, Nuts_50_250_meanGully$Season,
                                       Nuts_50_250_meanGully$Variable, Nuts_50_250_meanGully$nutrient_mean,
                                       Nuts_50_250_sdGully$nutrient_sd, Nuts_50_250_meanGully$DepthInterval)

Nuts_250_400_mean_sdGully <-cbind.data.frame(Nuts_250_400_meanGully$Station, Nuts_250_400_meanGully$Season,
                                        Nuts_250_400_meanGully$Variable, Nuts_250_400_meanGully$nutrient_mean,
                                        Nuts_250_400_sdGully$nutrient_sd, Nuts_250_400_meanGully$DepthInterval)

names(Nuts_0_50_mean_sdGully)[names(Nuts_0_50_mean_sdGully) == "Nuts_0_50_meanGully$Station"] <- "Station"
names(Nuts_0_50_mean_sdGully)[names(Nuts_0_50_mean_sdGully) == "Nuts_0_50_meanGully$Season"] <- "Season"
names(Nuts_0_50_mean_sdGully)[names(Nuts_0_50_mean_sdGully) == "Nuts_0_50_meanGully$Variable"] <- "Nutrient"
names(Nuts_0_50_mean_sdGully)[names(Nuts_0_50_mean_sdGully) == "Nuts_0_50_meanGully$nutrient_mean"] <- "Mean"
names(Nuts_0_50_mean_sdGully)[names(Nuts_0_50_mean_sdGully) == "Nuts_0_50_sdGully$nutrient_sd"] <- "SD"
names(Nuts_0_50_mean_sdGully)[names(Nuts_0_50_mean_sdGully) == "Nuts_0_50_meanGully$DepthInterval"] <- "DepthInterval"

names(Nuts_50_250_mean_sdGully)[names(Nuts_50_250_mean_sdGully) == "Nuts_50_250_meanGully$Station"] <- "Station"
names(Nuts_50_250_mean_sdGully)[names(Nuts_50_250_mean_sdGully) == "Nuts_50_250_meanGully$Season"] <- "Season"
names(Nuts_50_250_mean_sdGully)[names(Nuts_50_250_mean_sdGully) == "Nuts_50_250_meanGully$Variable"] <- "Nutrient"
names(Nuts_50_250_mean_sdGully)[names(Nuts_50_250_mean_sdGully) == "Nuts_50_250_meanGully$nutrient_mean"] <- "Mean"
names(Nuts_50_250_mean_sdGully)[names(Nuts_50_250_mean_sdGully) == "Nuts_50_250_sdGully$nutrient_sd"] <- "SD"
names(Nuts_50_250_mean_sdGully)[names(Nuts_50_250_mean_sdGully) == "Nuts_50_250_meanGully$DepthInterval"] <- "DepthInterval"

names(Nuts_250_400_mean_sdGully)[names(Nuts_250_400_mean_sdGully) == "Nuts_250_400_meanGully$Station"] <- "Station"
names(Nuts_250_400_mean_sdGully)[names(Nuts_250_400_mean_sdGully) == "Nuts_250_400_meanGully$Season"] <- "Season"
names(Nuts_250_400_mean_sdGully)[names(Nuts_250_400_mean_sdGully) == "Nuts_250_400_meanGully$Variable"] <- "Nutrient"
names(Nuts_250_400_mean_sdGully)[names(Nuts_250_400_mean_sdGully) == "Nuts_250_400_meanGully$nutrient_mean"] <- "Mean"
names(Nuts_250_400_mean_sdGully)[names(Nuts_250_400_mean_sdGully) == "Nuts_250_400_sdGully$nutrient_sd"] <- "SD"
names(Nuts_250_400_mean_sdGully)[names(Nuts_250_400_mean_sdGully) == "Nuts_250_400_meanGully$DepthInterval"] <- "DepthInterval"


AllDepthInts_Gully <- rbind.fill(Nuts_0_50_mean_sdGully, Nuts_50_250_mean_sdGully, Nuts_250_400_mean_sdGully)

#Remove the underscore and everything after in the Nutrient field
AllDepthInts_Gully$Nutrient <-gsub("_.*", "", AllDepthInts_Gully$Nutrient)
AllDepthInts_Gully


#Collate HL6, LL7, and Gully data together

AllData <- rbind.fill(AllDepthInts_HL6LL7, AllDepthInts_Gully)


#Produce the final Figure 22 for the report

AllData$Season<-ordered(AllData$Season, levels = c("Spring", "Fall"))

AllData$DepthInterval<-ordered(AllData$DepthInterval, levels = c("0 - 50 m", "50 - 250 m", "250 - 400 m"))

AllData$Station<-ordered(AllData$Station, levels = c("HL_06", "GULD_03", "SG_28", "GULD_04", "SG_23", "LL_07"))

pos <- position_dodge(width = 0.7)

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure22.png"), units="in", width=12, height=10, res=400)

fig22 <- ggplot(AllData, aes(x=Station, y = Mean, group=Season)) +
  geom_point(aes(x=Station, y = Mean, color=Season), size=3, position= pos) +  #Be careful. It orders the variables by color name
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.5, col="gray50", position = pos) +
  scale_color_manual(values = c("dodgerblue2", "orange")) +
  ylab(expression(paste("\nNutrient concentration  ", (mmol~m^{-3})))) +
  xlab("Station\n") +
  theme_bw() +
  theme(axis.text.y=element_text(size=15)) +
  theme(axis.text.x=element_text(size=15, angle=70, vjust = 0.5)) +
  theme(axis.title.y=element_text(size=16)) +
  theme(axis.title.x=element_text(size=16)) +
  theme(legend.position = "top")+
  theme(legend.text=element_text(size=15)) +
  theme(legend.title =element_text(size=16)) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

fig22 <- fig22 + facet_grid(DepthInterval ~ Nutrient) +
  theme(strip.text.x = element_text(size=14, face="bold", vjust = 1)) +
  theme(strip.text.y = element_text(size=14, face="bold", vjust = 1)) +
  guides(fill=FALSE)

print(fig22)

dev.off()



