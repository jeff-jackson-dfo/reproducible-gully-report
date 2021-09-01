###################################################################
###  FIGURE 23: Mean Vertical Structure in Seasonal Chlorophyll ###
###################################################################

# Last updated: 31-AUG-2021
# Author: Lindsay Beazley
# Modified by: Jeff Jackson

#Load packages

library(oce)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
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

springchem <- rbind.fill(chemSpringGuld03_v3, chemSpringGuld04_v2, chemSpringSG23_v2, chemSpringSG28_v2)

springchem


#create a proper event ID field:

springchem$Event <- (str_sub(springchem$event_id, start=-3))

#Rename year to Year
names(springchem)[3] <- "Year"


#Collate data

fallchem <- rbind.fill(chemFallGuld03_v2, chemFallGuld04_v2, chemFallSG23_v2, chemFallSG28)

fallchem


#create a proper event ID field:

fallchem$Event <- (str_sub(fallchem$event_id, start=-3))

#Rename year to Year
names(fallchem)[3] <- "Year"


##Select Depth intervals - SPRING


#Near-surface
springchl_surface <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>0 & depth<=5.3) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_surface

springchl_surface$DepthBin <- "Near-surface"


#10 m
springchl_10m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>5.3 & depth<=15) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_10m

springchl_10m$DepthBin <- "10 m"


#20 m
springchl_20m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>15 & depth<=25) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_20m

springchl_20m$DepthBin <- "20 m"


#30 m
springchl_30m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>25 & depth<=35) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_30m

springchl_30m$DepthBin <- "30 m"


#40 m
springchl_40m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>35 & depth<=45) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_40m

springchl_40m$DepthBin <- "40 m"


#50 m
springchl_50m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>45 & depth<=55) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_50m

springchl_50m$DepthBin <- "50 m"


#60 m
springchl_60m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>55 & depth<=65) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_60m

springchl_60m$DepthBin <- "60 m"


#80 m
springchl_80m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>75 & depth<=85) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_80m

springchl_80m$DepthBin <- "80 m"


#100 m
springchl_100m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>94 & depth<=107) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
springchl_100m

springchl_100m$DepthBin <- "100 m"



##Select Depth intervals - FALL

#Near-surface
fallchl_surface <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>0 & depth<=5.3) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_surface

fallchl_surface$DepthBin <- "Near-surface"


#10 m
fallchl_10m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>5.3 & depth<=15) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_10m

fallchl_10m$DepthBin <- "10 m"


#20 m
fallchl_20m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>15 & depth<=25) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_20m

fallchl_20m$DepthBin <- "20 m"


#30 m
fallchl_30m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>25 & depth<=35) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_30m

fallchl_30m$DepthBin <- "30 m"


#40 m
fallchl_40m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>35 & depth<=45) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_40m

fallchl_40m$DepthBin <- "40 m"


#50 m
fallchl_50m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>45 & depth<=55) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_50m

fallchl_50m$DepthBin <- "50 m"


#60 m
fallchl_60m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>55 & depth<=65) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_60m

fallchl_60m$DepthBin <- "60 m"


#80 m
fallchl_80m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>75 & depth<=85) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_80m

fallchl_80m$DepthBin <- "80 m"


#100 m
fallchl_100m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>95 & depth<=105) %>%
  summarise_at(vars(c("chlorophyll_a")), list(mean = mean, sd = sd), na.rm = TRUE)
fallchl_100m

fallchl_100m$DepthBin <- "100 m"


Combined_spring <- rbind.fill(springchl_surface, springchl_10m, springchl_20m, springchl_30m, springchl_40m,
                              springchl_50m, springchl_60m, springchl_80m, springchl_100m)

Combined_fall <- rbind.fill(fallchl_surface, fallchl_10m, fallchl_20m, fallchl_30m, fallchl_40m,
                              fallchl_50m, fallchl_60m, fallchl_80m, fallchl_100m)


#Make x axis numeric variable:

Combined_spring$Depth <- Combined_spring$DepthBin

#Rename levels of character Depth field to make it numeric for plotting
Combined_spring$Depth <- gsub("Near-surface", "1", Combined_spring$Depth)
Combined_spring$Depth <- gsub("10 m", "10", Combined_spring$Depth)
Combined_spring$Depth <- gsub("20 m", "20", Combined_spring$Depth)
Combined_spring$Depth <- gsub("30 m", "30", Combined_spring$Depth)
Combined_spring$Depth <- gsub("40 m", "40", Combined_spring$Depth)
Combined_spring$Depth <- gsub("50 m", "50", Combined_spring$Depth)
Combined_spring$Depth <- gsub("60 m", "60", Combined_spring$Depth)
Combined_spring$Depth <- gsub("80 m", "80", Combined_spring$Depth)
Combined_spring$Depth <- gsub("100 m", "100", Combined_spring$Depth)


#Make x axis numeric variable:

Combined_fall$Depth <- Combined_fall$DepthBin

#Rename levels of character Depth field to make it numeric for plotting
Combined_fall$Depth <- gsub("Near-surface", "1", Combined_fall$Depth)
Combined_fall$Depth <- gsub("10 m", "10", Combined_fall$Depth)
Combined_fall$Depth <- gsub("20 m", "20", Combined_fall$Depth)
Combined_fall$Depth <- gsub("30 m", "30", Combined_fall$Depth)
Combined_fall$Depth <- gsub("40 m", "40", Combined_fall$Depth)
Combined_fall$Depth <- gsub("50 m", "50", Combined_fall$Depth)
Combined_fall$Depth <- gsub("60 m", "60", Combined_fall$Depth)
Combined_fall$Depth <- gsub("80 m", "80", Combined_fall$Depth)
Combined_fall$Depth <- gsub("100 m", "100", Combined_fall$Depth)



#Make Depth numeric so it can be plotted as a continuous variable
Combined_fall$Depth<- as.numeric(Combined_fall$Depth)



Combined_spring$Season <- "Spring"
Combined_fall$Season <- "Fall"

springfallchl <- rbind.fill(Combined_spring, Combined_fall)

springfallchl$Season<-ordered(springfallchl$Season, levels = c("Spring", "Fall"))

#Make year numeric so it can be plotted as a continuous variable
springfallchl$Depth<- as.numeric(springfallchl$Depth)


#Rearrange order of variables

springfallchl$Station <- ordered(springfallchl$Station, levels = c("GULD_03",
                                                                 "SG_28",
                                                                 "GULD_04",
                                                                 "SG_23"))


figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure23.png"), units="in", width=12, height=7, res=400)

fig23 <- ggplot(springfallchl, aes(x=Depth)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.5, col="gray50") +
  geom_point(aes(y=mean), col="dodgerblue2", size=3) +
  geom_line(aes(y=mean), col="dodgerblue2", lwd=1.5) +
  coord_flip() +
  scale_x_reverse(breaks = seq(0, 100, by = 20)) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab(expression(paste("\nChlorophyll ", italic("a"), " concentration ", (mu*g~L^{-1})))) +
  xlab("Depth (m)\n") +
  theme(panel.spacing = unit(.5, "lines")) +
  theme(axis.text.y=element_text(size=14)) +
  theme(axis.text.x=element_text(size=14)) +
  theme(axis.title.y=element_text(size=15)) +
  theme(axis.title.x=element_text(size=15)) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

fig23 <- fig23 + facet_grid(Season ~ Station) +
  theme(strip.text.x = element_text(size=18, face="bold", vjust=2.2)) +
  theme(strip.text.y = element_text(size=18, face="bold", vjust=1)) +
  theme(strip.background.x = element_rect(color="white", fill="white", linetype="solid")) +
  guides(fill=FALSE)

print(fig23)

dev.off()

#Averages

Combined_spring
Combined_fall

#Spring average
StationMeanSpring <- Combined_spring %>%
  group_by(Station) %>%  #Group
  summarise_at(vars(c("mean")), funs(mean), na.rm = TRUE)
StationMeanSpring

#Fall average
StationMeanFall <- Combined_fall %>%
  group_by(Station) %>%  #Group
  summarise_at(vars(c("mean")), funs(mean), na.rm = TRUE)
StationMeanFall
