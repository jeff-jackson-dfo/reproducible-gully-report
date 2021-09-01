##################################################################
###  FIGURE 17: Mean Vertical Structure in Seasonal Nutrients  ###
##################################################################

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
library(gtable)
library(tidyr)
library(ggpubr)


#Load .RData containing the nutrient data extracted by Jeff Jackson
projpath <- getwd()
datapath <- paste0(projpath, "/data/2_ChemicalData/")
load(paste0(datapath, "monitoringSitesDIS-complete.RData"))


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

chemSpringGuld03_v2 <- subset(chemSpringGuld03, event_id != "18PZ00002_2000002202" &
                                                        event_id != "18HU06008_2006008027" &
                                                        event_id != "18HU08004_2008004070" &
                                                        event_id != "18HU06008_2006008028" &
                                                        event_id != "18HU04009_2004009091" &
                                                        event_id != "18HU06008_2006008029" &
                                                        event_id != "18HU10006_2010006177")

#Evaluation of the data revealed that 18HU04009_2004009094 consists of only 1 entry, at the surface. This should be removed.
chemSpringGuld03_v3 <- subset(chemSpringGuld03_v2, event_id != "18HU04009_2004009094")

#From Spring GULD_04, remove the following casts:
chemSpringGuld04_v2 <- subset(chemSpringGuld04, event_id != "18OL17001_087") #surface cast

#From Spring SG_23, remove the following casts:
chemSpringSG23_v2 <- subset(chemSpringSG23, event_id != "18OL17001_090" & #surface cast
                                            event_id != "18HU10006_2010006183") #not present in CTD data
#From Spring SG_28, remove the following casts:
chemSpringSG28_v2 <- subset(chemSpringSG28, event_id != "18OL17001_080" & #surface cast
                                            event_id != "18HU10006_2010006187") #Not present in CTD data


#From Fall GULD_03, remove the following casts:
chemFallGuld03_v2 <- subset(chemFallGuld03, event_id != "18HU00050_2000050289" &
                                            event_id != "18HU01061_2001061179")

#From Fall GULD_04, remove the following casts:
chemFallGuld04_v2 <- subset(chemFallGuld04, event_id != "18HU00050_2000050289")

#From Fall SG_23, remove the following casts:
chemFallSG23_v2 <- subset(chemFallSG23, event_id != "18HU07045_2007045130")

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


##Select Depth intervals - SPRING

# Intervals are approximately +/- 5 m of nominal bottle depth

#Near-surface
springnuts_surface <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>0 & depth<=5.3) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_surface

springnuts_surface$DepthBin <- "Near-surface"


#10 m
springnuts_10m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>5.31 & depth<=15) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_10m

springnuts_10m$DepthBin <- "10 m"


#20 m
springnuts_20m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>15 & depth<=25) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_20m

springnuts_20m$DepthBin <- "20 m"


#30 m
springnuts_30m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>25 & depth<=35) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_30m

springnuts_30m$DepthBin <- "30 m"


#40 m
springnuts_40m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>35 & depth<=45) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_40m

springnuts_40m$DepthBin <- "40 m"


#50 m
springnuts_50m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>45 & depth<=55) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_50m

springnuts_50m$DepthBin <- "50 m"


#60 m
springnuts_60m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>55 & depth<=65) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_60m

springnuts_60m$DepthBin <- "60 m"


#80 m
springnuts_80m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>75 & depth<=85) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_80m

springnuts_80m$DepthBin <- "80 m"


#100 m
springnuts_100m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>94 & depth<=107) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_100m

springnuts_100m$DepthBin <- "100 m"


#250 m
springnuts_250m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>245 & depth<=255) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_250m

springnuts_250m$DepthBin <- "250 m"


#500 m
springnuts_500m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>495 & depth<=505) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_500m

springnuts_500m$DepthBin <- "500 m"

#Exclude 500 m record for GULD_03 as they are already included in the GULD3 bottom calculation:

springnuts_500m_v2 <- subset(springnuts_500m, springnuts_500m$Station != "GULD_03")


#750 m
springnuts_750m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>743 & depth<=755) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_750m

springnuts_750m$DepthBin <- "750 m"


#1500 m
springnuts_1500m <- springchem %>%
  group_by(Station) %>%  #Group
  filter(depth>1495 & depth<=1505) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_1500m

springnuts_1500m$DepthBin <- "1500 m"


#Near-bottom GULD_03
springnutsGULD3 <- subset(springchem, Station == "GULD_03")
springnuts_bottomGULD_03 <- springnutsGULD3 %>%
  group_by(Station) %>%  #Group
  filter(depth>400) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_bottomGULD_03

Depthsspringnuts_bottomGULD_03 <- springnutsGULD3 %>%
  group_by(Station) %>%  #Group
  filter(depth>400)

mean(Depthsspringnuts_bottomGULD_03$depth)  #456.363 m
sd(Depthsspringnuts_bottomGULD_03$depth)    #40.43545 m

springnuts_bottomGULD_03$DepthBin <- "456"


#Near-bottom GULD_04
springnutsGULD4 <- subset(springchem, Station == "GULD_04")
springnuts_bottomGULD_04 <- springnutsGULD4 %>%
  group_by(Station) %>%  #Group
  filter(depth>1900) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_bottomGULD_04

Depthsspringnuts_bottomGULD_04 <- springnutsGULD4 %>%
  group_by(Station) %>%  #Group
  filter(depth>1900)

mean(Depthsspringnuts_bottomGULD_04$depth)  #2155.508 m
sd(Depthsspringnuts_bottomGULD_04$depth)    #79.51028 m

springnuts_bottomGULD_04$DepthBin <- "2156"


#Near-bottom SG_23
springnutsSG23 <- subset(springchem, Station == "SG_23")
springnuts_bottomSG23 <- springnutsSG23 %>%
  group_by(Station) %>%  #Group
  filter(depth>940) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_bottomSG23

Depthsspringnuts_bottomSG23 <- springnutsSG23 %>%
  group_by(Station) %>%  #Group
  filter(depth>940)

mean(Depthsspringnuts_bottomSG23$depth)  #1130.701 m
sd(Depthsspringnuts_bottomSG23$depth)    #126.7847 m

springnuts_bottomSG23$DepthBin <- "1131"


#Near-bottom SG_28
springnutsSG28 <- subset(springchem, Station == "SG_28")
springnuts_bottomSG28 <- springnutsSG28 %>%
  group_by(Station) %>%  #Group
  filter(depth>793) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# springnuts_bottomSG28

Depthsspringnuts_bottomSG28 <- springnutsSG28 %>%
  group_by(Station) %>%  #Group
  filter(depth>793)

mean(Depthsspringnuts_bottomSG28$depth)  #886.3471 m
sd(Depthsspringnuts_bottomSG28$depth)    #71.16243 m

springnuts_bottomSG28$DepthBin <- "886"


##Select Depth intervals - FALL


#Near-surface
fallnuts_surface <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>0 & depth<=5.3) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_surface

fallnuts_surface$DepthBin <- "Near-surface"


#10 m
fallnuts_10m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>5.3 & depth<=15) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_10m

fallnuts_10m$DepthBin <- "10 m"


#20 m
fallnuts_20m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>15 & depth<=25) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_20m

fallnuts_20m$DepthBin <- "20 m"


#30 m
fallnuts_30m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>25 & depth<=35) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_30m

fallnuts_30m$DepthBin <- "30 m"


#40 m
fallnuts_40m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>35 & depth<=45) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_40m

fallnuts_40m$DepthBin <- "40 m"


#50 m
fallnuts_50m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>45 & depth<=55) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_50m

fallnuts_50m$DepthBin <- "50 m"


#60 m
fallnuts_60m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>55 & depth<=65) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_60m

fallnuts_60m$DepthBin <- "60 m"


#80 m
fallnuts_80m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>75 & depth<=85) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_80m

fallnuts_80m$DepthBin <- "80 m"


#100 m
fallnuts_100m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>95 & depth<=105) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_100m

fallnuts_100m$DepthBin <- "100 m"


#250 m
fallnuts_250m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>245 & depth<=255) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_250m

fallnuts_250m$DepthBin <- "250 m"


#500 m
fallnuts_500m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>495 & depth<=505) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_500m

fallnuts_500m$DepthBin <- "500 m"


#750 m
fallnuts_750m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>745 & depth<=758) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_750m

fallnuts_750m$DepthBin <- "750 m"


#1500 m
fallnuts_1500m <- fallchem %>%
  group_by(Station) %>%  #Group
  filter(depth>1495 & depth<=1505) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_1500m

fallnuts_1500m$DepthBin <- "1500 m"


#Near-bottom GULD_03
fallnutsGULD3 <- subset(fallchem, Station == "GULD_03")
fallnuts_bottomGULD_03 <- fallnutsGULD3 %>%
  group_by(Station) %>%  #Group
  filter(depth>400) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_bottomGULD_03

Depthsfallnuts_bottomGULD_03 <- fallnutsGULD3 %>%
  group_by(Station) %>%  #Group
  filter(depth>400)

mean(Depthsfallnuts_bottomGULD_03$depth)  #478.0793 m
sd(Depthsfallnuts_bottomGULD_03$depth)    #50.17351 m


fallnuts_bottomGULD_03$DepthBin <- "478"


#Near-bottom GULD_04
fallnutsGULD4 <- subset(fallchem, Station == "GULD_04")
fallnuts_bottomGULD_04 <- fallnutsGULD4 %>%
  group_by(Station) %>%  #Group
  filter(depth>1900) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_bottomGULD_04

Depthsfallnuts_bottomGULD_04 <- fallnutsGULD4 %>%
  group_by(Station) %>%  #Group
  filter(depth>1900)

mean(Depthsfallnuts_bottomGULD_04$depth)  #2123.503 m
sd(Depthsfallnuts_bottomGULD_04$depth)    #80.31819 m


fallnuts_bottomGULD_04$DepthBin <- "2124"


#Near-bottom SG_23
fallnutsSG23 <- subset(fallchem, Station == "SG_23")
fallnuts_bottomSG23 <- fallnutsSG23 %>%
  group_by(Station) %>%  #Group
  filter(depth>940) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_bottomSG23

Depthsfallnuts_bottomSG23 <- fallnutsSG23 %>%
  group_by(Station) %>%  #Group
  filter(depth>940)

mean(Depthsfallnuts_bottomSG23$depth)  #1214.921 m
sd(Depthsfallnuts_bottomSG23$depth)    #144.8948 m

fallnuts_bottomSG23$DepthBin <- "1215"


#Near-bottom SG_28
fallnutsSG28 <- subset(fallchem, Station == "SG_28")
fallnuts_bottomSG28 <- fallnutsSG28 %>%
  group_by(Station) %>%  #Group
  filter(depth>793) %>%
  summarise_at(vars(c("nitrate", "silicate", "phosphate")), funs(mean, sd), na.rm = TRUE)
# fallnuts_bottomSG28

Depthsfallnuts_bottomSG28 <- fallnutsSG28 %>%
  group_by(Station) %>%  #Group
  filter(depth>793)

mean(Depthsfallnuts_bottomSG28$depth)  #860.0929 m
sd(Depthsfallnuts_bottomSG28$depth)    #41.50806 m

fallnuts_bottomSG28$DepthBin <- "860"



Combined_spring <- rbind.fill(springnuts_surface, springnuts_10m, springnuts_20m, springnuts_30m, springnuts_40m,
                              springnuts_50m, springnuts_60m, springnuts_80m, springnuts_100m, springnuts_250m,
                              springnuts_500m_v2, springnuts_750m, springnuts_1500m, springnuts_bottomGULD_03,
                              springnuts_bottomGULD_04, springnuts_bottomSG23, springnuts_bottomSG28)

Combined_fall <- rbind.fill(fallnuts_surface, fallnuts_10m, fallnuts_20m, fallnuts_30m, fallnuts_40m,
                              fallnuts_50m, fallnuts_60m, fallnuts_80m, fallnuts_100m, fallnuts_250m,
                              fallnuts_500m, fallnuts_750m, fallnuts_1500m, fallnuts_bottomGULD_03,
                              fallnuts_bottomGULD_04, fallnuts_bottomSG23, fallnuts_bottomSG28)


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
Combined_spring$Depth <- gsub("250 m", "250", Combined_spring$Depth)
Combined_spring$Depth <- gsub("500 m", "500", Combined_spring$Depth)
Combined_spring$Depth <- gsub("1500 m", "1500", Combined_spring$Depth)


#Make Depth numeric so it can be plotted as a continuous variable
Combined_spring$Depth<- as.numeric(Combined_spring$Depth)

#Spring nutrients, not included in final publication, but was a test plot to show a single season
# sprnuts <- ggplot(Combined_spring, aes(x=Depth)) +
#   geom_point(aes(y=nitrate_mean), col="red") +
#   geom_line(aes(y=nitrate_mean), col="red", lwd=1) +
#   geom_point(aes(y=silicate_mean), col="darkgreen") +
#   geom_line(aes(y=silicate_mean), col="darkgreen", lwd=1) +
#   geom_point(aes(y=phosphate_mean*12), col="blue") +
#   geom_line(aes(y=phosphate_mean*12), col="blue", lwd=1) +
#   scale_y_continuous(sec.axis = sec_axis(~./12, name = "Phosphate\n")) +
#   coord_flip() +
#   scale_x_reverse() +
#   theme_bw() +
#   ylab("\nNitrate & silicate concentration") +
#   xlab("Depth (m)\n") +
#   theme(axis.text.y=element_text(size=13)) +
#   theme(axis.text.x=element_text(size=13)) +
#   theme(axis.title.y=element_text(size=14)) +
#   theme(axis.title.x=element_text(size=14))
#
# sprnuts <- sprnuts + facet_grid(rows = vars(Station), scales="free") +
#   theme(strip.text.y = element_text(size=14, face="bold", vjust = 1))+
#   guides(fill=FALSE)


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
Combined_fall$Depth <- gsub("250 m", "250", Combined_fall$Depth)
Combined_fall$Depth <- gsub("500 m", "500", Combined_fall$Depth)
Combined_fall$Depth <- gsub("1500 m", "1500", Combined_fall$Depth)


#Make Depth numeric so it can be plotted as a continuous variable
Combined_fall$Depth<- as.numeric(Combined_fall$Depth)

Combined_spring$Season <- "Spring"
Combined_fall$Season <- "Fall"

springfallnuts <- rbind.fill(Combined_spring, Combined_fall)

springfallnuts$Season<-ordered(springfallnuts$Season, levels = c("Spring", "Fall"))

#Make year numeric so it can be plotted as a continuous variable
springfallnuts$Depth<- as.numeric(springfallnuts$Depth)


#Rearrange order of variables

springfallnuts$Station <- ordered(springfallnuts$Station, levels = c("GULD_03",
                                                                     "SG_28",
                                                                     "GULD_04",
                                                                     "SG_23"))
sprfallnuts <- ggplot(springfallnuts, aes(x=Depth)) +
  geom_point(aes(y=nitrate_mean, color = "dodgerblue2"), size=0) +
  geom_line(aes(y=nitrate_mean, color = "dodgerblue2"), lwd=1.5) +
  geom_point(aes(y=phosphate_mean*12, color = "gray65"), size=0) +
  geom_line(aes(y=phosphate_mean*12, color = "gray65"), lwd=1.5) +
  geom_point(aes(y=silicate_mean, color = "orange"), size=0) +
  geom_line(aes(y=silicate_mean, color = "orange"), lwd=1.5) +
  scale_color_identity(guide = "legend", name=NULL, labels = c("Nitrate", "Phosphate", "Silicate")) +
  guides(colour = guide_legend(label.theme = element_text(size=15))) +
  scale_y_continuous(sec.axis = sec_axis(~./12, name=expression(paste("Phosphate concentration ", (mu*M))))) +
  coord_flip() +
  scale_x_reverse() +
  theme_bw() +
  ylab(expression(paste("Nitrate & silicate concentration ", (mu*M)))) +
  xlab("Depth (m)\n") +
  theme(axis.text.y=element_text(size=15)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.title.y=element_text(size=16)) +
  theme(axis.title.x=element_text(size=16)) +
  theme(legend.position = c(0.08, 0.1)) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

sprfallnuts <- sprfallnuts + facet_grid(Season ~ Station) +
  theme(strip.text.x = element_text(size=18, face="bold", vjust=1)) +
  theme(strip.text.y = element_text(size=18, face="bold", vjust=1)) +
  theme(strip.background.x = element_rect(color="white", fill="white", linetype="solid")) +
  theme(strip.placement.x = "outside")+
  guides(fill = "none")

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure17.png"), units = "in", width = 12, height = 8, res = 400)

print(sprfallnuts)

#You will see that the Phosphate axis label is above the facet strip.
#Found a solution to this here: https://stackoverflow.com/questions/60091106/ggplot-placing-facet-strips-above-axis-title
#had to be modified slightly to adjust the top and bottom grob margins

gp <- ggplotGrob(sprfallnuts)

gp$layout #helps you to understand the gtable object
gtable_show_layout(ggplotGrob(sprfallnuts)) #helps you to understand the gtable object

t_title <- gp$layout[gp$layout[['name']] == 'xlab-t' ,][['t']]

t_strip1 <- gp$layout[grepl('strip-t-1', gp$layout[['name']]),][['t']]
t_strip2 <- gp$layout[grepl('strip-t-2', gp$layout[['name']]),][['t']]
t_strip3 <- gp$layout[grepl('strip-t-3', gp$layout[['name']]),][['t']]
t_strip4 <- gp$layout[grepl('strip-t-4', gp$layout[['name']]),][['t']]

gp$layout[gp$layout[['name']] == 'xlab-t' ,][['t']] <- 8

gp$layout[grepl('strip-t-1', gp$layout[['name']]),][['t']] <- 1
gp$layout[grepl('strip-t-2', gp$layout[['name']]),][['t']] <- 1
gp$layout[grepl('strip-t-3', gp$layout[['name']]),][['t']] <- 1
gp$layout[grepl('strip-t-4', gp$layout[['name']]),][['t']] <- 1

gp$layout[grepl('strip-t-1', gp$layout[['name']]),][['b']] <- 5
gp$layout[grepl('strip-t-2', gp$layout[['name']]),][['b']] <- 5
gp$layout[grepl('strip-t-3', gp$layout[['name']]),][['b']] <- 5
gp$layout[grepl('strip-t-4', gp$layout[['name']]),][['b']] <- 5

grid.newpage()

grid.draw(gp)

dev.off()
