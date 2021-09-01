#######################################################################################
##### FIGURE 8: Seasonal Mean Vertical Structure in Temperature, Salinity, Oxygen #####
#######################################################################################

# Last updated: 31-AUG-2021
# Author: Lindsay Beazley
# Modified by: Jeff Jackson


#Load packages

library(plyr)
library(ggplot2)


# Manipulate Spring Data --------------------------------------------------

#Read in .rds files that show the summary statistics for each station/season

projpath <- getwd()
datapath <- paste0(projpath, "/data/1_PhysicalData/")

GULD3_Spring_Temp <- readRDS(paste0(datapath, "guld03-temperature-spring-stats.rds"))
GULD3_Spring_Sal <- readRDS(paste0(datapath, "guld03-salinity-spring-stats.rds"))
GULD3_Spring_Oxy <- readRDS(paste0(datapath, "guld03-oxygen-spring-stats.rds"))

GULD4_Spring_Temp <- readRDS(paste0(datapath, "guld04-temperature-spring-stats.rds"))
GULD4_Spring_Sal <- readRDS(paste0(datapath, "guld04-salinity-spring-stats.rds"))
GULD4_Spring_Oxy <- readRDS(paste0(datapath, "guld04-oxygen-spring-stats.rds"))

SG23_Spring_Temp <- readRDS(paste0(datapath, "sg23-temperature-spring-stats.rds"))
SG23_Spring_Sal <- readRDS(paste0(datapath, "sg23-salinity-spring-stats.rds"))
SG23_Spring_Oxy <- readRDS(paste0(datapath, "sg23-oxygen-spring-stats.rds"))

SG28_Spring_Temp <- readRDS(paste0(datapath, "sg28-temperature-spring-stats.rds"))
SG28_Spring_Sal <- readRDS(paste0(datapath, "sg28-salinity-spring-stats.rds"))
SG28_Spring_Oxy <- readRDS(paste0(datapath, "sg28-oxygen-spring-stats.rds"))


#Add station name as another column to each tibble:

GULD3_Spring_Temp$station <- "GULD_03"
GULD3_Spring_Sal$station <- "GULD_03"
GULD3_Spring_Oxy$station <- "GULD_03"

GULD4_Spring_Temp$station <- "GULD_04"
GULD4_Spring_Sal$station <- "GULD_04"
GULD4_Spring_Oxy$station <- "GULD_04"

SG23_Spring_Temp$station <- "SG_23"
SG23_Spring_Sal$station <- "SG_23"
SG23_Spring_Oxy$station <- "SG_23"

SG28_Spring_Temp$station <- "SG_28"
SG28_Spring_Sal$station <- "SG_28"
SG28_Spring_Oxy$station <- "SG_28"


#Add variable name as another column to each tibble:

GULD3_Spring_Temp$variable <- "Temperature"
GULD3_Spring_Sal$variable <- "Salinity"
GULD3_Spring_Oxy$variable <- "Oxygen"

GULD4_Spring_Temp$variable <- "Temperature"
GULD4_Spring_Sal$variable <- "Salinity"
GULD4_Spring_Oxy$variable <- "Oxygen"

SG23_Spring_Temp$variable <- "Temperature"
SG23_Spring_Sal$variable <- "Salinity"
SG23_Spring_Oxy$variable <- "Oxygen"

SG28_Spring_Temp$variable <- "Temperature"
SG28_Spring_Sal$variable <- "Salinity"
SG28_Spring_Oxy$variable <- "Oxygen"




Temp_AllStations_spring <- rbind.fill(GULD3_Spring_Temp[c("pressure", "smean", "station", "variable")],
                               SG28_Spring_Temp[c("pressure", "smean", "station", "variable")],
                               GULD4_Spring_Temp[c("pressure", "smean", "station", "variable")],
                               SG23_Spring_Temp[c("pressure", "smean", "station", "variable")])


Sal_AllStations_spring <- rbind.fill(GULD3_Spring_Sal[c("pressure", "smean", "station", "variable")],
                              SG28_Spring_Sal[c("pressure", "smean", "station", "variable")],
                              GULD4_Spring_Sal[c("pressure", "smean", "station", "variable")],
                              SG23_Spring_Sal[c("pressure", "smean", "station", "variable")])

Oxy_AllStations_spring <- rbind.fill(GULD3_Spring_Oxy[c("pressure", "smean", "station", "variable")],
                              SG28_Spring_Oxy[c("pressure", "smean", "station", "variable")],
                              GULD4_Spring_Oxy[c("pressure", "smean", "station", "variable")],
                              SG23_Spring_Oxy[c("pressure", "smean", "station", "variable")])


#Combine the temperature, salinity, and oxygen spring data

Combined_spring <- rbind.fill(Temp_AllStations_spring, Sal_AllStations_spring, Oxy_AllStations_spring)

Combined_spring$Season <- "Spring"


#Save as an RDS if you wish
#saveRDS(Combined_spring, file = "Combined_spring_GullyProfiles_ForComparison.RDS")


#Reorder station and parameter levels

Combined_spring$variable<-ordered(Combined_spring$variable, levels = c("Temperature",
                                                          "Salinity",
                                                          "Oxygen"))

Combined_spring$station<-ordered(Combined_spring$station, levels = c("GULD_03",
                                                       "SG_28",
                                                       "GULD_04",
                                                       "SG_23"))


#Preliminary plot for spring data only (not included in publication)

# tiff("SpringTempSalOxy_AllStations.tiff", units="in", width=10, height=5, res=400)
#
# ggplot_spring<- ggplot(Combined_spring, aes(x = pressure, y = smean, colour=station, group=station)) +
#            geom_line(size=1) +
#            coord_flip() +
#            scale_x_reverse() +
#            scale_colour_manual(labels = c("GULD_03", "SG_28", "GULD_04", "SG_23"),
#                                values = c("blue", "green", "black", "red")) +
#            xlab("\nDepth (m)") +
#            labs(color='Station')+
#            theme_bw()+
#            theme(axis.text.y=element_text(size=13))+
#            theme(axis.text.x=element_text(size=13)) +
#            theme(axis.title.y=element_text(size=14)) +
#            theme(axis.title.y=element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
#            theme(legend.text=element_text(size=13))+
#            theme(legend.title=element_text(size=14))
#
# ggplot_spring + facet_grid(.~variable, scales="free") +
#   theme(strip.text.x = element_text(size=14, face="bold"))+
#   guides(fill=FALSE)
#
# dev.off()


# Manipulate Fall Data ----------------------------------------------------


#Read in .rds files that show the summary statistics for each station/season

GULD3_Fall_Temp <- readRDS(paste0(datapath, "guld03-temperature-fall-stats.rds"))
GULD3_Fall_Sal <- readRDS(paste0(datapath, "guld03-salinity-fall-stats.rds"))
GULD3_Fall_Oxy <- readRDS(paste0(datapath, "guld03-oxygen-fall-stats.rds"))

GULD4_Fall_Temp <- readRDS(paste0(datapath, "guld04-temperature-fall-stats.rds"))
GULD4_Fall_Sal <- readRDS(paste0(datapath, "guld04-salinity-fall-stats.rds"))
GULD4_Fall_Oxy <- readRDS(paste0(datapath, "guld04-oxygen-fall-stats.rds"))

SG23_Fall_Temp <- readRDS(paste0(datapath, "sg23-temperature-fall-stats.rds"))
SG23_Fall_Sal <- readRDS(paste0(datapath, "sg23-salinity-fall-stats.rds"))
SG23_Fall_Oxy <- readRDS(paste0(datapath, "sg23-oxygen-fall-stats.rds"))

SG28_Fall_Temp <- readRDS(paste0(datapath, "sg28-temperature-fall-stats.rds"))
SG28_Fall_Sal <- readRDS(paste0(datapath, "sg28-salinity-fall-stats.rds"))
SG28_Fall_Oxy <- readRDS(paste0(datapath, "sg28-oxygen-fall-stats.rds"))

#Add station name as another column to each tibble:

GULD3_Fall_Temp$station <- "GULD_03"
GULD3_Fall_Sal$station <- "GULD_03"
GULD3_Fall_Oxy$station <- "GULD_03"

GULD4_Fall_Temp$station <- "GULD_04"
GULD4_Fall_Sal$station <- "GULD_04"
GULD4_Fall_Oxy$station <- "GULD_04"

SG23_Fall_Temp$station <- "SG_23"
SG23_Fall_Sal$station <- "SG_23"
SG23_Fall_Oxy$station <- "SG_23"

SG28_Fall_Temp$station <- "SG_28"
SG28_Fall_Sal$station <- "SG_28"
SG28_Fall_Oxy$station <- "SG_28"


#Add variable name as another column to each tibble:

GULD3_Fall_Temp$variable <- "Temperature"
GULD3_Fall_Sal$variable <- "Salinity"
GULD3_Fall_Oxy$variable <- "Oxygen"

GULD4_Fall_Temp$variable <- "Temperature"
GULD4_Fall_Sal$variable <- "Salinity"
GULD4_Fall_Oxy$variable <- "Oxygen"

SG23_Fall_Temp$variable <- "Temperature"
SG23_Fall_Sal$variable <- "Salinity"
SG23_Fall_Oxy$variable <- "Oxygen"

SG28_Fall_Temp$variable <- "Temperature"
SG28_Fall_Sal$variable <- "Salinity"
SG28_Fall_Oxy$variable <- "Oxygen"



Temp_AllStations_fall <- rbind.fill(GULD3_Fall_Temp[c("pressure", "smean", "station", "variable")],
                               SG28_Fall_Temp[c("pressure", "smean", "station", "variable")],
                               GULD4_Fall_Temp[c("pressure", "smean", "station", "variable")],
                               SG23_Fall_Temp[c("pressure", "smean", "station", "variable")])


Sal_AllStations_fall <- rbind.fill(GULD3_Fall_Sal[c("pressure", "smean", "station", "variable")],
                              SG28_Fall_Sal[c("pressure", "smean", "station", "variable")],
                              GULD4_Fall_Sal[c("pressure", "smean", "station", "variable")],
                              SG23_Fall_Sal[c("pressure", "smean", "station", "variable")])

Oxy_AllStations_fall <- rbind.fill(GULD3_Fall_Oxy[c("pressure", "smean", "station", "variable")],
                              SG28_Fall_Oxy[c("pressure", "smean", "station", "variable")],
                              GULD4_Fall_Oxy[c("pressure", "smean", "station", "variable")],
                              SG23_Fall_Oxy[c("pressure", "smean", "station", "variable")])


#Combine the temperature, salinity, and oxygen fall data

Combined_fall <- rbind.fill(Temp_AllStations_fall, Sal_AllStations_fall, Oxy_AllStations_fall)

Combined_fall$Season <- "Fall"


#Save as an RDS if you wish
#saveRDS(Combined_fall, file = "Combined_fall_GullyProfiles_ForComparison.RDS")


#Reorder station and parameter levels

Combined_fall$variable<-ordered(Combined_fall$variable, levels = c("Temperature",
                                                         "Salinity",
                                                         "Oxygen"))

Combined_fall$station<-ordered(Combined_fall$station, levels = c("GULD_03",
                                                       "SG_28",
                                                       "GULD_04",
                                                       "SG_23"))



#Preliminary plot for fall data only (not included in publication)

# tiff("FallTempSalOxy_AllStations.tiff", units="in", width=10, height=5, res=400)
#
# ggplot_fall<- ggplot(Combined_fall, aes(x = pressure, y = smean, colour=station, group=station)) +
#   geom_line(size=1) +
#   coord_flip() +
#   scale_x_reverse() +
#   scale_colour_manual(labels = c("GULD_03", "SG_28", "GULD_04", "SG_23"),
#                       values = c("blue", "green", "black", "red")) +
#   xlab("\nDepth (m)") +
#   labs(color='Station')+
#   theme_bw()+
#   theme(axis.text.y=element_text(size=13))+
#   theme(axis.text.x=element_text(size=13)) +
#   theme(axis.title.y=element_text(size=14)) +
#   theme(legend.text=element_text(size=13))+
#   theme(legend.title=element_text(size=14))+
#   theme(axis.title.x=element_blank())
#
# ggplot_fall+ facet_grid(.~variable, scales="free") +
#   theme(strip.text.x = element_text(size=14, face="bold"))+
#   guides(fill=FALSE)
#
# dev.off()



# Combine Data from Both Seasons and Plot (Final Figure 8) ----------------


Combined_seasons <- rbind.fill(Combined_spring, Combined_fall)


#Reorder spring data before fall

Combined_seasons$Season<-ordered(Combined_seasons$Season, levels = c("Spring", "Fall"))


Combined_seasons$variable <- factor(Combined_seasons$variable, levels = c("Temperature", "Salinity","Oxygen"),
                                                            labels = c("Temperature~(degree*C)", "Salinity", "Oxygen~(ml~L^{-1})"))




fig08 <- ggplot(Combined_seasons, aes(x = pressure, y = smean, colour=station, group=station)) +
  geom_line(size=1) +
  coord_flip() +
  scale_x_reverse() +
  scale_colour_manual(labels = c("GULD_03", "SG_28", "GULD_04", "SG_23"),
                      values = c("dodgerblue2", "gray35", "orange", "gray65")) +
  xlab("\nDepth (m)") +
  labs(color='Station') +
  theme_bw() +
  theme(axis.text.y=element_text(size=13)) +
  theme(axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=15)) +
  theme(legend.text=element_text(size=13)) +
  theme(legend.title=element_text(size=14)) +
  theme(axis.title.y=element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x=element_blank())

fig08 <- fig08 + facet_grid(Season ~ variable, scales="free", labeller = label_parsed) +
  theme(strip.text.x = element_text(size=14)) +
  theme(strip.text.y = element_text(size=14)) +
  guides(fill=FALSE)

figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure08.png"), units = "in", width = 10, height = 6, res = 400)

print(fig08)

dev.off()
