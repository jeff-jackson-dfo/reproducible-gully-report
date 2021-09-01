##########################################################################
###  FIGURES 05, 06 and 07: Temperature, Salinity and Oxygen Profiles  ###
##########################################################################

# Last updated: 01-SEP-2021
# Author: Jeff Jackson

#Load packages
library(fs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(egg)  # The egg package reworked the function "ggarrange" so that plots are aligned and sized correctly.
library(ggpubr)

# Get the project's working folder
projpath <- getwd()

# Source required external functions
source(paste0(projpath, "/figure-code/plotProfilesByParamGG.R"))
source(paste0(projpath, "/figure-code/computeStats.R"))
source(paste0(projpath, "/figure-code/lower_ci.R"))
source(paste0(projpath, "/figure-code/upper_ci.R"))

# Set the limits for each parameter
plim <- c(2500,0)
templim <- c(0, 20)
sallim <- c(30,36)
stlim <- c(22,28)
o2lim <- c(2,10)

# Define the parameters to be plotted
params <- c("temperature", "salinity", "oxygen")

# Figure numbers for the three plots in the final report.
figs <- c(5:7)

# Create 2x4 panel of plots in a figure. Show the current parameter's profiles at each
# station for spring and fall in a figure. There will be 8 plots total. The figures will be
# saved as PNG files.

for (x in 1:length(params)) {

  # Get the current parameter to plot.
  param <- params[x]

  ylim <- plim
  if (param == "temperature") {
    xlim <- templim
    ptitle <- paste0("         Temperature (", "\u00b0", "C)\n")
  } else if (param == "salinity") {
    xlim <- sallim
    ptitle <- "       Salinity\n"
  } else if (param == "oxygen") {
    xlim <- o2lim
    # ptitle <- bquote(atop(bold(Oxygen~(ml~L^-1)), ""))
    ptitle <- expression(atop(paste("         ", bold(Oxygen~(ml~L^-1)), "")))
  }

  # Load and rename the data sets.
  projpath <- getwd()
  datapath <- paste0(projpath, "/data/1_PhysicalData/")
  load(paste0(datapath, "monitoringSitesCtdForAnalysis.RData"))
  load(paste0(datapath, "monitoringSitesCtdForAnalysisPrograms.RData"))

  datasetSpringGuld03 <- guld03_ctd[guld03_programs == "Spring AZMP"] # 15 Profiles
  datasetFallGuld03 <- guld03_ctd[guld03_programs == "Fall AZMP"]  # 12 Profiles

  datasetSpringGuld04 <- guld04_ctd[guld04_programs == "Spring AZMP"] # 10 Profiles
  datasetFallGuld04 <- guld04_ctd[guld04_programs == "Fall AZMP"]  # 11 Profiles

  datasetSpringSG23 <- sg23_ctd[sg23_programs == "Spring AZMP"] # 7 Profiles
  datasetFallSG23 <- sg23_ctd[sg23_programs == "Fall AZMP"] # 9 Profiles

  datasetSpringSG28 <- sg28_ctd[sg28_programs == "Spring AZMP"] # 8 Profiles
  datasetFallSG28 <- sg28_ctd[sg28_programs == "Fall AZMP"] # 10 Profiles

  # Prepare to save plot to PNG file.
  figpath <- paste0(projpath, "/figure/")
  png(file = paste0(figpath, paste0("Figure0", figs[x], ".png")), units = "in", width = 10, height = 8, bg = "white", res = 400)

  # Plot the spring profiles for GULD_03.
  p1 <- plotProfilesByParamGG(datasetSpringGuld03, "GULD_03", "spring", param, xlim, ylim, 1, datapath)

  # Plot the spring profiles for SG_28.
  p2 <- plotProfilesByParamGG(datasetSpringSG28, "SG_28", "spring", param, xlim, ylim, 2, datapath)

  # Plot the spring profiles for GULD_04.
  p3 <- plotProfilesByParamGG(datasetSpringGuld04, "GULD_04", "spring", param, xlim, ylim, 3, datapath)

  # Plot the spring profiles for SG_23.
  p4 <- plotProfilesByParamGG(datasetSpringSG23, "SG_23", "spring", param, xlim, ylim, 4, datapath)

  # Plot the fall profiles for GULD_03.
  p5 <- plotProfilesByParamGG(datasetFallGuld03, "GULD_03", "fall", param, xlim, ylim, 5, datapath)

  # Plot the fall profiles for SG_28.
  p6 <- plotProfilesByParamGG(datasetFallSG28, "SG_28", "fall", param, xlim, ylim, 6, datapath)

  # Plot the fall profiles for GULD_04.
  p7 <- plotProfilesByParamGG(datasetFallGuld04, "GULD_04", "fall", param, xlim, ylim, 7, datapath)

  # Plot the fall profiles for SG_23.
  p8 <- plotProfilesByParamGG(datasetFallSG23, "SG_23", "fall", param, xlim, ylim, 8, datapath)

  # Produce the 2x4 plot of Spring and Fall profiles at all four monitoring stations
  # for the current parameter.  Use the egg package's version of the ggarrange function
  # because it correctly lays out the plots with proper alignment and sizes.
  figure <- egg::ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,
                      labels = c("","","","","","","",""),
                      ncol = 4,
                      nrow = 2
                      )

  # Add a title over the figure and a y-axis label for all plots.
  # Include white-space for both the title and y-axis label to get them to be
  # properly centered with respect to the plot instead of the entire plotting area.
  figure <- annotate_figure(figure,
                  top = text_grob(ptitle, color = "black", face = "bold", size = 18),
                  left = text_grob("Depth (m)              ", color = "black", face = "bold", size = 18, rot = 90)
  )

  # Output the figure to the selected device (PNG file).
  print(figure)

  # Turn off the device to release the figure so it can be viewed outside of
  # RStudio without having to exit RStudio.
  dev.off()

  # Save the list of files used for spring.
  slist <- list()
  slist <- append(slist, path_file(unlist(lapply(datasetSpringGuld03, function(x) x@metadata$filename))))
  slist <- append(slist, path_file(unlist(lapply(datasetSpringGuld04, function(x) x@metadata$filename))))
  slist <- append(slist, path_file(unlist(lapply(datasetSpringSG23, function(x) x@metadata$filename))))
  slist <- append(slist, path_file(unlist(lapply(datasetSpringSG28, function(x) x@metadata$filename))))
  saveRDS(slist, paste0(datapath, "springDataForAnalysis.rds"))

  # Save the list of files used for fall.
  flist <- list()
  flist <- append(flist, path_file(unlist(lapply(datasetFallGuld03, function(x) x@metadata$filename))))
  flist <- append(flist, path_file(unlist(lapply(datasetFallGuld04, function(x) x@metadata$filename))))
  flist <- append(flist, path_file(unlist(lapply(datasetFallSG23, function(x) x@metadata$filename))))
  flist <- append(flist, path_file(unlist(lapply(datasetFallSG28, function(x) x@metadata$filename))))
  saveRDS(flist, paste0(datapath, "fallDataForAnalysis.rds"))

}
