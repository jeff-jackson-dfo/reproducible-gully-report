# Create 8 CTD Profile Plots in one figure that display Season (spring and fall)
# for the specified input parameter at each of the four Gully MPA Monitoring
# Stations.

# Created: 17-NOV-2020
# Author: Jeff Jackson
# Last updated: 01-SEP-2021

# library(fs)
# library(tidyverse)
# library(dplyr)
# library(ggplot2)
# library(ggthemes)

# source("computeStats.R")

plotProfilesByParamGG <- function(dataset, station, season, param, xlim, ylim, pnum, dpath) {

  if (param == "oxygen") {
    # Only use data after 2012 because oxygen data prior to 2013 was not corrected.
    iy <- unlist(lapply(dataset, function(x) x@metadata$startTime > "2012-12-31"))
    dataset <- dataset[iy]
  }

  fstation <- ""
  if (station == "GULD_03") {
    fstation <- "guld03"
  } else if (station == "GULD_04") {
    fstation <- "guld04"
  } else if (station == "SG_23") {
    fstation <- "sg23"
  } else if (station == "SG_28") {
    fstation <- "sg28"
  }

  # cat(paste0("\nProducing plot of ", station, " ", season, " ", param, " profiles"), "\n")

  # Get a list of pressure channels from all of the ODF objects.
  pres <- lapply(dataset, function(k) k@data$pressure)

  # Get the maximum pressure from each ctd object but only return the minimum
  # pressure of the bunch. The minimum will be used to plot the mean and
  # confidence intervals
  maximum_pressures <- unlist(lapply(pres, function(k) max(k)))

  # Remove any pressures less than 400 m.
  deep_ctds <- maximum_pressures > 400
  maximum_pressures <- maximum_pressures[deep_ctds]

  # Use the shallowest maximum pressure as the point where the mean line and
  # confidence intervals will stop being plotted.
  maxp <- min(maximum_pressures)

  # Create a tibble of data from the CTD profiles.
  tblCTD = tibble()
  for (i in seq_along(dataset)) {
    ctd <- dataset[[i]]
    # print(ctd@metadata$filename)

    # If the current file being processed is a HUD2008037 mission profile then
    # make the oxygen data channel null since no oxygen sensor data was collected.
    if (ctd@metadata$cruiseNumber == "HUD2008037")
    {
      tblTemp <- tibble(mission = rep(ctd@metadata$cruiseNumber, length(ctd@data$pressure)),
                        event = rep(ctd@metadata$eventNumber, length(ctd@data$pressure)),
                        pressure = ctd@data$pressure,
                        temperature = ctd@data$temperature,
                        salinity = ctd@data$salinity,
                        sigmatheta = ctd@data$sigmaTheta,
                        oxygen = rep(NA, length(ctd@data$pressure)),
                        filename = path_file(ctd@metadata$filename))
    } else {
      tblTemp <- tibble(mission = rep(ctd@metadata$cruiseNumber, length(ctd@data$pressure)),
                        event = rep(ctd@metadata$eventNumber, length(ctd@data$pressure)),
                        pressure = ctd@data$pressure,
                        temperature = ctd@data$temperature,
                        salinity = ctd@data$salinity,
                        sigmatheta = ctd@data$sigmaTheta,
                        oxygen = ctd@data$oxygen,
                        filename = path_file(ctd@metadata$filename))
    }
    tblCTD <- bind_rows(tblCTD, tblTemp)
  }

  if (pnum == 1) {
    xlabel <- paste0(station, "\n")
    xaxlabels <- waiver()
    ylabel <- ""
    # ylabel <- "Pressure (dbar)\n"
    yaxlabels <- waiver()
  } else if ((pnum >= 2) & (pnum <= 4)) {
    xlabel <- paste0(station, "\n")
    xaxlabels <- waiver()
    ylabel <- NULL
    yaxlabels <- NULL
  } else if (pnum == 5) {
    xlabel <- NULL
    xaxlabels <- NULL
    ylabel <- ""
    # ylabel <- "Pressure (dbar)\n"
    yaxlabels <- waiver()
  } else if (pnum > 5) {
    xlabel <- NULL
    xaxlabels <- NULL
    ylabel <- NULL
    yaxlabels <- NULL
  }

  # Create the profile plot for the current station and parameter.
  gg <- tblCTD %>%
    ggplot(aes_string(x = param, y = "pressure")) +
    geom_path(aes(shape = filename), color = "gray") +
    scale_x_continuous(limits = xlim, position = "top", labels = xaxlabels) +
    scale_y_reverse(limits = ylim, labels = yaxlabels) +
    labs(x = xlabel, y = ylabel) +
    theme_bw() +
    theme(axis.text = element_text(size = 12, face = "bold")) +
    theme(axis.title.x = element_text(size = 14, face = "bold")) +
    if (is.null(ylabel)) {
      theme(axis.text.y = element_blank())
    } +
    if (is.null(xlabel)) {
      theme(axis.text.x = element_blank())
    } +
    if (pnum == 1) {
      theme(plot.margin = margin(5, 1, 1, 5))
    } else if ((pnum == 2) | (pnum == 3)) {
      theme(plot.margin = margin(5, 1, 1, 1))
    } else if (pnum == 4) {
      theme(plot.margin = margin(5, 0, 1, 1))
    } else if (pnum == 5) {
      theme(plot.margin = margin(1, 1, 0, 5))
    } else if ((pnum == 6) | (pnum == 7)) {
      theme(plot.margin = margin(1, 1, 0, 1))
    } else if (pnum == 8) {
      theme(plot.margin = margin(1, 0, 0, 1))
    } +
    theme(legend.position = "none", plot.margin=margin(l=0, unit="cm"))

  # theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"))

  # Compute the summary statistics at each pressure for the current parameter.
  dfStats <- computeStats(dataset[deep_ctds], "pressure", param)

  # Remove all rows greater than maxp and less than 10 because only the mean and
  # confidence intervals should be plotted when all profiles have data and are
  # not strongly influenced by turbulent surface values.
  surface_rows_to_delete <- dfStats$pressure < 10
  dfStats <- dfStats[surface_rows_to_delete == FALSE,]
  deep_rows_to_delete <- dfStats$pressure > maxp
  dfStats <- dfStats[deep_rows_to_delete == FALSE,]

  # Save the stats for the current monitoring station and parameter combination.
  saveRDS(dfStats, paste0(datapath, fstation, "-", param, "-", season, "-stats.rds"))

  # Add the mean to the plot plus the 95% confidence limits.
  gg <- gg + geom_path(data = dfStats, aes(smean, pressure), col = "black", lwd = 1, lty = 1)
  gg <- gg + geom_path(data = dfStats, aes(upper_ci, pressure), col = "red", lwd = 0.5, lty = "dashed")
  gg <- gg + geom_path(data = dfStats, aes(lower_ci, pressure), col = "red", lwd = 0.5, lty = "dashed")

  if (pnum == 4) {
    if (param == "oxygen") {
      gg <- gg + annotate(geom = "text", x = 8, y = 2300, label = "Spring",
                          color = "black", size = 7, fontface = "bold")
    } else  if (param == "salinity") {
      gg <- gg + annotate(geom = "text", x = 34.5, y = 2300, label = "Spring",
                          color = "black", size = 7, fontface = "bold")
    } else  if (param == "temperature") {
      gg <- gg + annotate(geom = "text", x = 15, y = 2300, label = "Spring",
                          color = "black", size = 7, fontface = "bold")
    }
  }
  if (pnum == 8) {
    if (param == "oxygen") {
      gg <- gg + annotate(geom = "text", x = 8.5, y = 2400, label = "Fall",
                          color = "black", size = 7, fontface = "bold")
    } else  if (param == "salinity") {
      gg <- gg + annotate(geom = "text", x = 35, y = 2400, label = "Fall",
                          color = "black", size = 7, fontface = "bold")
    } else  if (param == "temperature") {
      gg <- gg + annotate(geom = "text", x = 17, y = 2400, label = "Fall",
                        color = "black", size = 7, fontface = "bold")
    }
  }

  print(gg)

  return(gg)
}
