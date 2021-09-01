###############################################
###  FIGURE 09: Temperature-Salinity plots  ###
###############################################

# Create 8 CTD T-S Plots in one figure that display Season (spring and fall)
# at each of the four Gully MPA Monitoring Stations.

# Created: 07-DEC-2020
# Author: Jeff Jackson
# Last updated: 01-SEP-2021

library(egg)
library(fs)
library(lubridate)
library(oce)

#################################################
### Produced CTD Temperature - Salinity Plots ###
#################################################

# Create 2x4 panel of plots in a figure. Show the current T-S plot at each
# station for spring and fall in a figure. There will be 8 plots total. The
# figures will be saved as PNG files.

# Load and rename the data sets.
projpath <- getwd()
datapath <- paste0(projpath, "/data/1_PhysicalData/")
load(paste0(datapath, "monitoringSitesCtdForAnalysis.RData"))
load(paste0(datapath, "monitoringSitesCtdForAnalysisPrograms.RData"))

# Coerce the "odf" objects to "ctd" objects to be able to use some of oce functions
# for "ctd" objects.
guld03_ctd <- lapply(guld03_ctd, function(x) as.ctd(x))
sg28_ctd <- lapply(sg28_ctd, function(x) as.ctd(x))
guld04_ctd <- lapply(guld04_ctd, function(x) as.ctd(x))
sg23_ctd <- lapply(sg23_ctd, function(x) as.ctd(x))

datasetSpringGuld03 <- guld03_ctd[guld03_programs == "Spring AZMP"] # 15 Profiles
datasetFallGuld03 <- guld03_ctd[guld03_programs == "Fall AZMP"]  # 12 Profiles

# Remove the bad row from the "ctd" object for the ODF file "CTD_HUD2003005_044_1_DN.ODF"
datasetSpringGuld03[[2]] <- subset(datasetSpringGuld03[[2]], pressure > 2)

datasetSpringGuld04 <- guld04_ctd[guld04_programs == "Spring AZMP"] # 10 Profiles
datasetFallGuld04 <- guld04_ctd[guld04_programs == "Fall AZMP"]  # 11 Profiles

datasetSpringSG23 <- sg23_ctd[sg23_programs == "Spring AZMP"] # 7 Profiles
datasetFallSG23 <- sg23_ctd[sg23_programs == "Fall AZMP"] # 9 Profiles

datasetSpringSG28 <- sg28_ctd[sg28_programs == "Spring AZMP"] # 8 Profiles
datasetFallSG28 <- sg28_ctd[sg28_programs == "Fall AZMP"] # 10 Profiles

# Initialize vectors to loop on.
seasons <- c("Spring", "Fall")
stations <- c("Guld03", "SG28", "Guld04", "SG23")

# Create color palette
pal <- colorRampPalette(c("brown", "red", "orange", "yellow", "green", "blue", "purple"))
tsColors <- pal(17)

# Create an array of the years being plotted.
tsYears <- c(c(2000),c(2002:2009),c(2011:2018))

# Create the outer axis labels to be added to the overall panel of plots.
xLabel <- "Salinity"
yLabel <- paste0("Temperature (", "\u00b0", "C)")

# Save the plot to a file.
figpath <- paste0(projpath, "/figure/")
png(paste0(figpath, "Figure09.png"), units = "in", width = 11, height = 8.5, res = 400)

# Change the plotting figure so it plots 2 rows each with four plots.
# par(mfrow=c(2,4))

m <- matrix(c(1,2,3,4,5,6,7,8), ncol = 4, byrow = TRUE)
layout(m, widths = c(0.3,0.233,0.233,0.233), heights = c(0.5,0.5))

# Generate the panel of eight T-S plots; loop through the rows.
for (x in 1:2) {

  # Get the current season
  season <- seasons[x]

  # loop through the columns
  for (y in 1:4) {

    station <- stations[y]

    datasetODF <- eval(parse(text=paste0("dataset", season, station)))

    templim <- c(0, 20)
    sallim <- c(30,36)

    # Coerce the ODF objects into CTD objects.
    datasetCTD <- lapply(datasetODF, function(x) as.ctd(x))

    nd <- length(datasetCTD)

    s <- 2:nd

    # Get the CTD start times so that each year can be extracted.
    startTime <- as.POSIXlt(unlist(lapply(datasetCTD, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
    startYear <- startTime$year + 1900
    years <- unique(startYear)

    # Create the break points for the plots as the middle of each year.
    zbreakyears <- c(sort(years)[1] - 1, sort(years)) + 0.5

    # Get the spring and fall occupations.
    startMonth <- month(startTime)

    zcm <- years
    zlim <- range(startYear)

    # Get the colors associated for each year represented in the current data set.
    ix <- which(tsYears %in% years)
    tsDsColors <- tsColors[ix]

    # Color T-S plot
    ctd <- datasetCTD[[1]]
    if (x == 1) {
      if (y == 1) {
        plotTS(ctd, inSitu = TRUE, Tlim = templim, Slim = sallim, pt.bg = tsDsColors[1], lwd = 1, cex = 2, cex.rho = 1, xaxt = "n", xlab = "", yaxt = "n", ylab = "", mar=c(0.5,6,5,0.75))
        axis(2, at = seq(0,20,2), cex.axis = 1.7, font.axis = 2)
        box()
        title(main = "GULD_03", line = 2, cex.main = 3, font.main = 2)
      } else if (y == 2) {
        plotTS(ctd, inSitu = TRUE, Tlim = templim, Slim = sallim, pch = 21, pt.bg = tsDsColors[1], lwd = 1, cex = 2, cex.rho = 1, xaxt = "n", xlab = "", yaxt = "n", ylab = "", mar=c(0.5,0.5,5,0.75))
        title(main = "SG_28", line = 2, cex.main = 3, font.main = 2)
      } else if (y == 3) {
        plotTS(ctd, inSitu = TRUE, Tlim = templim, Slim = sallim, pch = 21, pt.bg = tsDsColors[1], lwd = 1, cex = 2, cex.rho = 1, xaxt = "n", xlab = "", yaxt = "n", ylab = "", mar=c(0.5,0.5,5,0.75))
        title(main = "GULD_04", line = 2, cex.main = 3, font.main = 2)
      } else if (y == 4) {
        plotTS(ctd, inSitu = TRUE, Tlim = templim, Slim = sallim, pch = 21, pt.bg = tsDsColors[1], lwd = 1, cex = 2, cex.rho = 1, xaxt = "n", xlab = "", yaxt = "n", ylab = "", mar=c(0.5,0.5,5,1))
        title(main = "SG_23", line = 2, cex.main = 3, font.main = 2)
      }
    } else if (x == 2) {
      if (y == 1) {
        plotTS(ctd, inSitu = TRUE, Tlim = templim, Slim = sallim, pch = 21, pt.bg = tsDsColors[1], lwd = 1, cex = 2, cex.rho = 1, xaxt = "n", xlab = "", yaxt = "n", ylab = "", mar=c(6,6,0.75,0.75))
        axis(1, at = seq(30,36,1), cex.axis = 1.7, font.axis = 2)
        box()
        axis(2, at = seq(0,20,2), cex.axis = 1.7, font.axis = 2)
        box()
      } else if (y == 2) {
        plotTS(ctd, inSitu = TRUE, Tlim = templim, Slim = sallim, pch = 21, pt.bg = tsDsColors[1], lwd = 1, cex = 2, cex.rho = 1, xaxt = "n", xlab = "", yaxt = "n", ylab = "", mar=c(6,0.5,0.75,0.75))
        axis(1, at = seq(30,36,1), cex.axis = 1.7, font.axis = 2)
        box()
      } else if (y == 3) {
        plotTS(ctd, inSitu = TRUE, Tlim = templim, Slim = sallim, pch = 21, pt.bg = tsDsColors[1], lwd = 1, cex = 2, cex.rho = 1, xaxt = "n", xlab = "", yaxt = "n", ylab = "", mar=c(6,0.5,0.75,0.75))
        axis(1, at = seq(30,36,1), cex.axis = 1.7, font.axis = 2)
        box()
      } else if (y == 4) {
        plotTS(ctd, inSitu = TRUE, Tlim = templim, Slim = sallim, pch = 21, pt.bg = tsDsColors[1], lwd = 1, cex = 2, cex.rho = 1, xaxt = "n", xlab = "", yaxt = "n", ylab = "", mar=c(6,0.5,0.75,1))
        axis(1, at = seq(30,36,1), cex.axis = 1.7, font.axis = 2)
        box()
      }
    }
    yr <- which(years == 2013)
    for (i in s) {
      ctd <- datasetCTD[[i]]
      if ((season == "Spring") & (station == "SG28")) {
        if (i == yr) {
          # print(ctd@metadata$filename)
          points(ctd[['salinity2']], ctd[['temperature2']], pch = 21, bg = tsDsColors[i], lwd = 0.5, cex = 2)
        } else {
          points(ctd[['salinity']], ctd[['temperature']], pch = 21, bg = tsDsColors[i], lwd = 0.5, cex = 2)
        }
      } else {
        points(ctd[['salinity']], ctd[['temperature']], pch = 21, bg = tsDsColors[i], lwd = 0.5, cex = 2)
      }
    }

    if ((season == "Spring") & (station == "Guld03")) {
      # Get the legend details without plotting the legend.
      leg <- legend(29.5, 20.5, title = "Years", legend = tsYears, col = tsColors,
                    pch = 21, text.font = 2, bg = 'lightblue', cex = 2, plot = FALSE)
      # Adjust the legend size
      leftx <- leg$rect$left
      rightx <- 30.5
      topy <- leg$rect$top
      bottomy <- 6
      # use the new coordinates to define custom
      legend(x = c(leftx, rightx), y = c(topy, bottomy), legend = tsYears,
             col = tsColors, pch = 16, text.font = 2, bty = "n", cex = 2,
             x.intersp = 0.4, y.intersp = 0.7)
    }
  }

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

  par(margin(t = 2.0, r = 0, b = 1.0, l = 1.0, unit = "in"))

}

# Add the x and y axis labels for the entire panel of plots.
mtext(xLabel, side = 1, line = -2, cex = 2, font = 2, outer = TRUE)
mtext(yLabel, side = 2, line = -2.5, cex = 2, font = 2, outer = TRUE)

dev.off()
