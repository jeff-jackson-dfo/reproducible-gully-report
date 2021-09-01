####################################################
###  FIGURES 31 and 32: Copepod Abundance Plots  ###
####################################################

# Last updated: 31-AUG-2021
# Author: Jeff Jackson

library(readxl)   # Required for reading Excel files
library(plyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(scales)
library(stringi)

projpath <- getwd()
datafilepath <- paste0(projpath, "/data/3_BiologicalData/copepod-abundances.xlsx")

# Load the Gully biological copepod taxonomic data.
springAvg <- readxl::read_excel(datafilepath, sheet = "Spring Avg")
springSD <- readxl::read_excel(datafilepath, sheet = "Spring StDev")
fallAvg <- readxl::read_excel(datafilepath, sheet = "Fall Avg")
fallSD <- readxl::read_excel(datafilepath, sheet = "Fall StDev")

# Sort the tibbles according to taxonomic names in alphabetical order
springAvg <- arrange(springAvg, Taxonomy)
springSD <- arrange(springSD, Taxonomy)
fallAvg <- arrange(fallAvg, Taxonomy)
fallSD <- arrange(fallSD, Taxonomy)

# Rename the columns to make it easier to plot the data
cnames <- c("Taxonomy", "HL_06", "Gully mouth", "GULD_03", "LL_07")
colnames(springAvg) <- cnames
colnames(springSD) <- cnames
colnames(fallAvg) <- cnames
colnames(fallSD) <- cnames

# Pivot the data frames from wider to longer
sAvg <- pivot_longer(springAvg, 2:5)
sSD <- pivot_longer(springSD, 2:5)
fAvg <- pivot_longer(fallAvg, 2:5)
fSD <- pivot_longer(fallSD, 2:5)

# Combine averages with stdev into one data frame
sTaxo <- cbind(sAvg, sSD$value)
fTaxo <- cbind(fAvg, fSD$value)

colnames(sTaxo) <- c("taxonomy", "station", "avg", "sd")
colnames(fTaxo) <- c("taxonomy", "station", "avg", "sd")

# Change the order of the site names to be in a specific order
sTaxo$station <- factor(sTaxo$station, levels = c("HL_06", "Gully mouth", "GULD_03", "LL_07"))
fTaxo$station <- factor(fTaxo$station, levels = c("HL_06", "Gully mouth", "GULD_03", "LL_07"))

# Divide the values by 1000
sTaxo$avg <- sTaxo$avg / 1000
sTaxo$sd <- sTaxo$sd / 1000
fTaxo$avg <- fTaxo$avg / 1000
fTaxo$sd <- fTaxo$sd / 1000

# Legend labels
sllab <- cnames[2:5]
slabtxt <- c(", N = 143", ", N = 169", ", N = 221", ", N = 171")
sllab <- sllab %s+% slabtxt
fllab <- cnames[2:5]
flabtxt <- c(", N = 174", ", N = 135", ", N = 90", ", N = 100")
fllab <- fllab %s+% flabtxt

# Save the images as files for importing into the report
figpath <- paste0(projpath, "/figure/")

png(paste0(figpath, "Figure31.png"), units = "in", width = 8, height = 10, res = 400)

# Produce a bar plot for the spring taxonomic data grouped by station
fig31 <- ggplot(data = sTaxo, aes(x = avg, y = reorder(taxonomy, desc(taxonomy)), fill = station)) +
              geom_bar(stat="identity", width = 0.7, position=position_dodge()) +
              geom_errorbar(aes(xmin = pmax(avg - sd, 0), xmax = avg + sd), position=position_dodge(.7), width = 0) +
              scale_fill_manual(values=c("#A9A9A9", "#FFFF00", "#FFA500", "#6495ED"), labels = sllab) +
              scale_x_continuous(expand = c(0,0), limits = c(0,200), oob = rescale_none) +
              labs(x = bquote("Abundance (1000s"~m^-2*")"), y = "") +
              theme_classic(base_size = 18) +
              theme(legend.position = c(0.6, 0.8),
                    legend.title = element_blank(),
                    axis.text.y = element_text(face = "bold.italic"),
                    axis.text.x = element_text(face = "bold"),
                    panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
                    plot.margin = unit(c(0.5,0.75,0,0),"cm")
              ) +
              guides(fill = guide_legend(reverse = TRUE))

print(fig31)

dev.off()

png(paste0(figpath, "Figure32.png"), units = "in", width = 8, height = 10, res = 400)

# Produce a bar plot for the fall taxonomic data grouped by station
fig32 <- ggplot(data = fTaxo, aes(x = avg, y = reorder(taxonomy, desc(taxonomy)), fill = station)) +
  geom_bar(stat="identity", width = 0.7, position=position_dodge()) +
  geom_errorbar(aes(xmin = pmax(avg - sd, 0), xmax = avg + sd), position=position_dodge(.7), width = 0) +
  scale_fill_manual(values=c("#A9A9A9", "#FFFF00", "#FFA500", "#6495ED"), labels = fllab) +
  scale_x_continuous(expand = c(0,0), limits = c(0,200), oob = rescale_none) +
  labs(x = bquote("Abundance (1000s"~m^-2*")"), y = "") +
  theme_classic(base_size = 18) +
  theme(legend.position = c(0.6, 0.8),
        legend.title = element_blank(),
        axis.text.y = element_text(face = "bold.italic"),
        axis.text.x = element_text(face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
        plot.margin = unit(c(0.5,0.75,0,0),"cm")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

print(fig32)

dev.off()
