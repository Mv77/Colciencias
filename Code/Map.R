# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Ms/Micro Desarrollo/Trabajo")

library(foreign)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(wrapr)
library(gtools)
library(xtable)
library(maptools)
library(RColorBrewer)
library(classInt)

# Load data ----
load("Resultados/Match.RData")

# Generate set with only matched data
matches <- gm$matches
data93[, treat := as.character(treat)]
data93[matches[,2], treat := "Control pareado"]

# Read shp ----
map <- readShapePoly("map/MGN_ADM_MPIO_POLITICO",IDvar = "MPIO_CCNCT")

# Create id
map@data$codmpio <- as.numeric(as.character(map@data$MPIO_CCNCT))
# Merge with treatment info
d_map <- merge(map,
               data93[,c("codmpio","treat"),with = F],
               by = "codmpio",all.x = T)


# Colors
nbreaks <- 3
#colors <- brewer.pal(nbreaks, "YlOrRd")
colors <- brewer.pal(nbreaks, "Dark2")

# Color treatment setup
col <- rep("",nrow(d_map@data))
col[d_map@data$treat == "Control"] <- colors[1]
col[d_map@data$treat == "Control pareado"] <- colors[2]
col[d_map@data$treat == "Tratados"] <- colors[3]
col[col == ""] <- NA

# Plot
plot(d_map, col = col)

# Legend
legend(x = "top",
       bty = "n",
       legend=c("Control",
                "Control Pareado",
                "Tratamiento"),
       fill=colors,
       horiz = T)

dev.copy(png,
         file = "Final/Images/Map.png")
dev.off()
dev.copy(pdf,
         file = "Final/Images/Map.pdf")
dev.off()

