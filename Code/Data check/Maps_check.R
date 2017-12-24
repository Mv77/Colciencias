# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Gustavo/Protected")

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

# Load PANEL CEDE
dataCEDE <- read.dta(file = "Data/data.dta") %>%
  data.table()

# Data ----
load("Data/data_julian.RData")
dataj <- data.table(data05_marg)
rm(list = setdiff(ls(),c("dataCEDE","dataj")))
dataj[, julian := 1]


# Check Julian ----

# Read shp ----
map <- readShapePoly("Data/SHP/Municipios/MGN_ADM_MPIO_POLITICO",IDvar = "MPIO_CCNCT")

# Create id
map@data$codmpio <- as.numeric(as.character(map@data$MPIO_CCNCT))
# Merge with treatment info
d_map <- merge(map,
               dataj[,c("codmpio","julian"),with = F],
               by = "codmpio",all.x = T)

# Colors
nbreaks <- 2
#colors <- brewer.pal(nbreaks, "YlOrRd")
colors <- brewer.pal(nbreaks, "Dark2")

# Color setup
col <- rep("",nrow(d_map@data))
col[d_map@data$julian == 1] <- colors[1]
col[col == ""] <- NA

# Plot
plot(d_map, col = col, main = "Julian")

dev.copy(pdf,
         file = "Results/Maps/Data_Julian.pdf")
dev.off()



# Check CEDE ----
dataCEDE[,cede:=1]
dataCEDE <- subset(dataCEDE,
                   ano == 1993)
# Read shp ----
map <- readShapePoly("map/MGN_ADM_MPIO_POLITICO",IDvar = "MPIO_CCNCT")

# Create id
map@data$codmpio <- as.numeric(as.character(map@data$MPIO_CCNCT))

# Merge with treatment info
d_map <- merge(map,
               dataCEDE[,c("codmpio","cede"),with = F],
               by = "codmpio",all.x = T)

# Colors
nbreaks <- 2
#colors <- brewer.pal(nbreaks, "YlOrRd")
colors <- brewer.pal(nbreaks, "Dark2")

# Color treatment setup
col <- rep("",nrow(d_map@data))
col[d_map@data$cede == 1] <- colors[1]
col[col == ""] <- NA

# Plot
plot(d_map, col = col, main = "CEDE obs 1993")

dev.copy(pdf,
         file = "Results/Maps/Data_CEDE_obs_1993.pdf")
dev.off()




# Check protected area coverage ----
pa <- fread("Data/pa_merlin.csv")
pa <- subset(pa, !is.na(codmpio))

dataCEDE <- merge(dataCEDE,pa,by = "codmpio", all.x = T)
dataCEDE[is.na(pa.area), pa.area := 0]

# Merge with treatment info
d_map <- merge(map,
               dataCEDE[,c("codmpio","pa.area"),with = F],
               by = "codmpio",all.x = T)

# Plot
plot(d_map, col = gray(d_map@data$pa.area/100), main = "Protected areas")

dev.copy(pdf,
         file = "Results/Maps/Data_CEDE_obs_1993.pdf")
dev.off()





pobl_tot > 0 & !is.na(nbi) & gpc > 0 & gini > 0
