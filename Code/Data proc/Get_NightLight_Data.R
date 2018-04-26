library(nightlights) # Install with devtools from https://github.com/walshc/nightlights
library(data.table)

# Read SHP with Colombia's municipalities
shp <- rgdal::readOGR("Data/SHP/Municipios", "MGN_ADM_MPIO_POLITICO")

# Extract mean nightlights in 93
mean_nl_93 <- extractNightLights(directory = "Data/Nightlights93", shp,
                                 stats = c("mean"))
mean_nl_93$year <- 1993
mean_nl_93 <- subset(mean_nl_93, select = c("MPIO_CCNCT","year","night.lights.1993.mean"))
names(mean_nl_93) <- c("codigo","year","mean_nightlight")

# Extract mean nightlights in 05
mean_nl_05 <- extractNightLights(directory = "Data/Nightlights05", shp,
                                 stats = c("mean"))
mean_nl_05$year <- 2005
mean_nl_05 <- subset(mean_nl_05, select = c("MPIO_CCNCT","year","night.lights.2005.mean"))
names(mean_nl_05) <- c("codigo","year","mean_nightlight")

# Bind 93 and 05 and save
mean_nl <- rbind(mean_nl_93,mean_nl_05)
save(mean_nl,
     file = "Data/Nightlight.RData")