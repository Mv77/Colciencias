# Preamble ----
rm(list=ls())

library(data.table)
library(dplyr)
library(foreign)
library(ggplot2)
library(maptools)
library(ggmap)
library(broom)

outcomes <- c("nbi","gini","gpc")

# Load Data
load("Data/data_proc.RData")
data <- subset(data,select = c("codmpio","ano",outcomes))


# Read shp ----
map <- readShapePoly("Data/SHP/Municipios/MGN_ADM_MPIO_POLITICO",IDvar = "MPIO_CCNCT") %>%
        tidy() %>%
        data.table()
# Create codmpio
map[, codmpio := as.numeric(id)]


map <- rbind(merge(map, data[ano == 1993],
                   by = "codmpio", all.x = T),
             merge(map, data[ano == 2005],
                   by = "codmpio", all.x = T))

for (out in outcomes){
  
  p <- ggplot() +
    theme_bw() +
    geom_polygon(data = map,
                 aes_string(x = "long", y = "lat",
                            group = "group",
                            fill = out),
                 size = .3,
                 colour = 'black') +
    facet_wrap(~ano)
  
  print(p)
  
  dev.copy(pdf,
           file = paste("Results/Maps/",out,".pdf"))
  dev.off()
  
}

# Julian ----
load("Data/data_julian.RData")
data <- data.table(data05_marg)
rm(list = setdiff(ls(),c("map","data")))
data[,  Ind := 1]

data <- merge(map, select(data, c("codmpio","Ind")),
              all.x = T)
# Plot
p <- ggplot() +
  theme_bw() +
  geom_polygon(data = data,
               aes_string(x = "long", y = "lat",
                          group = "group",
                          fill = "Ind"),
               size = .3,
               colour = 'black')
print(p)

dev.copy(pdf,
         file = "Results/Maps/Julian_obs.pdf")
dev.off()

