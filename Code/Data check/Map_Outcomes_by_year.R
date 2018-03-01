# Preamble ----
rm(list=ls())

library(data.table)
library(dplyr)
library(foreign)
library(ggplot2)
library(ggthemes)
library(maptools)
library(ggmap)
library(broom)

outcomes <- c("nbi","gini","gpc")

output <- "pdf"

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
    theme_map() +
    geom_polygon(data = map,
                 aes_string(x = "long", y = "lat",
                            group = "group",
                            fill = out),
                 size = .3,
                 colour = 'black') +
    scale_fill_continuous(name = toupper(out)) +
    theme(text = element_text(size=15) ) +
    facet_wrap(~ano)
  
  print(p)
  
  if (output == "pdf"){
    
    dev.copy(pdf,
             file = paste("Results/Maps/",out,".pdf",sep =""))
    dev.off()
    
  } else if (output == "png") {
    
    dev.copy(png,
             file = paste("Results/Maps/",out,".png", sep =""))
    dev.off()
    
  } else if (output == "jpeg") {
    
    dev.copy(jpeg,
             file = paste("Results/Maps/",out,".jpeg", sep =""))
    dev.off()
    
  }
  
  
  
}