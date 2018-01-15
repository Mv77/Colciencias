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

# Parameters ----
tholds <- seq(0.1,0.4,0.1)


# Load protected areas shp ----
areas <- readShapePoly("Data/SHP/Areas/area_protegidaPolygon",IDvar = "id_pnn")
areasdta <- areas@data %>% data.table()
areasdta[, id_pnn := as.character(id_pnn)]


areas <- tidy(areas) %>%
  data.table()

areas <- merge(areas, areasdta[,c("id_pnn","categoria"), with =F],
               by.x = "id", by.y = "id_pnn", all.x =T)

for (thold in tholds) {
  
  # Load data ----
  load(paste("Results/Match/Match_",100*thold,".RData",sep =""))
  
  # Generate set with only matched data ----
  matches <- gm$matches
  data_m[matches[,2], Status := "Matched Control"]
  
  # Read shp ----
  map <- readShapePoly("Data/SHP/Municipios/MGN_ADM_MPIO_POLITICO",IDvar = "MPIO_CCNCT") %>%
    tidy() %>%
    data.table()
  # Create codmpio
  map[, codmpio := as.numeric(id)]
  
  # Merge map with treatment info ----
  
  data_m <- subset(data_m, select = c("codmpio","Status"))
  
  # Add municipalities that were excluded
  load(paste("Results/Treatment/Treatment_",100*thold,".RData", sep =""))
  
  data_m <- rbind(data_m, treatment[Status == "Protected"])
  
  # Merge with map
  map <- merge(map, data_m[, c("codmpio","Status"), with = F],
               by = "codmpio", all.x = T)
  
  # Change NA's for "no data"
  map[, Status := as.character(Status)]
  map[is.na(Status), Status := "No Info"]
  map[Status == "Protected", Status := "Excluded"]
  
  # Change order of factor levels
  map[, Status := factor(Status,
                         levels = c("Treated","Matched Control","Control",
                                    "Excluded","No Info"))]
  
  # Plot ----
  
  p <- ggplot() +
    theme_map() +
    scale_fill_canva(palette = "Primary colors with a vibrant twist") +
    geom_polygon(data = map,
                 aes(x = long, y = lat,
                     group = group,
                     fill = Status),
                 alpha = 1,
                 size = 0.05,
                 colour = 'black') +
    geom_polygon(data = subset(areas, categoria %in% cats),
                 aes(x = long, y = lat,
                     group = group, colour = categoria, linetype = categoria),
                 size = 1,
                 alpha = 0) +
    scale_colour_canva(name = "Protected Areas",
                       palette = "Nightlife") +
    scale_linetype(name = "Protected Areas")
  print(p)
  
  dev.copy(pdf,
           file = paste("Results/Maps/Treatment_",thold*100,".pdf",sep =""))
  dev.off()
  
}



