# Map for visualizing all protected areas in Colombia by category

# Preamble ----
rm(list=ls())

library(dplyr)
library(data.table)
library(ggmap)
library(ggthemes)
library(maptools)
library(broom)

# Read shp
map <- readShapePoly("Data/SHP/Areas/area_protegidaPolygon",IDvar = "id_pnn")

data <- tidy(map) %>%
        data.table()

data[, id := as.numeric(id)]

cat <- subset(map@data, select = c("id_pnn","categoria")) %>% data.table()
cat[, id := as.numeric(as.character(id_pnn))]

data <- merge(data, cat,
              by = "id", all.x = T)



p <- qmap("colombia", zoom = 5, maptype = "satellite") +
  geom_polygon(data = data,
               aes(x = long, y = lat,
                   group = group, fill = categoria),
               size = .3,
               colour = 'black')
print(p)