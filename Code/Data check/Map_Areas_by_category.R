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

data[, Category := factor(as.character(categoria),
                          levels = c("ANU","PNN","RN","SF","SFA","SFF","VP"),
                          labels = c("Unique Natural Area",
                                     "National Natural Park",
                                     "Natural Reserve",
                                     "SF",
                                     "SFA",
                                     "SFF",
                                     "Road-Park"))]

p <- qmap("colombia", zoom = 5, maptype = "satellite") +
  geom_polygon(data = data,
               aes(x = long, y = lat,
                   group = group, fill = Category),
               size = .3,
               colour = 'black')
print(p)
dev.copy(pdf, file = "Results/Maps/Areas_by_category.pdf")
dev.off()