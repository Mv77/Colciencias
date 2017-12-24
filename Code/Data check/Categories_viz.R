# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Gustavo/Protected")

library(dplyr)
library(data.table)
library(ggmap)
library(maptools)
library(broom)

# Read shp
map <- readShapePoly("Data/SHP/Municipios/MGN_ADM_MPIO_POLITICO",IDvar = "MPIO_CCNCT")

data <- tidy(map) %>%
        data.table()
data[, codmpio := as.numeric(id)]

# Get boundary box for colombia
box <- make_bbox(lon = data$long,
                 lat = data$lat, f = 0.3)

# Read protected designations
prot <- fread("Data/Protection_allcats.csv")

# Keep only the area with the maximum percentage of coverage
prot[, max := max(per_prot), by = codmpio]
prot <- subset(prot,
               max == per_prot & ano_prot <= 1999,
               select = c("codmpio","per_prot","ano_prot","categoria"))

# Merge with polygons
data <- merge(data, prot,
              by = "codmpio", all.x = T)

data[, categoria := factor(categoria)]

col <- get_map(location = box,
               source = "google",
               maptype = "terrain",
               force = T)

#p <- ggmap(col, legend = "topleft") +
p <- ggplot() +
  theme_bw() +
  geom_polygon(data = data[!is.na(categoria)],
               aes(x = long, y = lat,
                   group = group,
                   fill = per_prot),
               size = .3,
               colour = 'black') +
  facet_wrap(~categoria) +
  scale_fill_continuous( name = "Porcentaje\nProtegido")
  
print(p)