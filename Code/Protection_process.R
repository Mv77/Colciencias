# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Gustavo/Protected")

library(data.table)
library(stringr)

prot <- fread("Data/Protected_mateo.csv")
prot[, codmpio := as.numeric(MPIO_CCNCT)]

# Fix a wrong resolution
prot[, resolucion := gsub("RESOLUCION 092  DE 21/041978",
                          "RESOLUCION 092 DE 21/04/1978",
                          resolucion)]

prot[, resolucion := gsub("RESOLUCION 159 DE 6/061977",
                          "RESOLUCION 159 DE 6/06/1977",
                          resolucion)]

# Compute percentage of protection
prot[, per_prot := area_prot/area_mpio]

# Drop those with less than 1% protection (consider them shp errors)
prot <- subset(prot, per_prot > 0.01)

# Extract year of protected area
prot[, ano_prot := str_extract(resolucion,"/(19|20)[0-9][0-9]")]
prot[, ano_prot := as.numeric(gsub("/","",ano_prot))]

# Subset columns
prot <- subset(prot,
               select = c("categoria","codmpio","per_prot","ano_prot"))

write.csv(prot,
          file = "Data/Protection_allcats.csv",
          row.names = F)