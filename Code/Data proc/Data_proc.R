# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Gustavo/Protected")

library(foreign)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(wrapr)
library(reporttools)

# Load PANEL CEDE
data <- read.dta(file = "Data/data.dta") %>%
  data.table()

# Data Julian ----
load("Data/data_julian.RData")

# TODO: I must find the source of rain, slope, and forest_cover to not use Julian's stuff
dataj <- subset(data05_marg, select = c("codmpio","rain","slope","forest_cover"))

data <- merge(data, dataj, by = "codmpio",
              all.x = T)


# Paste NBI ----
load("Data/NBI Dane/nbi.RData")

# Delete CEDE NBI
data[, nbi := NULL]
# Paste own
data <- merge(data, nbi, by = c("codmpio","ano"),
              all.x = T)

rm(list = setdiff(ls(),"data"))


# Marcar mpios poblacion 0
data[ pobl_tot <= 0, pobl_tot := NA]

# Cambiar nombres
names(data) <- gsub("y_corr_tribut_predial","y_predial",names(data))
names(data) <- gsub("g_cap_FBKF","FBKF",names(data))
names(data) <- gsub("areaoficialkm2","area",names(data))
names(data) <- gsub("rain","lluvia",names(data))
names(data) <- gsub("anos_est_mun","a_edu",names(data))

# Transformar variables----

# Transformar variables a porcentaje de poblacion total
# y gastos e ingrésos a percapita
data[, c("pobl_rur", "pobl_urb",
         "per_alfa", "y_total",
         "y_predial", "FBKF") := lapply(list(pobl_rur, pobl_urb, per_alfa,
                                             y_total,y_predial, FBKF),
                                        function(x) x/pobl_tot) ]

# Obtener densidad ppoblacional
data[, densidad_pob := pobl_tot/area]

# Aplicar logs
data[, c("pobl_tot", "area",
             "y_total","y_predial",
             "FBKF","altura") := lapply(list(pobl_tot, area,
                                             y_total,y_predial,FBKF,altura),
                                             function(x) log(x+1)) ]

# Marcar missings en outcomes ----
data[gpc == 0, gpc := NA]
data[gini == 0, gini := NA]

# Transformación de variables----

# Reemplazar densidad por log densidad
data[, densidad_pob := log(densidad_pob)]
# Normalizar el índice de agua por 1millon
data[, agua := agua/10^6]
# Normalizar gpc por 100.000
data[, gpc := gpc / 100000]

# Subset de variables de interes ----

# Variables of interest for balance
controls <- c("discapital","altura","slope","forest_cover","lluvia","agua","aptitud",
              "densidad_pob","indrural","a_edu")

deps <- c("nbi","gini","gpc")

data <- subset(data, select = c("codmpio","ano",controls,deps))

# Reducir a muestra con datos para ambos anos
data[, nobs := .N, by = codmpio]
data <- subset(data, nobs == 2)
data[, nobs := NULL]

# Computation of differences ----

# Subset data ----
outc <- data[,c("codmpio","ano",deps), with = F]
# Reshape wide
outc <- dcast(data, codmpio ~ ano, value.var = deps)

# Generate differences
for (var in deps){
  
  mapping = c(x = paste("d_",var,sep =""),
              x0 = paste(var,"_1993",sep = ""),
              x1 = paste(var,"_2005",sep = ""))
  
  let(alias = mapping,
      expr = {
        outc$x <- outc$x1 - outc$x0
      })
  
}

# Drop original vars
outc <- subset(outc, select = c("codmpio", grep("d_",names(outc), value = T)))
# Add differences to database
data <- merge(data,outc,
              by = "codmpio", all.x = T)

# Save results ----
save(data,controls,deps,
     file = "Data/data_proc.RData")