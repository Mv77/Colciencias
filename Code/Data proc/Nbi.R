# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Gustavo/Protected")

library(data.table)
library(dplyr)


# 1993 ----

nbi93 <- fread("Data/NBI Dane/NBI_1993.csv")
nbi93 <- nbi93[,1:4]

nbi93 <- nbi93[complete.cases(nbi93),]

colnames(nbi93) <- c("codigo","mpio","municipio","nbi")

nbi93[, codigo := as.numeric(codigo)]
nbi93[, codmpio := 1000*codigo + mpio]

nbi93 <- select(nbi93, c("codmpio","nbi"))
nbi93[, ano := 1993]


# 2005 ----
nbi05 <- fread("Data/NBI Dane/NBI_2005.csv")

nbi05[, codmpio := 1000*coddp + codm]

nbi05 <- select(nbi05, c("codmpio","nbi"))
nbi05[, ano := 2005]

# Bind and save
nbi <- rbind(nbi93,nbi05)

save(list = c("nbi"),
     file = "Data/NBI Dane/nbi.RData")