# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Ms/Micro Desarrollo/Trabajo")
source("Code/funs/plm.R")
source("Code/funs/plm_plot.R")

library(foreign)
#library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(wrapr)
library(gtools)
library(xtable)

# Load data ----
load("Resultados/Match.RData")

# Generate set with only matched data
matches <- gm$matches
data <- data93[c(matches[,1],matches[,2]),]

treated <- data93[matches[,1],]
control <- data93[matches[,2],]

ncontrols <- length(controls)
cols <- ceiling(sqrt(ncontrols))
par(mfrow = c(cols,cols))

data <- as.data.frame(data)

for (dep in paste("d",deps,sep="_")){
 
  for (con in controls) {
    
    mn <- max(min(treated[con]),min(treated[con]))
    mx <- min(max(treated[con]),max(treated[con]))
    step <- (mx - mn)/100
    
    plmplot(data = data,
            x.var = con, y.var = dep, treat = "t", controls = setdiff(controls, con),
            sp = 0.7,
            plot.all = T,
            rug = T,
            title=NULL, x.lab=con, y.lab=dep,
            min = mn,
            max = mx,
            step = step)
    
    
  }
   
}