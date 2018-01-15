# Preamble ----
rm(list=ls())

library(foreign)
library(data.table)
library(dplyr)
library(reporttools)

source("Code/funs/functions.R")

# Parameters ----

# Treatment tresholds
tholds <- seq(.1,.4,.1)

# Pre and post-match balance plots ----
for (thold in tholds){
  
  load(paste("Results/Match/Match_",100*thold,".RData",sep =""))
  
  control_balance_plots(data = data_m, gm = gm, controls, id, thold)
  
}

# Tables of descriptive statistics ----
load("Data/data_proc.RData")
for (thold in tholds) {
  
  desc_tables(data,thold, deps, controls, id)
  
}