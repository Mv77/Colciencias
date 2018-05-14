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
genetic <- T


# Infix
infix <- ifelse(genetic,"GEN","MAH")

# Load data
load("Data/data_proc.RData")

for (thold in tholds){
  
  load(paste("Results/Match/Match_",100*thold,"_",infix,".RData",sep =""))
  
  # Pre and post-match balance plots
  plots <- control_balance_plots(data = data, m = m, controls, id = "codmpio", thold, control_names)
  
  print(plots$Pre)
  dev.copy(pdf, file = paste("Results/Images/ControlUnmatchedDist_",thold*100,"_",infix,".pdf",sep =""))
  dev.off()
  
  print(plots$Post)
  dev.copy(pdf, file = paste("Results/Images/ControlMatchedDist_",thold*100,"_",infix,".pdf",sep =""))
  dev.off()
  
  # Tables of descriptive statistics
  desc_tables(data,m = m, deps, controls, id, dep_names = dep_names, control_names = control_names)
  
}