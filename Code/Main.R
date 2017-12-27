# Preamble ----

rm(list = ls())

library(data.table)

library(gtools)
library(dplyr)

# Set directory and seed
setwd("C:/Users/Mateo/Google Drive/Colciencias")
set.seed(1)

# Load functions
source("Code/funs/functions.R")

# Load data
load("Data/data_proc.RData")

# Process ----

for (thold in c(.1,.2,.3,.4)){

  match_prot(data = data, id = "codmpio",
             controls = controls, dep = deps, thold = thold, dif = T)

}

# This is a cycle for estimating effects
for (thold in c(.1,.2,.3,.4)){

  e <- estimate_atts(id = "codmpio", status = "Status",
                controls, dependents = deps, thold = 0.1, dif = T)

}