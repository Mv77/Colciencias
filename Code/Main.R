# Preamble ----

rm(list = ls())

library(data.table)

library(gtools)
library(dplyr)

# Parameters ----

# Should matching be carried out
match <- T
# Thresholds for treatment
tholds <- seq(.1,.4,.1)
# Should a table be printed in tex?
print_tab <- T


# Matching parameters (to be used only if match == T)
M <- 1
caliper <- NULL
replace <- T
popsize <- 1000


# Setup ----

# Set seed
set.seed(1)

# Load functions
source("Code/funs/functions.R")

# Load data
load("Data/data_proc.RData")

# Matching ----

if (match) {
  
  for (thold in tholds){
    
    match_prot(data = data, id = "codmpio",
               controls = controls, dep = deps, thold = thold, dif = T,
               M = M, caliper = caliper, replace = replace, popsize = popsize)
    
  }
  
}

# Effect estimation ----

tables <- list()
for (thold in tholds){
  
  # Find effect estimates and format them in tables
  est <- estimate_atts(id = "codmpio", status = "Status",
                       controls, dependents = deps, thold = thold, dif = T) %>%
    
    lapply(function(x) tab(x))
  
  # Bind tablees
  est <- do.call(cbind, est)
  
  # Add an outcome as a variable and an 
  # indicator for the treatment threshold, in the first position
  est <- cbind(Outcome = row.names(est),
               Treatment = paste("$",rep(thold,nrow(est)),"$", sep = ""),
               est)
  
  # Delete row names
  row.names(est) <- NULL
  
  # Add estimators to the list
  tables <- c(tables, list(est))
  
}

# Bind tables vertically
table <- do.call(rbind, tables)

# Order by outcome and treatment
table <- table[order(table$Outcome,table$Treatment),]

# Printing ----
if ( print_tab ){
  
  print.xtable(xtable(table),
               sanitize.text.function = function(x){x},
               file = "Results/Tables/Main_results.tex",
               booktabs = F,
               floating = F,
               hline.after = c(),
               only.contents = T,
               include.colnames = F,
               include.rownames = F)
  
}