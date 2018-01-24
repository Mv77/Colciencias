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
tholds_conditional <- 0.1
# Should a table be printed in tex?
print_tab <- F


# Matching parameters (to be used only if match == T)
genetic <- T
M <- 1
caliper <- NULL
replace <- T
popsize <- 5000

# Setup ----

# Set seed
set.seed(1)

# Infix for results
infix <- ifelse(genetic,"GEN","MAH")

# Load functions
debugSource("Code/funs/functions.R")

# Load data
load("Data/data_proc.RData")

# Merge treatment variables ----
for (thold in tholds){
  
  # Load and merge treatment data
  load(paste("Results/Treatment/Treatment_",100*thold,".RData", sep = ""))
  
  data <- merge(data, treatment, by = "codmpio", all.x = T)
  
  treat <- names(treatment)[2]
  
  # Those not treated or protected can serve as controls
  data[[treat]][is.na(data[[treat]])] <- "Control"
  
}

tables <- list()
for (thold in tholds){
  
  treat <- paste("status",100*thold,sep = "_")
  
  # Assign "Status"
  data$Status <- data[[treat]]
  
  # Create a dataset with only treated and controls, and relevant variables
  dep_d <- paste("d_",deps, sep ="")
  data_m <- subset(data,
                   subset = ano == 1993 & Status %in% c("Treated","Control"),
                   select = c("codmpio","Status",controls,deps,dep_d))
  # Only complete rows
  data_m <- subset(data_m, complete.cases(data_m))
  
  # Matching
  if (match) {
    
    m <- match_prot(data = data_m, id = "codmpio", treatment = "Status",
                    controls = controls, dep = deps, genetic = genetic,
                    thold = thold, dif = T, M = M, caliper = caliper,
                    replace = replace, popsize = popsize)
  
  } else {
    
    load(paste("Results/Match/Match_",100*thold,"_",infix,".RData", sep =""))
    
  }
  
  # Find effect estimates and format them in tables
  est <- estimate_atts(data = data_m,
                       match = m,
                       dependents = deps,
                       thold = thold,
                       dif = T) %>%
    
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
  
  
  
  # Conditional ATT plots
  if (thold %in% tholds_conditional) {
   
    plots <- conditional_plot(data = data_m, m = m, dep = dep_d)
    
    for (p in plots) {
      
      print(p$plot)
      
    }
     
  }
  
}

# Bind tables vertically
tab_result <- do.call(rbind, tables)

# Order by outcome and treatment
tab_result <- tab_result[order(tab_result$Outcome,tab_result$Treatment),]

# Printing of tables ----
if ( print_tab ){
  
  print.xtable(xtable(table),
               sanitize.text.function = function(x){x},
               file = paste("Results/Tables/Main_results_",infix,".tex"),
               booktabs = F,
               floating = F,
               hline.after = c(),
               only.contents = T,
               include.colnames = F,
               include.rownames = F)
  
}
