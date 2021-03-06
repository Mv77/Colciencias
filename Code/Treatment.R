# Preamble ----
rm(list=ls())

library(data.table)
library(dplyr)

# Parameters ----

# Categories considered in treatment
cats <- c("PNN","RN")
# Treshold for percentage coverage for treatment
tholds <- seq(0.1,0.4,0.1)
# Maximum year of establishment
ymax <- 1993

for (thold in tholds){
  
  # Load data ----
  prot <- fread("Data/PA Designation Mateo/Protection_allcats.csv")
  
  # Create cummulative protected percentage
  setorderv(prot, c("codmpio","categoria","ano_prot"))
  prot[, cum_prot := cumsum(per_prot), by = c("codmpio","categoria")]
  
  
  # Create treatmeent variable ----
  prot[, Protection := 0]
  
  # O: Nothing
  # 1: Excluded
  # 2: Treated
  
  # Areas with more than 1% and less than the protection threshold should be excluded
  prot[ cum_prot < thold & cum_prot >= 0.01 & categoria %in% cats, Protection := 1 ] 
  
  # Mark areas that reached protection treshold before our final year in any category
  # These are disabled as controls
  prot[ cum_prot >= thold & ano_prot <= 2005 & categoria %in% cats, Protection := 1]
  
  # Mark protected areas of our interest: those reaching protection treshold
  # before specified year in specified categories
  prot[ cum_prot >= thold & categoria %in% cats & ano_prot <= ymax, Protection := 2]
  
  # Once an area gains a status, it can only go up by adding more areas
  setorderv(prot, c("codmpio","ano_prot"))
  prot[, Protection := cummax(Protection), by = codmpio]
  
  # Create treatment table ----
  prot[, Status := max(Protection), by = codmpio]
  
  treatment <- prot %>%
    dplyr::select(c("codmpio","Status")) %>%
    unique()
  
  treatment[, Status := factor(Status, levels = c(0,1,2),
                               labels = c("Control","Excluded","Treated"))]
  
  names(treatment) <- c("codmpio",paste("status",100*thold,sep ="_"))
  
  
  # Save results ----
  save(file = paste("Results/Treatment/Treatment_",thold*100,".RData",sep = ""),
       list = c("prot","treatment","thold","ymax","cats")) 
  
}