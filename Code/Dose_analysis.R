# This script allows a different effect of treatment for every level of protection
# delta Y = f(percentage of coverage) + X*Beta + e

# Preamble ----
rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Parameters
thold <- 0.1

# Data pre-processing ----

# Load data
load("Data/data_proc.RData")

# plm
source("Code/funs/plm.R")

# Load treatment information (to obtain excluded areas)
load(paste("Results/Treatment/Treatment_",thold*100,".RData", sep = "") )

# Keep only protection on the desired categories
prot <- subset(prot, subset = categoria %in% cats)

# Find municipalities that had new protection between 1993 and 2005
id_exc <- subset(prot,
                 subset = ano_prot > 1993 & ano_prot < 2005,
                 select = "codmpio") %>% unique() %>% unlist()

# Exclude municipalities with protection between 1993 and 2005
prot <- subset(prot, subset = !(codmpio %in% id_exc) )

# Exclude entries after 1993
prot <- subset(prot, subset = ano_prot <= 1993)

# Find maximum protection by area and category and keep only that observation
prot[, m_prot := max(cum_prot), by = c("codmpio","categoria")]
prot <- subset(prot, subset = cum_prot == m_prot)

# Find total protection before 1993 by municipality
p <- prot %>% group_by(codmpio) %>% summarise(perc_prot_93 = sum(cum_prot))

# Paste protection percentage to data
data <- merge(data, p, by = "codmpio", all.x = T)

# Replace NA's by 0
data[is.na(perc_prot_93), perc_prot_93 := 0]

# Replace omitted municipalities with NA
data[codmpio %in% id_exc, perc_prot_93 := NA]

# Select data for estimation
ddeps <- paste("d",deps,sep = "_")
data <- subset(data, subset = ano == 1993,
               select = c("codmpio",controls,ddeps,"perc_prot_93"))

data <- subset(data, complete.cases(data))
data <- data.frame(data)

tables <- list()
for (i in seq_along(ddeps) ){
  
  y <- ddeps[i]
  
  plm_res <- plm(data, x = "perc_prot_93", y = y, z = controls, order = 10,
                 loess=T, span = 1, seq=NULL, se= T, predict= T, degree = 2, family = "gaussian")
  
  tab <- data.frame(X = plm_res$pred.x,
                    Y = plm_res$pred.fit,
                    Upp = plm_res$pred.upp,
                    Low = plm_res$pred.low)
  
  tab$outcome <- toupper(deps[i])
  
  tables <- c(tables,list(tab))
  
}

tables <- do.call(rbind,tables)
tables$outcome <- as.factor(tables$outcome)

p <- ggplot(data = tables, aes(x = X, y = Y, ymin = Low, ymax = Upp)) +
  theme_bw() +
  scale_color_gdocs() +
  scale_fill_gdocs() +
  ylab("Effect estimate") +
  xlab("Fraction of municipality area protected") +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2) +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~outcome, nrow = 3, scales = "free_y")
print(p)

# Print to pdf
dev.copy(pdf,
         file = paste("Results/Images/Dose_effects_",
                      thold*100,".pdf",sep =""))
dev.off()