# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Gustavo/Protected")

library(foreign)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(Matching)
library(gtools)
library(xtable)
library(wrapr)


idvars <- c("codmpio","Status","ano")

# Load data ----
load("Results/Match.RData")

# Matching ATT estimators ----
outcomes <- paste("d",deps,sep="_")
outc <- data_m[,outcomes, with = F]

# Matching Estimates ----

# Compute estimates with the genetic match
att_match <- lapply(outc, function(x) Match(estimand = "ATT",
                                            Y = x,
                                            Tr =data_m$t,
                                            Weight.matrix = gm,
                                            X = data_m[,controls, with = F]) )
lapply(att_match, summary)

# Extract
att_match <- sapply(att_match, function(x) c(Est = x$est,
                                             SE = x$se,
                                             tstat =  x$est/x$se,
                                             pval = (1- pnorm(abs(x$est/x$se)))*2))

# Bias corrected estimates ----
att_bias <- lapply(outc, function(x) Match(estimand = "ATT",
                                           Y = x,
                                           Tr =data_m$t,
                                           Weight.matrix = gm,
                                           X = data_m[,controls, with = F],
                                           BiasAdjust = T))
lapply(att_bias, summary)

# Extract
att_bias<- sapply(att_bias, function(x) c(Est = x$est,
                                          SE = x$se,
                                          tstat =  x$est/x$se,
                                          pval = (1- pnorm(abs(x$est/x$se)))*2))


# Naive estimators ----
#######################

# Naive level ----

# Create level variables
naive_lev <- list()

for (dep in deps){
  
  let(alias = c(y = dep,
                y2005 = paste(dep,"2005",sep=""),
                dy = paste("d",dep, sep ="_")),
      expr = {
        
        data_m$y2005 <- data_m$y + data_m$dy
        
        est <- lm(y2005 ~ t, data = data_m)
        summary(est)
        
        naive_lev[[dep]] <- coef(summary(est))
        
      })
  
}

naive_lev <- sapply(naive_lev, function(x) c(Est = x["t",1],
                                             SE = x["t",2],
                                             tstat =  x["t",3],
                                             pval = x["t",4]))


# Naive diffs ----
naive_dif <- list()
for (dep in deps){
  
  let(alias = c(dy = paste("d",dep, sep ="_")),
      expr = {
        
        est <- lm(dy ~ t, data = data_m)
        summary(est)
        
        naive_dif[[dep]] <- coef(summary(est))
        
      })
  
}

naive_dif <- sapply(naive_dif, function(x) c(Est = x["t",1],
                                             SE = x["t",2],
                                             tstat =  x["t",3],
                                             pval = x["t",4]))


tab <- function(t, digits = 4){
  
  t <- t(t)
  
  t[,"Est"] <- round(t[,"Est"], digits = digits)
  t[,"SE"] <- round(t[,"SE"], digits = digits)
  
  p <- t[,"pval"]
  stars <- rep("",nrow(t))
  stars[p<0.1] <- "*"
  stars[p<0.05] <- "**"
  stars[p<0.01] <- "***"
  
  tab <- data.frame(Est = paste("$",t[,"Est"],stars,"$",sep = ""),
                    SE = paste("$(",t[,"SE"],")$", sep =""),
                    row.names = rownames(t))
  
  return(tab)
  
}

tabs <- lapply(list(naive_lev,naive_dif,att_match,att_bias),
               function(x) tab(x))

tab <- do.call(cbind, tabs)

table <- xtable(tab)
print.xtable(table,
             sanitize.text.function = function(x){x},
             file = "Results/Tables/Main_results.tex",
             booktabs = F,
             floating = F,
             hline.after = c(),
             only.contents = T,
             include.colnames = F)

# tab_res <- function(att,naive){
#   
#   tab <- rbind(att[c(1,4),], naive)
#   
#   tab <- round(t(tab), digits = 4)
#   
#   colnames(tab) <- paste(c("Match","Match","Naive","Naive"), colnames(tab),sep = ".")
#   
#   return(tab)
#   
# }
# 
# tab <- tab_res(att_match,naive_est)
# 
# table <- xtable(tab)
# print.xtable(table,
#              file = "Final/Tables/Main_results.tex",
#              booktabs = T,
#              floating = F)

