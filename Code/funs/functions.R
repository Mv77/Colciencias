library(data.table)
library(Matching)
library(wrapr)
library(xtable)

# Do gennetic matching on protected areas with threshold as parameter,
# Save balance tests and matching results.
match_prot <- function(data, id = "codmpio", status = "Status",
                       controls, dep, thold = 0.1, dif = T){
  
  # Load and merge treatment data
  load(paste("Results/Treatment/Treatment_",100*thold,".RData", sep = ""))
  
  data <- merge(data, treatment, by = id, all.x = T)
  
  # Those not treated or protected can serve as controls
  data[is.na(Status), Status := "Control"]
  
  # Filter to 1993
  data93 <- subset(data, ano == 1993)
  
  setorderv(data93,c("Status","codmpio"))
  
  # Create a dataset with only treated and controls, and relevant variables
  dep_d <- paste("d_",dep, sep ="")
  data_m <- subset(data93,
                   subset = Status %in% c("Treated","Control"),
                   select = c(id,status,controls,dep,dep_d))
  
  # Create a numeric index for treatment
  data_m[, t := ifelse(Status == "Treated",1,0)]
  
  # Matching ----
  
  # Only complete rows
  data_m <- subset(data_m, complete.cases(data_m))
  
  # Genetic matching
  gm <- GenMatch(Tr = data_m$t,
                 X = data_m[,controls, with = F],
                 replace = F,
                 M = 1,
                 pop.size = 1000,
                 wait.generations = 20)
  
  # Formula for balance diagnostic
  form <- paste(controls, collapse = "+")
  form <- paste("t ~",form)
  
  # Pre and post matching balance diagnostic
  mt <- Match(Tr = data_m$t,
              Weight.matrix = gm,
              X = data_m[,controls, with = F])
  
  bal <- MatchBalance(formula(form),
                      data = data_m,
                      nboots = 1000,
                      match.out = mt)
  
  
  balance <- bal_tab(bal, controls)
  table <- xtable(balance)
  print.xtable(table,
               file = paste("Results/Tables/Balance_",100*thold,".tex",sep = ""),
               booktabs = T,
               floating = F)
  
  save(data_m,gm,id,status,controls,dep,
       file = paste("Results/Match/Match_",100*thold,".RData", sep =""))
  
  return(list("balance" = balance,
              "data"=data_m,
              "gm"=gm))
  
}

# Extracts a formated table with tests results for match balance
bal_tab <- function(bal,names){
  
  prematch <- lapply( bal$BeforeMatching, function(x) c("K.S prematch" = x$ks$ks.boot.pvalue,
                                                        "T.test prematch" = x$tt$p.value))
  
  postmatch <- lapply( bal$AfterMatching, function(x) c("K.S postmatch" = x$ks$ks.boot.pvalue,
                                                        "T.test postmatch" = x$tt$p.value))
  
  tests <- cbind(do.call(rbind, prematch),
                 do.call(rbind, postmatch)) %>% data.frame
  
  tests <- round(tests, digits = 2)
  
  tests <- apply(tests,2, function(x) paste(format.pval(x, digits = 2, eps = 10^-2),
                                            stars.pval(x), sep =""))
  
  
  rownames(tests) <- names
  
  return(data.frame(tests))
  
}



estimate_atts <- function(id = "codmpio", status = "Status",
                          controls, dependents, thold = 0.1, dif = T){
  
  # Load data
  load(paste("Results/Match/Match_",100*thold,".RData", sep =""))
  
  if (dif) {
    outcomes <- paste("d",dependents,sep="_")
  } else {
    # TODO
  }
  
  outc <- data_m[,outcomes, with = F]
  
  # Matching Estimates
  
  # Compute estimates with the genetic match
  att_match <- lapply(outc, function(x) Match(estimand = "ATT",
                                              Y = x,
                                              Tr =data_m$t,
                                              Weight.matrix = gm,
                                              X = data_m[,controls, with = F]))
  
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
  
  # Extract
  att_bias<- sapply(att_bias, function(x) c(Est = x$est,
                                            SE = x$se,
                                            tstat =  x$est/x$se,
                                            pval = (1- pnorm(abs(x$est/x$se)))*2))
  
  
  # Naive estimators
  
  # Naive level
  
  # Create level variables
  naive_lev <- list()
  
  for (d in dependents){
    
    let(alias = c(y = d,
                  y2005 = paste(d,"2005",sep=""),
                  dy = paste("d",d, sep ="_")),
        expr = {
          
          data_m$y2005 <- data_m$y + data_m$dy
          
          est <- lm(y2005 ~ t, data = data_m)
          summary(est)
          
          naive_lev[[d]] <- coef(summary(est))
          
        })
    
  }
  
  naive_lev <- sapply(naive_lev, function(x) c(Est = x["t",1],
                                               SE = x["t",2],
                                               tstat =  x["t",3],
                                               pval = x["t",4]))
  
  
  # Naive diffs ----
  naive_dif <- list()
  for (d in dep){
    
    let(alias = c(dy = paste("d",d, sep ="_")),
        expr = {
          
          est <- lm(dy ~ t, data = data_m)
          summary(est)
          
          naive_dif[[d]] <- coef(summary(est))
          
        })
    
  }
  
  naive_dif <- sapply(naive_dif, function(x) c(Est = x["t",1],
                                               SE = x["t",2],
                                               tstat =  x["t",3],
                                               pval = x["t",4]))
  
  
  
  
  results <- list("Naive_L" = naive_lev,
                  "Naive_D" = naive_dif,
                  "Match_D" = att_match,
                  "Match_D_bias" = att_bias)
  
  save(results,
       file = paste("Results/Estimates/Est_",100*thold,".RData",sep = ""))
  
  return(results)
  
}
