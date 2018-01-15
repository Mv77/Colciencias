library(data.table)
library(Matching)
library(wrapr)
library(xtable)

# Do gennetic matching on protected areas with threshold as parameter,
# Save balance tests and matching results.
match_prot <- function(data, id = "codmpio", status = "Status",
                       controls, dep, thold = 0.1, dif = T,
                       M = 1, caliper = NULL, replace = TRUE, popsize = 1000){
  
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
                 M = M,
                 caliper = caliper,
                 replace = replace,
                 pop.size = popsize,
                 wait.generations = 20)
  
  # Create a list of prameters
  params <- list("M" = M, "caliper" = caliper, "replace" = replace)
  
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
  
  save(data_m,gm,id,status,controls,dep,params,
       file = paste("Results/Match/Match_",100*thold,".RData", sep =""))
  
  return(list("balance" = balance,
              "data"=data_m,
              "gm"=gm,
              "params" = params))
  
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


# Estimates atts with different methods
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
                                              X = data_m[,controls, with = F],
                                              M = params$M,
                                              caliper = params$caliper,
                                              replace = params$replace))
  
  # Extract
  att_match <- sapply(att_match, function(x) c(Est = x$est,
                                               SE = x$se,
                                               tstat =  x$est/x$se,
                                               pval = (1- pnorm(abs(x$est/x$se)))*2,
                                               Nobs = x$wnobs*2))
  
  # Bias corrected estimates ----
  att_bias <- lapply(outc, function(x) Match(estimand = "ATT",
                                             Y = x,
                                             Tr =data_m$t,
                                             Weight.matrix = gm,
                                             X = data_m[,controls, with = F],
                                             BiasAdjust = T,
                                             M = params$M,
                                             caliper = params$caliper,
                                             replace = params$replace))
  
  # Extract
  att_bias<- sapply(att_bias, function(x) c(Est = x$est,
                                            SE = x$se,
                                            tstat =  x$est/x$se,
                                            pval = (1- pnorm(abs(x$est/x$se)))*2,
                                            Nobs = x$wnobs*2))
  
  
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
          
          
          naive_lev[[d]] <- list(coef = coef(summary(est)),
                                 nobs = nobs(est))
                                 
        })
    
  }
  
  naive_lev <- sapply(naive_lev, function(x) c(Est = x$coef["t",1],
                                               SE = x$coef["t",2],
                                               tstat =  x$coef["t",3],
                                               pval = x$coef["t",4],
                                               Nobs = x$nobs))
  
  
  # Naive diffs ----
  naive_dif <- list()
  for (d in dep){
    
    let(alias = c(dy = paste("d",d, sep ="_")),
        expr = {
          
          est <- lm(dy ~ t, data = data_m)
          
          naive_dif[[d]] <- list(coef = coef(summary(est)),
                                 nobs = nobs(est))
          
        })
    
  }
  
  naive_dif <- sapply(naive_dif, function(x) c(Est = x$coef["t",1],
                                               SE = x$coef["t",2],
                                               tstat =  x$coef["t",3],
                                               pval = x$coef["t",4],
                                               Nobs = x$nobs))
  
  
  
  
  results <- list("Naive_L" = naive_lev,
                  "Naive_D" = naive_dif,
                  "Match_D" = att_match,
                  "Match_D_bias" = att_bias)
  
  save(results,
       file = paste("Results/Estimates/Est_",100*thold,".RData",sep = ""))
  
  return(results)
  
}

# Formats a table of estimates
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
                    Nobs = paste("$",t[,"Nobs"],"$", sep =""),
                    row.names = rownames(t),
                    stringsAsFactors = F)
  
  return(tab)
  
}

# Plots of control distributions pre and post matching
control_balance_plots <- function(data,gm,controls,id,thold){
  
  
  # Pre-matching plot
  pret <- melt(data[,c(id,"Status",controls),with=F],
               id.vars = c(id,"Status"))
  
  p <- ggplot(data = pret, aes(  x = value, fill = Status) ) +
    theme_bw() +
    scale_fill_gdocs() +
    scale_color_gdocs() +
    geom_density(alpha = 0.3) +
    
    geom_vline(data = pret %>%
                 group_by(variable,Status) %>%
                 summarise(value = mean(value, na.rm = T)),
               aes(xintercept = value, color = Status ),
               linetype = "dashed",
               size = 1) +
    
    facet_wrap(~variable, scales = "free") +
    xlab("Value") +
    ylab("Density") +
    theme(legend.position="bottom")
  print(p)
  
  dev.copy(pdf, file = paste("Results/Images/ControlUnmatchedDist_",thold*100,".pdf",sep =""))
  dev.off()
  
  # Post - matching plot
  
  # Post-match balance plot
  
  matches <- gm$matches
  data_match <- data_m[c(matches[,1],matches[,2]),]
  
  pret <- melt(data_match[,c(id,"Status",controls),with=F],
               id.vars = c(id,"Status"))
  
  p <- ggplot(data = pret, aes(  x = value, fill = Status) ) +
    theme_bw() +
    scale_fill_gdocs() +
    scale_color_gdocs() +
    geom_density(alpha = 0.3) +
    
    geom_vline(data = pret %>%
                 group_by(variable,Status) %>%
                 summarise(value = mean(value, na.rm = T)),
               aes(xintercept = value, color = Status ),
               linetype = "dashed",
               size = 1) +
    
    facet_wrap(~variable, scales = "free") +
    xlab("Value") +
    ylab("Density") +
    theme(legend.position="bottom")
  print(p)
  
  dev.copy(pdf, file = paste("Results/Images/ControlMatchedDist_",100*thold,".pdf", sep =""))
  dev.off()
  
  
}

# Descriptive statistics tables
desc_tables <- function(data,thold, deps, controls, id) {
  
  # Load treatment info
  load(paste("Results/Treatment/Treatment_",thold*100,".RData", sep =""))
  
  # Paste treatment with data
  data <- merge(data, treatment, by = "codmpio", all.x = T)
  
  # Non - pasted units are controls as they dont have protected areas
  data[is.na(Status), Status := "Control"]
  
  # Ommit areas which are not control or treated
  data <- subset(data, Status %in% c("Treated","Control"))
  
  
  # Create indicator for different groups in which stats will be split
  data[, group := 0]
  data[ano == 1993 & Status == "Treated", group := 1]
  data[ano == 2005 & Status == "Control", group := 2]
  data[ano == 2005 & Status == "Treated", group := 3]
  data[, group := factor(group,
                         levels = c(0,1,2,3),
                         labels = c("Control 1993","Treated 1993",
                                    "Control 2005","Treated 2005"))]
  
  # Controles pretreatment
  datades <- data %>%
    subset(ano == 1993, select = c(controls,"group")) %>%
    data.frame()
  tableContinuous( vars = datades[,controls],
                   group = datades$group,
                   stats = c("n","mean","s","min","max"),
                   prec = 2,
                   print.pval = "anova",
                   pval.bound = 10^-2,
                   booktabs = T,
                   file = paste("Results/Tables/Desc_covariates_1993_",100*thold,".tex", sep =""),
                   longtable = T)
  
  # Outcomes pre y post
  datades <- data %>%
    subset(select = c(deps,"group")) %>%
    data.frame()
  tableContinuous( vars = datades[,deps],
                   group = datades$group,
                   stats = c("n","mean","s","min","max"),
                   prec = 3,
                   booktabs = T,
                   file = paste("Results/Tables/Desc_outcomes_",100*thold,".tex", sep = ""),
                   longtable = F,
                   lab = "tab:outcomes")
  
}

