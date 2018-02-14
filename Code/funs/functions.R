library(data.table)
library(Matching)
library(wrapr)
library(xtable)
library(ggplot2)
library(ggthemes)

# Load plm function
source("Code/funs/plm.R")

# Do gennetic matching on protected areas with threshold as parameter,
# Save balance tests and matching results.
match_prot <- function(data, id = "codmpio", treatment = "Status",
                       controls, dep, genetic = T, thold = 0.1, dif = T,
                       M = 1, caliper = NULL, replace = TRUE, popsize = 1000){
  
  # Setup infix for saving results
  infix <- ifelse(genetic,"GEN","MAH")
  
  # Create a numeric index for treatment
  data$t <- ifelse(data[[treatment]] == "Treated",1,0)
  
  # Matching

  if (genetic){
    
    
    # Genetic matching
    weightm <- GenMatch(Tr = data$t,
                   X = data[,controls, with = F],
                   M = M,
                   caliper = caliper,
                   replace = replace,
                   pop.size = popsize,
                   wait.generations = 40)
    
    weight <- 3
    
  } else {
    
    # Mahalanobis distance matching
    weightm <- NULL
    weight <- 2
    
  }
  
  
  
  # Create a list of prameters
  params <- list("M" = M, "caliper" = caliper, "replace" = replace)
  
  # Formula for balance diagnostic
  form <- paste(controls, collapse = "+")
  form <- paste("t ~",form)
  
  # Pre and post matching balance diagnostic
  mt <- Match(Tr = data$t,
              Weight.matrix = weightm,
              Weight = weight,
              X = data[,controls, with = F])
  
  bal <- MatchBalance(formula(form),
                      data = data,
                      nboots = 1000,
                      match.out = mt)
  
  
  balance <- bal_tab(bal, controls)
  table <- xtable(balance)
  print.xtable(table,
               file = paste("Results/Tables/Balance_",100*thold,"_",infix,".tex",sep = ""),
               booktabs = T,
               floating = F)
  
  id_controls <- data[[id]][mt$index.control]
  
  id_treated <- data[[id]][mt$index.treated]
  
  id_unused <- setdiff(data[[id]], c(id_controls,id_treated))
  
  m <- list("id" = id,
            "treatment" = treatment,
            "controls" = controls,
            "balance" = balance,
            "weight_matrix"=weightm,
            "weight" = weight,
            "id_controls" = id_controls,
            "id_treated" = id_treated,
            "id_unused" = id_unused,
            "params" = params)
  
  save(m, file = paste("Results/Match/Match_",100*thold,"_",infix,".RData", sep =""))
  
  return(m)
  
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
estimate_atts <- function(data, match, dependents, thold, dif){
  
  if (dif) {
    outcomes <- paste("d",dependents,sep="_")
  } else {
    # TODO
  }
  
  # Create a numeric index for treatment
  data$t <- ifelse(data[[m$treatment]] == "Treated",1,0)
  
  # Extract outcomes
  outc <- data[,outcomes, with = F]
  
  # Matching Estimates
  
  # Compute estimates with the genetic match
  att_match <- lapply(outc, function(x) Match(estimand = "ATT",
                                              Y = x,
                                              Tr =data$t,
                                              Weight.matrix = match$weight_matrix,
                                              Weight = match$weight,
                                              X = data[,match$controls, with = F],
                                              M = match$params$M,
                                              caliper = match$params$caliper,
                                              replace = match$params$replace))
  
  # Extract
  att_match <- sapply(att_match, function(x) c(Est = x$est,
                                               SE = x$se,
                                               tstat =  x$est/x$se,
                                               pval = (1- pnorm(abs(x$est/x$se)))*2,
                                               Nobs = x$wnobs*2))
  
  # Bias corrected estimates ----
  att_bias <- lapply(outc, function(x) Match(estimand = "ATT",
                                             Y = x,
                                             Tr =data$t,
                                             Weight.matrix = match$weight_matrix,
                                             Weight = match$weight,
                                             X = data[,match$controls, with = F],
                                             BiasAdjust = T,
                                             M = match$params$M,
                                             caliper = match$params$caliper,
                                             replace = match$params$replace))
  
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
          
          data$y2005 <- data$y + data$dy
          
          est <- lm(y2005 ~ t, data = data)
          
          
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
  for (d in dependents){
    
    let(alias = c(dy = paste("d",d, sep ="_")),
        expr = {
          
          est <- lm(dy ~ t, data = data)
          
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
control_balance_plots <- function(data, m, controls,id,thold, control_names){
  
  # Control_names contains the labels for control variables in the same order for plots
  
  
  # Subset data to only those passed to the matching procedure
  data <- subset(data,
                 ano == 1993 & codmpio %in% c(m$id_controls,m$id_treated,m$id_unused))
  
  # Create a dataset with treatment status
  t <- rbind(data.table("codmpio" = unique(m$id_controls), "Status" = "Matched Control"),
             data.table("codmpio" = m$id_treated, "Status" = "Treated"),
             data.table("codmpio" = m$id_unused, "Status" = "Unused Control"))
  
  data <- merge(data, t, by = "codmpio", all.x = T)
  
  data[, Treatment := "Control"]
  data[Status == "Treated", Treatment := "Treated"]
  
  # Pre-matching plot
  pret <- melt(data[,c(id,"Treatment",controls),with=F],
               id.vars = c(id,"Treatment"))
  
  # Substitute labels
  pret[, variable := as.character(variable)]
  
  for (i in seq_along(controls)){
    
    pret[, variable := gsub(controls[i],
                            control_names[i],
                            variable)]
    
  }
  
  pret[, variable := factor(variable) ]
  
  pre <- ggplot(data = pret, aes(  x = value, fill = Treatment) ) +
    theme_bw() +
    scale_fill_gdocs() +
    scale_color_gdocs() +
    geom_density(alpha = 0.3) +
    
    geom_vline(data = pret %>%
                 group_by(variable,Treatment) %>%
                 summarise(value = mean(value, na.rm = T)),
               aes(xintercept = value, color = Treatment ),
               linetype = "dashed",
               size = 1) +
    
    facet_wrap(~variable, scales = "free") +
    xlab("Value") +
    ylab("Density") +
    theme(legend.position="bottom")
  
  
  # Post - matching plot
  
  # Post-match balance plot
  
  # List of used units, with duplicates if matching is done with replacement
  used_ids <- c(m$id_controls,m$id_treated)
  
  data <- data[match(used_ids, data[[id]]),]
  
  pret <- melt(data[,c(id,"Status",controls),with=F],
               id.vars = c(id,"Status"))
  
  for (i in seq_along(controls)){
    
    pret[, variable := gsub(controls[i],
                            control_names[i],
                            variable)]
    
  }
  
  pret[, variable := factor(variable)]
  
  post <- ggplot(data = pret, aes(  x = value, fill = Status) ) +
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
  
  plots <- list("Pre" = pre, "Post" = post)
  
  return(plots)

}

# Descriptive statistics tables
desc_tables <- function(data,treatment, deps, controls, id) {
  
  # Rename treatment info
  names(treatment) <- c("codmpio","Status")
  
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

# Generate conditional effect plots similar to the ones in Hanauer & Canavire (2015)
conditional_plot <- function(data,m,dep,controls,control_names){
  
  # Keep only treated unites and matched controls
  control_pos <- match(m$id_controls, data[[m$id]])
  treated_pos <- match(m$id_treated, data[[m$id]])
  
  data <- data[c(control_pos,treated_pos),]
  
  results <- list()
  
  for (y in dep) {
    
    table <- data.frame(Variable = character(),
                        Status = character(),
                        X = numeric(),
                        Y = numeric(),
                        Upp = numeric(),
                        Low = numeric())
    
    for (x in controls){
      
      z <- setdiff(controls,x)
      
      for (stat in c("Control","Treated")){
        
        plm_res <- plm(as.data.frame(data[Status == stat,]),
                       x = x, y = y, z = z, loess = T)
        
        tab <- data.table(X = plm_res$pred.x,
                          Y = plm_res$pred.fit,
                          Upp = plm_res$pred.upp,
                          Low = plm_res$pred.low)
        
        tab$Variable <- x
        tab$Status <- stat
        
        table <- rbind(table,tab) 
        
      }
      
    }
    
    # Substitute labels
    for (i in seq_along(controls)){
      
      table[, Variable := gsub(controls[i],
                               control_names[i],
                               Variable)]
      
    }
    
    table$Variable <- as.factor(table$Variable)
    table$Status <- as.factor(table$Status)
    
    p <- ggplot(data = table, aes(x = X, y = Y, ymin = Low, ymax = Upp,
                                  colour = Status, fill = Status)) +
      theme_bw() +
      scale_color_gdocs() +
      scale_fill_gdocs() +
      
      geom_line(size = 1) +
      geom_ribbon(alpha = 0.2) +
      
      xlab("Variable value") +
      ylab(paste("Effect of variable on", gsub("D_","",toupper(y))))+
      
      geom_hline(yintercept = 0, color = "black") +
      
      facet_wrap(~Variable, scales = "free")
    
    res <- list(list("table"=table,"plot"=p))
    names(res) <- y
    results <- c(results, res)
    
  }
  
  return(results)
  
} 
