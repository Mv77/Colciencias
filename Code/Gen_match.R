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

# Function defs ----

# Extracts a table with tests results for match balance
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

idvars <- c("codmpio","Status","ano")

# Load data ----
load("Data/data_proc.RData")

# Paste treatment status to data
load("Results/Treatment.RData")
data <- merge(data, treatment, by = "codmpio", all.x = T)

# Those not treated or protected can serve as controls
data[is.na(Status), Status := "Control"]

# Filter to 1993
data93 <- subset(data, ano == 1993)

setorderv(data93,c("Status","codmpio"))

# Create a dataset with only treated and controls
data_m <- subset(data93, Status %in% c("Treated","Control"))
# Create a numeric index for treatment
data_m[, t := ifelse(Status == "Treated",1,0)]

# Matching ----

# Only complete rows
data_m <- subset(data_m,complete.cases(data_m))

# Diagnostico de balance
form <- paste(controls, collapse = "+")
form <- paste("t ~",form)
MatchBalance(formula(form),
             nboots = 1000,
             data = data_m)

# Genetic matching
gm <- GenMatch(Tr = data_m$t,
               X = data_m[,controls, with = F],
               replace = F,
               M = 1,
               pop.size = 1000,
               wait.generations = 20)

# Diagnostico de balance pre y post matching.
mt <- Match(Tr = data_m$t,
            Weight.matrix = gm,
            X = data_m[,controls, with = F])

bal <- MatchBalance(formula(form),
                    data = data_m,
                    nboots = 1000,
                    match.out = mt)


table <- bal_tab(bal, controls)
table <- xtable(table)
print.xtable(table,
             file = "Results/Tables/Balance.tex",
             booktabs = T,
             floating = F)


# Save results ----
save(data_m,gm,idvars,controls,deps,
     file = "Results/Match.RData")

# Distribution plot postmatch----
matches <- gm$matches

data_match <- data_m[c(matches[,1],matches[,2]),]

pret <- melt(data_match[,c(idvars,controls),with=F],
             id.vars = idvars)

p <- ggplot(data = pret, aes(  x = value, fill = Status) ) +
  theme_bw() +
  scale_fill_gdocs(name = "Tratamiento",
                   labels = c("Control pareado","Tratamiento")) +
  geom_density(alpha = 0.3) +
  facet_wrap(~variable, scales = "free") +
  xlab("Valor") +
  ylab("Densidad") +
  theme(legend.position="bottom")
print(p)


pdf( file = "Results/Images/MatchedDist.pdf")
print(p)
dev.off()


# Outcomes
pret <- melt(data_match[,c(idvars,paste("d",deps,sep = "_")),with=F],
             id.vars = idvars)

p <- ggplot(data = pret, aes(  x = value, fill = Status) ) +
  theme_bw() +
  scale_fill_gdocs(name = "Tratamiento",
                   labels = c("Control pareado","Tratamiento")) +
  geom_density(alpha = 0.3) +
  facet_wrap(~variable, scales = "free") +
  xlab("Valor") +
  ylab("Densidad") +
  theme(legend.position="bottom")
print(p)

pdf( file = "Results/Images/Matched_outcomes.pdf")
print(p)
dev.off()