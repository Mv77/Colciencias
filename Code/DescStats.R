# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Ms/Micro Desarrollo/Trabajo")

library(foreign)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(reporttools)



load("Data/data_proc.RData")

idvars <- c("codmpio","treat","ano")

# Distribution plot ----
pret <- melt(data[,c(idvars,controls),with=F],
             id.vars = idvars)

p <- ggplot(data = pret[ano == 1993], aes(  x = value, fill = treat) ) +
  theme_bw() +
  scale_fill_gdocs(name = "Tratamiento") +
  geom_density(alpha = 0.3) +
  facet_wrap(~variable, scales = "free") +
  xlab("Valor") +
  ylab("Densidad") +
  theme(legend.position="bottom")
print(p)

pdf( file = "Final/Images/PreTreatDist.pdf")
print(p)
dev.off()

# Outcome dif distribution plot ----
pret <- melt(data[,c(idvars,paste("d",deps,sep="_")),with=F],
             id.vars = idvars)

p <- ggplot(data = pret[ano == 1993], aes(  x = value, fill = treat) ) +
  theme_bw() +
  scale_fill_gdocs(name = "Tratamiento") +
  geom_density(alpha = 0.3) +
  facet_wrap(~variable, scales = "free") +
  xlab("Valor") +
  ylab("Densidad") +
  theme(legend.position="bottom")
print(p)

pdf( file = "Final/Images/Unmatched_outcomes.pdf")
print(p)
dev.off()

# Mean comparison ----

setorderv(data, c("codmpio","ano"))

# Create indicator for differences
data[, group := 0]
data[ano == 1993 & treat == "Tratados", group := 1]
data[ano == 2005 & treat == "Control", group := 2]
data[ano == 2005 & treat == "Tratados", group := 3]
data[, group := factor(group,
                           levels = c(0,1,2,3),
                           labels = c("Control 1993","Tratamiento 1993",
                                      "Control 2005","Tratamiento 2005"))]

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
                 file = "Final/Tables/Cont_pretreat.tex",
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
                 file = "Final/Tables/Outcomes.tex",
                 longtable = F,
                 lab = "tab:outcomes")

