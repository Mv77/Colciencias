# Preamble ----
rm(list=ls())
setwd("C:/Users/Mateo/Google Drive/Ms/Micro Desarrollo/Trabajo")

library(foreign)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(wrapr)
library(gtools)
library(xtable)

# Load data ----
load("Resultados/Match.RData")

# Generate set with only matched data
matches <- gm$matches
data <- data93[c(matches[,1],matches[,2]),]

# 2d loess ----
for (outcome in deps) {
  
  ou <- paste("d",outcome,sep="_")
  
  let(alias = c(dep = ou),
      expr = {
        
        aux <- melt(data,
                    id.vars = c("codmpio","treat","ano",ou),
                    measure.vars = controls)
        
        p <- ggplot(data = aux, aes(x = value, y = dep, color = treat)) + 
          theme_bw() +
          scale_color_gdocs() +
          geom_point(size = 1) +
          geom_smooth(size = 2) +
          facet_wrap(~variable, scales = "free")
        
        print(p)
        
        dev.copy(pdf,
                 file = paste("Final/Images/",ou,"_2dloess.pdf",sep = ""))
        dev.off()
        
      })
  
}

# 3d loess----

xy <- c("discapital","forest_cover")
npoints <- 30




for (d in deps) {
  
  map <- c(zvar = paste("d",d,sep="_"),
           xvar = xy[1],
           yvar = xy[2])
  
  let(alias = map,
      expr = {
        
        # 3D loess regression
        lo3d <- loess(zvar ~ xvar + yvar, data = data)
        lo3d_cont <- loess(zvar ~ xvar + yvar, data = data[treat == "Control"])
        lo3d_trea <- loess(zvar ~ xvar + yvar, data = data[treat == "Tratados"])
        
        # Grids for x and y
        xgrid <- seq(min(data$xvar),max(data$xvar), length.out = npoints)
        ygrid <- seq(min(data$yvar),max(data$yvar), length.out = npoints)
        
        # Create joint grid
        grid <- expand.grid(xgrid,ygrid)
        names(grid) <- xy
        
        # Create z values on grid
        z <- predict(lo3d, newdata = grid, type ="response", se = F)
        z_cont <- predict(lo3d_cont, newdata = grid, type ="response", se = F)
        z_treat <- predict(lo3d_trea, newdata = grid, type ="response", se = F)
        
        # Color setup
        
        # Create a function interpolating colors in the range of specified colors
        jet.colors <- colorRampPalette( c("blue", "green") )
        # Generate the desired number of colors from this palette
        nbcol <- 25
        color <- jet.colors(nbcol)
        # Compute the z-value at the facet centres
        nrz <- nrow(z)
        ncz <- ncol(z)
        zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
        # Recode facet z-values into color indices
        facetcol <- cut(zfacet, nbcol)
        
        # Plot params
        par(mfrow=c(1,3))
        args <- list(x = xgrid, y = ygrid,
                     col = color[facetcol],
                     xlab = xy[1],ylab=xy[2], zlab = paste("d",d,sep="_"),
                     scale = T, axes = T, ticktype = "detailed",
                     theta = 40, phi = 20)
        
        do.call(persp, args = c(list(z = z_cont, main = "Control"),
                                args))
        do.call(persp, args = c(list(z = z_treat, main = "Tratados"),
                                args))
        do.call(persp, args = c(list(z = z_treat - z_cont, main = "Tratados - Control"),
                                args))
        
      })
  
}