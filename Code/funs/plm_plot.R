
plmplot <- function(data, x.var="string", y.var="string", treat="string", controls="Vector of String Names", 
										title="String", x.lab="String", y.lab="String", sp, min, max, step=1, rug=T, imp=0, plot.all=FALSE, color=NULL,
										y.min=NULL, y.max=NULL, int=NULL, d.width=16, d.height=9, return.fit=F, just.loess=F, 
										cil.in=NULL, cih.in=NULL, se.in=NULL,add.user.ci=FALSE){

if(is.null(color)){
	color="#0000ff22"
}

#Assign treatment and control data 
c <- data[data[treat] == 0,]
t <- data[data[treat] == 1,]

#data for the rug option
cx <- as.matrix(c[x.var])
cy <- as.matrix(c[y.var])
tx <- as.matrix(t[x.var])
ty <- as.matrix(t[y.var])

#order by x var
t <- t[order(t[x.var]), ]
c <- c[order(c[x.var]), ]

#user defined range for x
seq.sl <- seq(min,max,step)


#this will graph the results just using loess
if(just.loess){
	low1<-loess(cy~cx, span=sp, seq=seq.sl)
	low1p<-predict(low1, seq.sl, se = TRUE)
	cil1<-low1p$fit - low1p$se.fit*1.96
	cih1<-low1p$fit + low1p$se.fit*1.96
print("here1")
	#loess for treated
	low2<-loess(ty~tx, span=sp, seq=seq.sl)
	low2p<-predict(low2, seq.sl, se = TRUE)
	cilt<-low2p$fit - low2p$se.fit*1.96
	ciht<-low2p$fit + low2p$se.fit*1.96
print("here2")

	diff <- ty - cy

	#predictions and CIs for ATT
	low3 <- loess(diff~tx, span=sp)
	low3p <- predict(low3, seq.sl, se=TRUE)
	cild <- low3p$fit - low3p$se.fit * 1.96 
	cihd <- low3p$fit + low3p$se.fit * 1.96 
print("here3")
#store for graph
fit1 <- low1p$fit
fit2 <- low2p$fit
fit3 <- low3p$fit

} else{



#plm for controls
cat("PLM Results for Control Units" , "\n", "Control Covariates = ", controls)
plmc <- plm(data=c, x=x.var, y=y.var, z=controls, seq=seq.sl, span=sp)

#store loess predictions for conrol group
if(imp == 0){
#without imputation
	low1<-loess(plmc$y.plm~plmc$x.plm, span=sp, seq=seq.sl)
	low1p<-predict(low1, seq.sl, se = TRUE)
	cil1<-low1p$fit - low1p$se.fit*1.96
	cih1<-low1p$fit + low1p$se.fit*1.96
} #end if(imp == 0)

if(imp == 1){
	#FOR IMPUTATIONS 
	#replace the control Ys with the PLM Ys
	c['y.var'] <- plmc$y.plm
	
	#LOESS for controls (imputing)
	low1p <- loess.imp(y=y.var, x=x.var, datat=t, datac=c, covs=controls, span=sp, min=min, max=max, step=1, impute=T)#, f.weight="ones.sum.x")#, df.adjust=T, unique="codseg73")#,
	#									 #graph=T, ylab="Poverty Index", xlab="Slope", dir="d:/rplot2")
	
	
	cil1<-low1p$fit - low1p$se.fit*1.96
	cih1<-low1p$fit + low1p$se.fit*1.96
} # end if(imp == 1)
#################

#plm for treated
cat("PLM Results for Treated Units" , "\n", "Control Covariates = ", controls)
plmt <- plm(data=t, x=x.var, y=y.var, z=controls, seq=seq.sl, span=sp)

#loess for treated
low2<-loess(plmt$y.plm~plmt$x.plm, span=sp, seq=seq.sl)
low2p<-predict(low2, seq.sl, se = TRUE)
cilt<-low2p$fit - low2p$se.fit*1.96
ciht<-low2p$fit + low2p$se.fit*1.96


#store differences
if(imp == 0){
	#loess for difference
	#without IMPUTATIONS
	diff <- plmt$y.plm - plmc$y.plm
}

if(imp == 1){
	#with imputation
	diff <- plmt$y.plm - t[y.var]
}


#predictions and CIs for ATT
low3 <- loess(diff~plmt$x.plm, span=sp)
low3p <- predict(low3, seq.sl, se=TRUE)
cild <- low3p$fit - low3p$se.fit * 1.96 
cihd <- low3p$fit + low3p$se.fit * 1.96 

##########################DELETE
print(low3p$se.fit)


#store for graph
fit1 <- low1p$fit
fit2 <- low2p$fit
fit3 <- low3p$fit

out <- list(fit=low3p$fit, plmt.y=plmt$y.plm, plmc.y=plmc$y.plm)
print("herre")
if(return.fit){
return(out)
print("here2")
}
}



cil1 <- cil1
cih1 <- cih1
cil2 <- cilt
cih2 <- ciht
cil3 <- cild
cih3 <- cihd

#use user defined (i.e., from a bootstrap) CIs
if(!is.null(cil.in)){
	if(add.user.ci){ #this will plot the bootstrapped CIs over the default CIs
		cil2 <- cil.in
		cih2 <- cih.in
	} else{
		cil3 <- cil.in
		cih3 <- cih.in
	print("Using user defined confidence bands")
	}
}

print("before ses \n")

#use user defined (i.e., from a bootstrap) SEs 
if(!is.null(se.in)){
	if(add.user.ci){ #this will plot the bootstrapped CIs over the default CIs
		cil2 <- low3p$fit - se.in * 1.96
		cih2 <- low3p$fit + se.in * 1.96
		print("Using user defined standard errors1")
	} else{
		cil3 <- low3p$fit - se.in * 1.96
		cih3 <- low3p$fit + se.in * 1.96
		print("Using user defined standard errors2")
	}
}

print("after ses \n")



#data frame for ggplot
g <- data.frame(cil2, cih2, cil1, cih1, cil3, cih3, fit1, fit2, fit3)
s <- seq.sl
#data for rug
h <- data.frame( cx, cy, tx, ty)
names(h) <- c("cx", "cy", "tx", "ty")
#set plot window size
#dev.new(width=d.width, height=d.height)

if(add.user.ci){
#if the ylim is not defined then use default
if(is.null(y.min) | is.null(y.max)){
	if(plot.all){
	#begin plot
		print("plot1")

	(p <- ggplot(g,aes(x=s)) + #geom_ribbon(aes(ymin=-low3.boot$cil, ymax=-low3.boot$cih), col="#0000ff22", fill="#0000ff22") + #bootstrapped SEs
											 #geom_ribbon(aes(ymin=cil1, ymax=cih1), col="#ff000022", fill="#00ff0022") + #CI for Controls (gREEN)
											 geom_ribbon(aes(ymin=cil2, ymax=cih2), col="#0000ff22", fill="#ff000022") + #CI for Treated (red)
											 geom_ribbon(aes(ymin=cil3, ymax=cih3), col=color, fill=color) + #CI for ATT (blue)
											# geom_line(aes(x=s, y=cil), lty="dashed") + geom_line(aes(x=c, y=cih), lty="dashed") + 
											# geom_line(aes(x=s, y=cil1), lty="dashed") + geom_line(aes(x=c, y=cih1), lty="dashed") +
											# geom_line(aes(x=s, y=cil3), lty="dashed") + geom_line(aes(x=c, y=cih3), lty="dashed") +
											  geom_line(aes(x=s, y=fit1), lty="dotted", lwd=1.1) + geom_line(aes(x=s, y=fit2), lty="dashed", lwd=1.1) + 
											 geom_line(aes(x=s, y=fit3)) + 
											 ylab(y.lab) + xlab(x.lab) +
											 labs(title = title))
	
	if(rug){p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  		theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  		theme(axis.title.x = element_text(size = 14)) + geom_rug(data=h, aes(x=cx, y=cy), sides="b", col="red") + geom_rug(data=h, aes(x=tx, y=ty), sides="b", col="#0000ff55")+ xlim(min,max)
					}else{
	p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  theme(axis.title.x = element_text(size = 14)) 
		  		}
	}else{
	print("plot2")
	(p <- ggplot(g,aes(x=s)) + #geom_ribbon(aes(ymin=-low3.boot$cil, ymax=-low3.boot$cih), col=color, fill=color) + #bootstrapped SEs
											 #geom_ribbon(aes(ymin=cil1, ymax=cih1), col="#ff000022", fill="#00ff0022") + #CI for Controls (gREEN)
											 geom_ribbon(aes(ymin=cil2, ymax=cih2), col=color, fill="#ff000022") + #CI for Treated (red)
											 geom_ribbon(aes(ymin=cil3, ymax=cih3), col=color, fill=color) + #CI for ATT (blue)
											# geom_line(aes(x=s, y=cil), lty="dashed") + geom_line(aes(x=c, y=cih), lty="dashed") + 
											# geom_line(aes(x=s, y=cil1), lty="dashed") + geom_line(aes(x=c, y=cih1), lty="dashed") +
											# geom_line(aes(x=s, y=cil3), lty="dashed") + geom_line(aes(x=c, y=cih3), lty="dashed") +
											# geom_line(aes(x=s, y=fit1), lty="dotted", lwd=1.1) + geom_line(aes(x=s, y=fit2), lty="dashed", lwd=1.1) + 
											 geom_line(aes(x=s, y=fit3)) + 
											 ylab(y.lab) + xlab(x.lab) +
											 labs(title = title))
	
	if(rug){p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  		theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  		theme(axis.title.x = element_text(size = 14)) + geom_rug(data=h, aes(x=cx, y=cy), sides="b", col="red") + geom_rug(data=h, aes(x=tx, y=ty), sides="b", col="#00ff0055")+ xlim(min,max)
					}else{
	p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  theme(axis.title.x = element_text(size = 14)) 
		  		}
	}
}else{ #if the ylim is defined
	if(plot.all){
	print("tis the problem")
	
	#begin plot
	(p <- ggplot(g,aes(x=s)) + #geom_ribbon(aes(ymin=-low3.boot$cil, ymax=-low3.boot$cih), col=color, fill=color) + #bootstrapped SEs
											 #geom_ribbon(aes(ymin=cil1, ymax=cih1), col="#ff000022", fill="#00ff0022") + #CI for Controls (gREEN)
											 geom_ribbon(aes(ymin=cil2, ymax=cih2), col="#ff000022", fill="#ff000022") + #CI for Treated (red)
											 geom_ribbon(aes(ymin=cil3, ymax=cih3), col=color, fill=color) + #CI for ATT (blue)
											# geom_line(aes(x=s, y=cil), lty="dashed") + geom_line(aes(x=c, y=cih), lty="dashed") + 
											# geom_line(aes(x=s, y=cil1), lty="dashed") + geom_line(aes(x=c, y=cih1), lty="dashed") +
											# geom_line(aes(x=s, y=cil3), lty="dashed") + geom_line(aes(x=c, y=cih3), lty="dashed") +
											#  geom_line(aes(x=s, y=fit1), lty="dotted", lwd=1.1) + geom_line(aes(x=s, y=fit2), lty="dashed", lwd=1.1) + 
											 geom_line(aes(x=s, y=fit3)) + 
											 ylab(y.lab) + xlab(x.lab) +
											 labs(title = title) + scale_y_continuous(limits=c(y.min, y.max), breaks = seq(y.min, y.max, int)))
	
	if(rug){p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  		theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  		theme(axis.title.x = element_text(size = 14)) + geom_rug(data=h, aes(x=cx, y=cy), sides="b", col="red") + geom_rug(data=h, aes(x=tx, y=ty), sides="b", col="#00ff0055")+ xlim(min,max)
					}else{
	p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  theme(axis.title.x = element_text(size = 14)) 
		  		}
	}else{
		print("tis the problem2")

	(p <- ggplot(g,aes(x=s)) + #geom_ribbon(aes(ymin=-low3.boot$cil, ymax=-low3.boot$cih), col=color, fill=color) + #bootstrapped SEs
											 #geom_ribbon(aes(ymin=cil1, ymax=cih1), col="#ff000022", fill="#00ff0022") + #CI for Controls (gREEN)
											 geom_ribbon(aes(ymin=cil2, ymax=cih2), col="#ff000022", fill="#ff000022") + #CI for Treated (red)
											 geom_ribbon(aes(ymin=cil3, ymax=cih3), col=color, fill=color) + #CI for ATT (blue)
											# geom_line(aes(x=s, y=cil), lty="dashed") + geom_line(aes(x=c, y=cih), lty="dashed") + 
											# geom_line(aes(x=s, y=cil1), lty="dashed") + geom_line(aes(x=c, y=cih1), lty="dashed") +
											# geom_line(aes(x=s, y=cil3), lty="dashed") + geom_line(aes(x=c, y=cih3), lty="dashed") +
											# geom_line(aes(x=s, y=fit1), lty="dotted", lwd=1.1) + geom_line(aes(x=s, y=fit2), lty="dashed", lwd=1.1) + 
											 geom_line(aes(x=s, y=fit3)) + 
											 ylab(y.lab) + xlab(x.lab) +
											 labs(title = title)  + scale_y_continuous(limits=c(y.min, y.max), breaks = seq(y.min, y.max, int)))
	
	if(rug){p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  		theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  		theme(axis.title.x = element_text(size = 14)) + geom_rug(data=h, aes(x=cx, y=cy), sides="b", col="red") + geom_rug(data=h, aes(x=tx, y=ty), sides="b", col="#00ff0055")+ xlim(min,max)
					}else{
	p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  theme(axis.title.x = element_text(size = 14)) 
		  		}
	}
}

################################################################################################################################
###################      ABOVE PLOT CODE USED WHEN OVERLAYING BOOTSTAPPED CIs
################################################################################################################################

}else{ #add.user.cih3
#if the ylim is not defined then use default
if(is.null(y.min) | is.null(y.max)){
	if(plot.all){
		print("plot3")

	#begin plot
	(p <- ggplot(g,aes(x=s)) + #geom_ribbon(aes(ymin=-low3.boot$cil, ymax=-low3.boot$cih), col="#0000ff22", fill="#0000ff22") + #bootstrapped SEs
											 #geom_ribbon(aes(ymin=cil1, ymax=cih1), col="#ff000022", fill="#00ff0022") + #CI for Controls (gREEN)
											 #geom_ribbon(aes(ymin=cil2, ymax=cih2), col="#0000ff22", fill="#ff000022") + #CI for Treated (red)
											 geom_ribbon(aes(ymin=cil3, ymax=cih3), col=color, fill=color) + #CI for ATT (blue)
											# geom_line(aes(x=s, y=cil), lty="dashed") + geom_line(aes(x=c, y=cih), lty="dashed") + 
											# geom_line(aes(x=s, y=cil1), lty="dashed") + geom_line(aes(x=c, y=cih1), lty="dashed") +
											# geom_line(aes(x=s, y=cil3), lty="dashed") + geom_line(aes(x=c, y=cih3), lty="dashed") +
											  geom_line(aes(x=s, y=fit1), lty="dotted", lwd=1.1) + geom_line(aes(x=s, y=fit2), lty="dashed", lwd=1.1) + 
											 geom_line(aes(x=s, y=fit3)) + 
											 ylab(y.lab) + xlab(x.lab) +
											 labs(title = title))
	
	if(rug){p <- p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  		theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  		theme(axis.title.x = element_text(size = 14)) + geom_rug(data=h, aes(x=cx, y=cy), sides="b", col="red") + geom_rug(data=h, aes(x=tx, y=ty), sides="b", col="#0000ff55")+ xlim(min,max)
					}else{
	p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  theme(axis.title.x = element_text(size = 14)) 
					}
	  print(p)
	}else{
		print("plot4")

	(p <- ggplot(g,aes(x=s)) + #geom_ribbon(aes(ymin=-low3.boot$cil, ymax=-low3.boot$cih), col=color, fill=color) + #bootstrapped SEs
											 #geom_ribbon(aes(ymin=cil1, ymax=cih1), col="#ff000022", fill="#00ff0022") + #CI for Controls (gREEN)
											 #geom_ribbon(aes(ymin=cil2, ymax=cih2), col=color, fill="#ff000022") + #CI for Treated (red)
											 geom_ribbon(aes(ymin=cil3, ymax=cih3), col=color, fill=color) + #CI for ATT (blue)
											# geom_line(aes(x=s, y=cil), lty="dashed") + geom_line(aes(x=c, y=cih), lty="dashed") + 
											# geom_line(aes(x=s, y=cil1), lty="dashed") + geom_line(aes(x=c, y=cih1), lty="dashed") +
											# geom_line(aes(x=s, y=cil3), lty="dashed") + geom_line(aes(x=c, y=cih3), lty="dashed") +
											# geom_line(aes(x=s, y=fit1), lty="dotted", lwd=1.1) + geom_line(aes(x=s, y=fit2), lty="dashed", lwd=1.1) + 
											 geom_line(aes(x=s, y=fit3)) + 
											 ylab(y.lab) + xlab(x.lab) +
											 labs(title = title))
	
	if(rug){p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  		theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  		theme(axis.title.x = element_text(size = 14)) + geom_rug(data=h, aes(x=cx, y=cy), sides="b", col="red") + geom_rug(data=h, aes(x=tx, y=ty), sides="b", col="#00ff0055")+ xlim(min,max)
					}else{
	p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  theme(axis.title.x = element_text(size = 14)) 
					}
	}
}else{ #if the ylim is defined
	if(plot.all){
		print("plot5")

	#begin plot
	(p <- ggplot(g,aes(x=s)) + #geom_ribbon(aes(ymin=-low3.boot$cil, ymax=-low3.boot$cih), col=color, fill=color) + #bootstrapped SEs
											 #geom_ribbon(aes(ymin=cil1, ymax=cih1), col="#ff000022", fill="#00ff0022") + #CI for Controls (gREEN)
											 #geom_ribbon(aes(ymin=cil2, ymax=cih2), col=color, fill="#ff000022") + #CI for Treated (red)
											 geom_ribbon(aes(ymin=cil3, ymax=cih3), col=color, fill=color) + #CI for ATT (blue)
											# geom_line(aes(x=s, y=cil), lty="dashed") + geom_line(aes(x=c, y=cih), lty="dashed") + 
											# geom_line(aes(x=s, y=cil1), lty="dashed") + geom_line(aes(x=c, y=cih1), lty="dashed") +
											# geom_line(aes(x=s, y=cil3), lty="dashed") + geom_line(aes(x=c, y=cih3), lty="dashed") +
											  geom_line(aes(x=s, y=fit1), lty="dotted", lwd=1.1) + geom_line(aes(x=s, y=fit2), lty="dashed", lwd=1.1) + 
											 geom_line(aes(x=s, y=fit3)) + 
											 ylab(y.lab) + xlab(x.lab) +
											 labs(title = title) + scale_y_continuous(limits=c(y.min, y.max), breaks = seq(y.min, y.max, int)))
	
	if(rug){p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  		theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  		theme(axis.title.x = element_text(size = 14)) + geom_rug(data=h, aes(x=cx, y=cy), sides="b", col="red") + geom_rug(data=h, aes(x=tx, y=ty), sides="b", col="#00ff0055")+ xlim(min,max)
					}else{
	p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  theme(axis.title.x = element_text(size = 14)) 
		  		}
	}else{
		print("plot6")

	(p <- ggplot(g,aes(x=s)) + #geom_ribbon(aes(ymin=-low3.boot$cil, ymax=-low3.boot$cih), col=color, fill=color) + #bootstrapped SEs
											 #geom_ribbon(aes(ymin=cil1, ymax=cih1), col="#ff000022", fill="#00ff0022") + #CI for Controls (gREEN)
											 #geom_ribbon(aes(ymin=cil2, ymax=cih2), col=color, fill="#ff000022") + #CI for Treated (red)
											 geom_ribbon(aes(ymin=cil3, ymax=cih3), col=color, fill=color) + #CI for ATT (blue)
											# geom_line(aes(x=s, y=cil), lty="dashed") + geom_line(aes(x=c, y=cih), lty="dashed") + 
											# geom_line(aes(x=s, y=cil1), lty="dashed") + geom_line(aes(x=c, y=cih1), lty="dashed") +
											# geom_line(aes(x=s, y=cil3), lty="dashed") + geom_line(aes(x=c, y=cih3), lty="dashed") +
											# geom_line(aes(x=s, y=fit1), lty="dotted", lwd=1.1) + geom_line(aes(x=s, y=fit2), lty="dashed", lwd=1.1) + 
											 geom_line(aes(x=s, y=fit3)) + 
											 ylab(y.lab) + xlab(x.lab) +
											 labs(title = title)  + scale_y_continuous(limits=c(y.min, y.max), breaks = seq(y.min, y.max, int)))
	
	if(rug){p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  		theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  		theme(axis.title.x = element_text(size = 14)) + geom_rug(data=h, aes(x=cx, y=cy), sides="b", col="red") + geom_rug(data=h, aes(x=tx, y=ty), sides="b", col="#00ff0055")+ xlim(min,max)
					}else{
	p + theme(axis.text.y = element_text(size = 14, hjust = 1, lineheight = 0.9)) + theme(axis.text.x = element_text(size = 14, vjust = 1, lineheight = 0.9)) +
		  theme(plot.title = element_text(size = 20)) + theme(axis.title.y = element_text(size = 14, vjust=.5, angle=90)) + 
		  theme(axis.title.x = element_text(size = 14)) 
		  		}
	}
}
}
}

