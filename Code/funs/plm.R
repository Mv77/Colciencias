##Program to estimate partial linear model using loess at last stage to estimate nonparametric portion

plm <- function(data, x="String nonpar var", y="string for outcome", z="string for controls", order=10,
                loess=T, weights, span = 0.75, seq=NULL, se= T, predict= T, degree = 2, family = c("gaussian", "symmetric")){
  #order the data according to the independent variable of interest
  sdata <- data[order(data[x]),]
  
  #store X, Y and Z variables in matrix for use in OLS
  Xs <- as.matrix(sdata[x])
  
  Ys <- as.matrix(sdata[y])
  
  Zs <- as.matrix(sdata[z])
  
  #Define the potential weighting vectors (See Yatchew 1997)
  #Note that these are normalized weights so that, for instance, W1 is the same as taking simple 1st dif.	
  W1  <- c(0.7071, -0.7071)
  W2  <- c(0.8090, -0.5000, -0.3090)
  W3  <- c(0.1942,  0.2809,  0.3832, -0.8582)
  W4  <- c(0.2708, -0.0142,  0.6909, -0.4858, -0.4617)
  W5  <- c(0.9064, -0.2600, -0.2167, -0.1774, -0.1420, -0.1103)
  W6  <- c(0.2400,  0.0300, -0.0342,  0.7738, -0.3587, -0.3038, -0.3472)
  W7  <- c(0.9302, -0.1965, -0.1728, -0.1506, -0.1299, -0.1107, -0.0930, -0.0768)
  W8  <- c(0.2171,  0.0467, -0.0046, -0.0348,  0.8207, -0.2860, -0.2453, -0.2260, -0.2879)
  W9  <- c(0.9443, -0.1578, -0.1429, -0.1287, -0.1152, -0.1025, -0.0905, -0.0792, -0.0687, -0.0588)
  W10 <- c(0.9494, -0.1437, -0.1314, -0.1197, -0.1085, -0.0978, -0.0877, -0.0782, -0.0691, -0.0606, -0.0527)
  
  #Round order in case of non-integer
  order <- round(order, 0)
  
  if(order < 1 | order > 10) {
    return("The order of the weighting matrix must lie between 1 and 10 (inclusive)")
  }#end if
  
  #Identify the weighting vector according to the value of order
  W <- get(paste("W", order, sep=""))
  
  ##Take difference for Y and Z according to the order of X
  #counter
  j = 1
  #define variables for loop
  Y.delta <- vector()
  Z.delta <- matrix(0, (nrow(Zs) - order), ncol(Zs))
  
  #This builds the matrix of first difference observations
  for(i in (order + 1):nrow(Xs)){
    Y.delta[j] <- Ys[i] * W[1]
    for(c in 1:ncol(Zs)){
      Z.delta[j,c] <- Zs[i,c] * W[1]
    } #end for	
    j=j+1
  } #end for
  
  #Now calculate the remainder of the differencing
  for(o in 1:order){
    j = 1
    #Loop over the rows
    for(i in (order + 1):nrow(Xs)){
      Y.delta[j] <- Y.delta[j] + Ys[i - o] * W[o + 1]
      #Loop over the columns
      for(c in 1:ncol(Zs)){
        Z.delta[j,c] <- Z.delta[j,c] + Zs[i - o,c] * W[o + 1]
      } #end for columns
      j=j+1
    }	 #end for rows
  } #end for order
  #regression without intercept	
  reg <- lm(Y.delta ~  0 + Z.delta)
  print(summary(reg))
  
  #Get the coefficients from the differnce regression
  b.dif <- reg$coefficients
  
  #This estimates y.plm = Ys - Z*Bdiff which can then be plotted against X to determin f(X) (NOTE THAT IT IS DEMEANED A LA THE STATA VERSION)
  y.plm <- Ys - (Zs %*% b.dif - mean(Zs %*% b.dif))
  
  
  #######################################################################
  #Graphing section
  
  if(loess){
    if(is.null(seq)){
      seq <- seq(min(Xs), max(Xs), length.out=30)
    }#end if seq 
    #Use loess to estimate the nonparametric function
    low <- loess(y.plm~Xs, span=span, degree=degree, family=family)
    lowp <- predict(low, seq, se=F)
    #plot(seq, lowp, type="l", ylab=y, xlab=x)
    if(se){
      #Predict sequence of loess smoothed points along X range with standard errors
      lowp <- predict(low, seq, se=T)
      #lines(seq, (lowp$fit + 1.96*lowp$se), lty=2)
      #lines(seq, (lowp$fit - 1.96*lowp$se), lty=2)			
    }#end if se	
    plm.out <- list(y.plm=y.plm, x.plm=Xs, loess.fit=low$fit, pred.fit=lowp$fit, se=lowp$se)
  }#end if loess		
  
  if(loess==FALSE){
    plm.out <- list(y.plm=y.plm, x.plm=Xs)
  }
  
  #######################################################################
  
  #Return output values
  
  return(plm.out)
}#end function
