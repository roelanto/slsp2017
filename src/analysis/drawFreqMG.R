library(MASS)
library(knitr)
library(mixtools)

#' Draws a histogram of pause frequencies. 
#' 
drawFreqMG <- function(allpauses, topleft=FALSE, bottomleft=FALSE, bottom=FALSE, k=2, plotAddition=TRUE, xlim=c(0.1,2.5), ymax=3.5, main="",  ... ) {
  if (topleft || bottomleft) {
    leftmar <- 2
  } else {
    leftmar <- 2
  }
  if (bottomleft || bottom) {
    botmar <- 3
  } else {
    botmar <- 3
  }
  par(mar = c(botmar, leftmar, 1, 0.3) + 0.1, tck = -0.02, cex.axis = 1.1, cex = 1.4)
  #browser()
  mixmdl <- normalmixEM(log(c(0.05,20,allpauses)*1000, base=10), ECM=TRUE, k=k)
  # plot(mixmdl$posterior)
  # histogram mid point values
  breaks <- seq(from=1.6, to=5, by = .2)
  ats <- hist(mixmdl$x, plot = FALSE, breaks=breaks)$mids
  
  
  cat(hist(mixmdl$x, plot = FALSE, breaks=breaks)$mids)
  message("min: ", min(ats), " max: ", max(ats))
  labels <- floor(10^hist(mixmdl$x, plot = FALSE)$mids)
  loglabels <- parse(text=paste("10^", seq(from=1, to=5, by = 1)))
  # loglabels <- hist(mixmdl$x, plot = FALSE, breaks=breaks)$mids
  #loglabels <- seq(from=1, to=5, by = 1)
  actuallabels <- floor(10^hist(mixmdl$x, plot = FALSE, breaks=breaks)$mids)
  myseq <- seq(from=1, to=5, length.out=1e3)
  # use myplotmixEM to draw result of two gaussians
  resplot <- myplotmixEM(mixmdl, which=2, ann=FALSE, border=col.alpha("black", 1), col2=c(col.alpha("blue", 1), col.alpha("blue", 1)), lwd2=c(3,3), ylab1="y1", ylab2="y2", axes=FALSE, breaks=breaks, maxy=1.5)
  #curve( 
  #  (mixmdl$lambda[1] * dnorm(x, mean=mixmdl$mu[1], sd=mixmdl$sigma[1])) + 
  #   (mixmdl$lambda[2] * dnorm(x, mean=mixmdl$mu[2], sd=mixmdl$sigma[2])), add = TRUE)
  #+ (0.576 * rnorm(n = 1000, mean=2.5, sd=0.352)), add = TRUE)
  
  # dens((0.424 * rnorm(n = 1000, mean=1.96, sd=0.136)) + (0.576 * rnorm(n = 1000, mean=2.5, sd=0.352)), add = TRUE)
  
  #  ats <- seq(from=log(1000*min(allpauses), base=10), to=log(1000*max(allpauses), base=10), length.out = 3)
  #  labels <- seq(from=min(allpauses), to=max(allpauses), length.out = 3)
  #axis(side = 3, at = ats, labels = actuallabels, cex=0.6)
  title(main=main)
  #  lines(density(log(allpauses*1000, base=10)), lty=4, lwd=2)
  # axis(side=1)
  axis(side = 1, at = seq(from=1, to=5, by = 1), labels = loglabels)
  axis(side = 2, at = seq(from=0, to=1.5, length.out=4), labels = c(NA, NA, NA, NA))
  
  if (bottomleft) {
    #mtext(side=1, text=expression(paste('Pause length in log'[10],'(ms)')),  line = 3)
    mtext(side=1, text=expression(paste('Pause length (ms)')), cex=1.4,   line = 2)
    mtext(side=2, text="Density", line=0.5, cex=1.4)
    #mtext(side=3, text="Pause length (miliseconds)", line=2)
    #axis(side = 3, at = ats, labels = actuallabels, cex=0.6)
  }
  print(mixmdl$ft)
  print(mixmdl$loglik)
  if (plotAddition) {
  myseq <- seq(from=1, to=5, by=0.001)
  
  yval <- (mixmdl$lambda[1] * dnorm(x=myseq, mean=mixmdl$mu[1], sd = mixmdl$sigma[1])) 
  for (i in c(2:k)) {
    yval <- yval + (mixmdl$lambda[i] * dnorm(x=myseq, mean=mixmdl$mu[i], sd = mixmdl$sigma[i]))
  } 
  points(x=myseq, y=yval, col="green", type="l", lwd=3, lty=5)
  }
  sd1 <- sd(10^rnorm(n=1e4, mean=mixmdl$mu[1], sd=mixmdl$sigma[1]))
  sd2 <- sd(10^rnorm(n=1e4, mean=mixmdl$mu[2], sd=mixmdl$sigma[2]))
  message("sd1: ", sd1, " sd2: ", sd2)
  return (list(mu=mixmdl$mu, sigma=c(sd1,sd2), lambda=mixmdl$lambda))
  #xlim <- par("usr")[1:2]
  #log10AtX <- seq(ceiling(xlim[1]), floor(xlim[2]))
  #  
  #axis(side=1, at=10^log10AtX, lab=as.expression(lapply(log10AtX, function(y)bquote(10^.(y)))))
}



# the following two methods were created to doublecheck the correct computation of the lot. It is obsolete, in the sense that 
# the alternative computation methods (repnormmixEM) didn't yield any significant results. 
# createRes2 creates a matrix structure of the resdf (mxn where m = subjects and n = measurements)
# this is used for tryagain to plot the data and mixmdl and repmixmdl
# NB: tryagain expects allpauses to be log-transformed already (log(x * 1000, base=10))
createRes2 <- function(resdf, filenames, n=160) {
  res2 <- sapply(filenames, function(x) {
      log(
        
        
      1000*unlist(
        resdf[resdf$filename==x,]$pauses)[sample(c(1:length(unlist(resdf[resdf$filename==x,]$pauses))), size=n)]
      , base=10)})
  res2
}

tryagain <- function(allpauses,res2,  k=2) {
  mixmdl <- normalmixEM(allpauses,  ECM = TRUE, k=k)
  plot.mixEM(mixmdl, whichplots = 2)
  mixmdlrep <- repnormmixEM(res2,  k=3)
  cat(mixmdl$lambda, "\n")
  cat(mixmdl$mu, "\n")
  cat(mixmdl$sigma, "\n")
  cat(mixmdlrep$lambda, "\n")
  cat(mixmdlrep$mu, "\n")
  cat(mixmdlrep$sigma, "\n")
  
  myseq <- seq(from=1, to=4, by=0.01)
  yval <- (mixmdlrep$lambda[1] * dnorm(x=myseq, mean=mixmdlrep$mu[1], sd = mixmdlrep$sigma[1])) 
  for (i in c(2:k)) {
    yval <- yval + (mixmdlrep$lambda[i] * dnorm(x=myseq, mean=mixmdlrep$mu[i], sd = mixmdlrep$sigma[i]))
  } 
  points(x=myseq, y=yval, col="purple", type="l", lwd=3)
  
  plot(NULL, xlim=c(0,4), ylim=c(0,3))
  for (i in c(1:24)) {
    histdata <- hist(res2[,i], plot=FALSE)
    points(histdata$mids, histdata$density,type="l")
  }
  yval <- (mixmdlrep$lambda[1] * dnorm(x=myseq, mean=mixmdlrep$mu[1], sd = mixmdlrep$sigma[1])) 
  points(x=myseq, y=yval, col="purple", type="l", lwd=3)
  yval <- (mixmdl$lambda[1] * dnorm(x=myseq, mean=mixmdl$mu[1], sd = mixmdl$sigma[1])) 
  points(x=myseq, y=yval, col="red", type="l", lwd=3)
  yval <- (mixmdlrep$lambda[2] * dnorm(x=myseq, mean=mixmdlrep$mu[2], sd = mixmdlrep$sigma[2])) 
  points(x=myseq, y=yval, col="purple", type="l", lwd=3)
  yval <- (mixmdlrep$lambda[3] * dnorm(x=myseq, mean=mixmdlrep$mu[3], sd = mixmdlrep$sigma[3])) 
  points(x=myseq, y=yval, col="purple", type="l", lwd=3)
  yval <- (mixmdl$lambda[2] * dnorm(x=myseq, mean=mixmdl$mu[2], sd = mixmdl$sigma[2])) 
  points(x=myseq, y=yval, col="red", type="l", lwd=3)
  yval <- (mixmdlrep$lambda[1] * dnorm(x=myseq, mean=mixmdlrep$mu[1], sd = mixmdlrep$sigma[1])) 
  for (i in c(2:k)) {
    yval <- yval + (mixmdlrep$lambda[i] * dnorm(x=myseq, mean=mixmdlrep$mu[i], sd = mixmdlrep$sigma[i]))
  } 
  points(x=myseq, y=yval, col="green", type="l", lwd=3)
  
  
  
  # run with: tryagain(allpauses = allpauses, res2=createRes2(resdf, sourcefiles.c), k=2)

}
