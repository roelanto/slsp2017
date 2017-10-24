library(gtools)
library(rethinking)

#' Draws a density histogram. 
#' 
#' @param data ?
#' @param normdata ?
#' @param main ?
#' @param showpeak if TRUE, draw dashed line at peak of density curve
#' @param first if TRUE, will add label to y-axis. Intended to be used in a sequence of plots where only a y-axis annotation on the first plot is required. 
#' @return The sum of \code{x} and \code{y}.
drawdens <- function(data, normdata=NULL, xlim=c(0.1,0.7), col="#0000FFFF", lwd=2, first=FALSE, showpeak=FALSE, main) {
  set_nice_margins()
  if (showpeak) {
     peak_x <- density(data, adjust=0.5)[["x"]][which(density(data, adjust = 0.5)[["y"]] == max(density(data, adjust=0.5)[["y"]]))]
  }
  peak_x <- mean(normdata)
  peak_y <- max(density(data, adjust=0.5)[["y"]])
  ylab <- NULL
  if (first == TRUE) {
    ylab <- "Density"
  } else {
    ylab <- ""
  }
  if (!is.null(normdata)) {
    main <- paste(main, stars.pval(wilcox.test(data, normdata, exact = FALSE)$p.value))
  }
  par(mar = c(2, 0.5, 4, 0.5) + 0.1, tck = -0.02)
  dens(data, adj=0.5, xlim=xlim, col=col, lwd=lwd, norm.comp=TRUE, ylab=ylab, show.HPDI=FALSE, main=main, axes=FALSE)
  abline(v=peak_x, lty=2)
  axis(side = 1, labels = TRUE)
}

#' Creates a plot for the pause densities 
#' @param colname
#' @param resdf
#' 
#' The resdf argument contains the main data passed into this function. It is a dataframe with at least columns `filename`, each of which 
#' also occurs in one of the lists sourcefiles.[c,dad,pd,ppa.nfa, etc]. 
#' 
plotPauseDensity <- function(x, colname, ylim=c(0,30), xlim=c(0.1,0.7), ...) {
    resdf <- x
    normdata <- resdf[startsWith(as.character(resdf$filename), "c_"),colname]
    whichData <- startsWith(as.character(resdf$filename), "c_")
    drawdens(data=resdf[whichData,colname],  xlim=xlim, normdata=normdata, first=TRUE, main=paste0("Controls, N=", sum(whichData)))
    whichData <- startsWith(as.character(resdf$filename), "ad_")
    drawdens(data=resdf[whichData,colname], xlim=xlim,   normdata=normdata,  main=paste0("AD, N=", sum(whichData)))
    whichData <- startsWith(as.character(resdf$filename), "pd_")
##    drawdens(data=resdf[whichData,colname],  xlim=xlim,  normdata=normdata, main=paste0("Parkinson, N=", sum(whichData)))
    whichData <- startsWith(as.character(resdf$filename), "ppa1_")
##    drawdens(data=resdf[whichData,colname],   xlim=xlim, normdata=normdata, main=paste0("PNFA #1, N=", sum(whichData)))
    whichData <- startsWith(as.character(resdf$filename), "ppa2_")
##    drawdens(data=resdf[whichData,colname],   xlim=xlim, normdata=normdata, main=paste0("PNFA #2, N=", sum(whichData)))
    whichData <- startsWith(as.character(resdf$filename), "sd_")
    drawdens(data=resdf[whichData,colname],  xlim=xlim,  normdata=normdata, main=paste0("SD, N=", sum(whichData)))
}
