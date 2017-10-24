# framewidth: in samples, the width of each frame (used for determining ltsd/decision plots)
# pal: 1,2: color of specs/altspecs, 3: color of horizontal line, 4: color of ltsd trace, 5,6: dots that represent yes/no pause dcisions
plotWithVAD <- function(object, 
                        ltsdvals, 
                        decision, fullxlim=TRUE, start, end, colorize=NULL, drawdraft=FALSE, draftskip=10, sub="", 
                        framewidth=0.01, speccolor=FALSE, ltsd.lwd=2, ltsd.col=FALSE, decisionCutoff=NULL, 
                        ltsdwindowwidth = 20, separatePlots=FALSE, ylim=c(-1,1), 
                        pal=c(col.alpha(acol="blue", alpha=0.4), col.alpha(acol="red", alpha=0.4), "red", col.alpha("black", 0.8), "red", "green"), top=FALSE, ...) {
  
  if (speccolor == FALSE) {
    speccolor <- col.alpha(acol="blue", alpha=0.4)
    altspeccolor <- col.alpha(acol="red", alpha=0.4)
  }
  if (ltsd.col == FALSE) {
    ltsd.col <- pal[4]
  }
  # scale only on the left channel
  if (start == -1) {
    start <- 0
    end <- length(object)/object@samp.rate
  }
  if (fullxlim==FALSE) {
    xlim<-c(start*object@samp.rate, end*object@samp.rate)
  } else {
    xlim<-c(0, length(object@left))
  }
  plot(NULL, xlim=xlim,ylim=ylim, xlab=ifelse(top==FALSE, c("time (s)"), ""), ylab="Relative amplitude", axes=FALSE, ...)
  # scale values to whatever is highest, the max or min values. maxSpecValue is the maximum value; to scale spec values, divide by maxSpecValue. 
  maxSpecValue <- max(abs(PI(object@left, prob=0.99999)))
  maxLTSDValue <- max(abs(PI(ltsdvals, prob=0.99999)))
  specs <- object@left / maxSpecValue #max(abs(object@left))
  # decide which points to plot. Because there are so many, it is very useful to include start- and end-values.
  samplesOfInterest <- seq(from=1+(start*object@samp.rate), to=end*object@samp.rate, by=1)
  cexval <- 0.1
  if (drawdraft == TRUE) {
    samplesOfInterest <- samplesOfInterest[seq(from=samplesOfInterest[1], to=samplesOfInterest[length(samplesOfInterest)], by=draftskip)]
    cexval=0.5
  }
  if(!is.null(colorize)) {
    lines(x=samplesOfInterest, y=specs[samplesOfInterest], cex=cexval, ylim=c(-1,1), col=ifelse((samplesOfInterest > min(colorize) & samplesOfInterest < max(colorize)), pal[2], pal[1]))
  } else {
    lines(x=samplesOfInterest, y=specs[samplesOfInterest], cex=cexval, ylim=c(-1,1), col=pal[1])
  }
  abline(h = 0, col=pal[3], lwd=0.2)
  
  if (length(ltsdvals) > 1) {
    ltsdvals_in_range <- sapply(c(1:length(ltsdvals)), function(frame) {
      range <- framerange(frameno=frame, width=0.01) * object@samp.rate
      #message("Frame ", frame, "range: ", min(range), "-", max(range), "start/end: ", start*object@samp.rate, "-", end*object@samp.rate, appendLF = FALSE)
      
      if (min(range) >= start*object@samp.rate && max(range) <= end*object@samp.rate) {
        # message(" TRUE")
        return(TRUE)
      }
      #essage(" FALSE")
      return(FALSE)
    })
    outputlength <- length(ltsdvals) # convert to a scale of seconds
    ltsdwindowwidth <- length(object) / length(ltsdvals)  
    ltsdvals.c <- ltsdvals
    ltsdvals.c <- (ltsdvals - mean(ltsdvals)) / maxLTSDValue
    lines(x=(which(ltsdvals_in_range == TRUE) -1 )* ltsdwindowwidth, y=ltsdvals.c[which(ltsdvals_in_range == TRUE)], col=pal[4], lwd=ltsd.lwd, ...)
    #  points(x=which(ltsdvals_in_range == TRUE) * framewidth * object@samp.rate, y=offset*ltsdvals[which(ltsdvals_in_range == TRUE)], col=col.alpha("blue", alpha=0.3), cex=0.1, ...)
  }
  
  if (length(decision) > 0) {
    xcoords <- sapply(which(ltsdvals_in_range == TRUE), function(x, framewidth, samp.rate) {
      x * framewidth * samp.rate
    }, framewidth=framewidth, samp.rate=object@samp.rate)
    ycoords <- sapply(which(ltsdvals_in_range == TRUE), function(x, framewidth, samp.rate) {
      ifelse(decision[x] == TRUE, 1, -1)
    })
    # lines(x=xcoords, y=ycoords, col=col.alpha(alpha=0.5, acol="green"), cex=0.05)
    
    decisionwindowwidth <- length(object) / length(decision)  
    # next lines of codes use lines instead of points. 
    #sapply(c(1:length(decision)), function(x) {
    #  lines(x=rep(x = x*decisionwindowwidth, 2), y=c(0.7, ifelse(decision[x] == TRUE, 0.6, c(0.8))), col=ifelse(decision[x] == TRUE, "red", "green"))
    #})
    points(x=seq(from=1, to=length(decision), by=1) * decisionwindowwidth, y=ifelse(decision == TRUE, max(ylim), max(ylim)-0.03), col=ifelse(decision == TRUE, pal[5], pal[6]), cex=0.3, pch=15)
    
    # points(x=(which(ltsdvals_in_range == TRUE) / ltsdwindowwidth)  * object@samp.rate, y=rep(1.0, length(which(ltsdvals_in_range == TRUE) )), col=ifelse(decision == TRUE, col.alpha(acol="green", alpha=1.0), col.alpha("red", alpha=1.0)), cex=0.6, pch=15)
    
    #points(x=xcoords, y=rep(-1.0, length(which(ltsdvals_in_range == TRUE) )), col=ifelse(decision == TRUE, col.alpha(acol="green", alpha=1.0), col.alpha("red", alpha=1.0)), cex=0.4, pch=15)
  }
  if (!is.null(decisionCutoff)) {
    #decisionCutoff <- (decisionCutoff - mean(ltsdvals)) / max(abs(PI(ltsdvals, prob=0.9999)))
    message("Abline at ", decisionCutoff, " - ", mean(ltsdvals), " / ", maxLTSDValue, " = ", (decisionCutoff -mean(ltsdvals)) /maxLTSDValue)
    # abline(h = (decisionCutoff -mean(ltsdvals)) /maxLTSDValue, lty=3)
    abline(h = decisionCutoff, lty=3)
  } else {
    message("No decision cutoff")
  }
  positions <- c(seq(from=0, to=length(object)/object@samp.rate, by=1))
  if (top==FALSE) {
  labels= positions
  } else {
    labels=rep(NA, 11)
  }
  #if ((length(object)/object@samp.rate) - labels[length(labels)] > 0.1) {
  #   labels <- append(labels, round(length(object)/object@samp.rate, 1))
  # }
  box(which="plot")
  message("PLOT is ", ifelse(top==FALSE, labels, rep(NA, length(labels))))
  axis(side=1, labels=labels,  tick=TRUE, at=positions*object@samp.rate)
  axis(side=2, labels=ylim, tick=TRUE, at=ylim)
  title(main=NULL, sub=sub,
        ylab="", xlab=ifelse(top==FALSE, c("time (s)"), "")) 
  
  
  main=paste0("")
  # col.alpha("", alpha=0.3), cex=0.1, ...)
  #lines(x=seq(from=0, to=outputlength, length.out=length(decision)), y=offset*decision, col="green", cex=0.01, ...)
}
