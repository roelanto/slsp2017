computeLTSD<- function(object, nfft=128, avgs, ltsematrix) {
  bands <- seq(from=1, to=max(freqrange), length.out = nfft+1)
  
  totalNumFrames <- (length(object)/object@samp.rate) / framelengthInSeconds
  ltsdvals <- t(sapply(c(1:totalNumFrames-1), function(i) {
    ltsd(object, nfft=nfft, bands=bands, avgs = avgs, ltses <- ltsematrix[i,], freqrange = freqrange, framewindowsize = framewindowsize, frameno=i, debugoutput = FALSE)
  }))
  ltsdvals <- ltsdvals[!is.na(ltsdvals)]
  ltsdvals
}