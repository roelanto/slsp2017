computeLTSE<- function(object, framelengthInSeconds=0.01, nfft=128, debugoutput=FALSE) {
  bands <- seq(from=1, to=max(freqrange), length.out = nfft+1)
  totalNumFrames <- (length(object)/object@samp.rate) / framelengthInSeconds
  ltsematrix <- matrix(nrow=totalNumFrames, ncol = (length(bands)-1))
  m <- sapply(c(1:(totalNumFrames-1)), function(i, object, framelengthInSeconds, windowlengthInFrames, bands, freqrange, totalNumFrames, filename) {
    determineLTSEs(object = object, frameno=i, framelengthInSeconds=framelengthInSeconds, windowlengthInFrames = windowlengthInFrames, bands=bands, freqrange=freqrange, debugoutput = debugoutput) 
  }, object=object, windowlengthInFrames=windowlengthInFrames, framelengthInSeconds=framelengthInSeconds, bands=bands, freqrange=freqrange, totalNumFrames=totalNumFrames,  filename=filename)
  ltsematrix <- t(m)
  ltsematrix
}