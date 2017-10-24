computeAVGS <- function(object, freqrange=c(0,3), scanwidth=0.5, nfft=128, from=0, to=30) {
  bands <- seq(from=1, to=max(freqrange), length.out = nfft+1)
  sequence <- seq(from=from, to=min(to, (length(object)/object@samp.rate)-scanwidth), by=scanwidth)
  avgs <- sapply(sequence, function (i, bands, freqrange, scanwidth, object) {
    determineAverages(object, bands=bands, freqrange=freqrange, start=i, end=i+scanwidth)
  }, bands=bands, freqrange=freqrange, scanwidth=scanwidth, object=object)
  scores <- apply(avgs, 2, computeQuality)
  avgs1 <- apply(avgs[,order(scores)[c(1:5)]], 1, mean)
  avgs1
}