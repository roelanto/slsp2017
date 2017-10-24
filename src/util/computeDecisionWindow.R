
computeDecisionWindow <- function(ltsdvals, window=2, factor=0.4) {
  decision <- vector()
  for (i in c(1: length(ltsdvals))) {
    val <- mean(ltsdvals[seq(from=i, to=min(i+window, length(ltsdvals)), by=1)])
    decision[i] <- val > (min(ltsdvals) + abs((factor * mean(ltsdvals))))
  }
  return(decision)
}
