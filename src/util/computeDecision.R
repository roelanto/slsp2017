
computeDecision <-  function(ltsdvals, factor=0.4) {
  cutoff <- computeDecisionCutoff(ltsdvals, factor=factor)
  print(paste("Decision cutoff computed: ", cutoff))
  decision <- sapply(ltsdvals, function(val, cutoff) {
    val > cutoff
  }, cutoff=cutoff)
}
