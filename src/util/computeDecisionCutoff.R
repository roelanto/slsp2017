
computeDecisionCutoff <- function(ltsdvals, factor) {
  # exclude crazy outliers from the min-max determination
  minval <- min(PI(ltsdvals, prob = 0.99))
  distance_to_mean <- mean(PI(ltsdvals, prob=0.99)) - min(PI(ltsdvals, prob=0.99))
  #distance_to_mean <- hsm(ltsdvals) - minval
  
  # was: max(abs(median(ltsdvals)), abs(min(ltsdvals))) - min(abs(median(ltsdvals)), abs(min(ltsdvals)))
  minval + abs(factor * distance_to_mean)
  #return(0)
}
