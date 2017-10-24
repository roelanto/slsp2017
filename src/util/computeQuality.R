computeQuality <- function(avgs, numbands=40) {
  #lmfit <- lm(avgs[c(1:numbands)] ~ c(1:numbands))
  #summary(lmfit)
  #cor(lmfit$fitted.values, avgs[c(1:40)])^2    
  sum(avgs[c(1:numbands)])
}