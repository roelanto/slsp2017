
drawtopdf <- function(pdfpath=NULL, allpauses, pdffilename=NULL, main=" ", width=7, height=7, k=2, addlegend = FALSE, ...) {
  if (!is.null(pdf)) {
    filename <- paste0(pdfpath, pdffilename)
    pdf(file = filename, width=width, height=height)
  }
  retval <- drawFreqMG(allpauses, main=main, k=k, ...)
  # fill = c(col.alpha("blue", 1), "green"),
  if (addlegend)
    legend(x = "topright", legend = c("Component", "Result"),  lty=c(1, 5), lwd=c(2,2), col=c("blue", "green"), bty="n")
  dev.off()
  retval
}


#' creates the six panels of the histograms. 
#' @param k the k parameter as passed to normalmixEM. by drawFreqMG
#' 
#' The k-parameter specifies the number of components to normalmixEM. 
#' @param pdfpath denotes a path to which PDF files are saved. If null, don't use PDF driver. If specified, end with trailing /.
#' @param k number of Gaussians to estimate (with mixtools package)
#' @param width the width of the resulting PDF (in inches)
#' @param height the height of the resulting PDF (in inches)
#'

plotPauseFrequencyslsp <- function(x, k=2, pdfpath=NULL, width=7, height=7) {
    resdf <- x
    colname <- "pauses"
    retval <- list()
    allpauses <- unlist(resdf[startsWith(as.character(resdf$filename), "c_"),]$pauses)
    retval[[1]] <- drawtopdf(pdfpath=pdfpath, allpauses=allpauses, width=width, height=height, pdffilename="nbd.pdf", main="Non-brain-damaged", k=k)
                                        #drawFreq1(allpauses, main=paste0("Controls, N=", length(allpauses)))
    
    allpauses <- unlist(resdf[startsWith(as.character(resdf$filename), "ad_"),]$pauses)
    retval[[2]] <- drawtopdf(pdfpath=pdfpath, allpauses=allpauses, width=width, height=height, pdffilename="ad.pdf", main="Alzheimer",  k=k)
    
    allpauses <- unlist(resdf[startsWith(as.character(resdf$filename), "pd_"),]$pauses)
    retval[[3]] <- drawtopdf(pdfpath=pdfpath, allpauses=allpauses, width=width, height=height, pdffilename="pd.pdf", main="Parkinson",  k=k)
    
    allpauses <- unlist(resdf[startsWith(as.character(resdf$filename), "ppa1_"),]$pauses)
    retval[[4]] <- drawtopdf(pdfpath=pdfpath, allpauses=allpauses, width=width, height=height, pdffilename="ppanf1.pdf", main="PPA-NF #1",  k=k, bottomleft=TRUE)
    allpauses <- unlist(resdf[startsWith(as.character(resdf$filename), "ppa2_"),]$pauses)
    retval[[5]] <- drawtopdf(pdfpath=pdfpath, allpauses=allpauses, width=width, height=height, pdffilename="ppanf2.pdf", main="PPA-NF #2", k=k)
    
    allpauses <- unlist(resdf[startsWith(as.character(resdf$filename), "sd_"),]$pauses)
    retval[[6]] <- drawtopdf(pdfpath=pdfpath, allpauses=allpauses, width=width, height=height, pdffilename="ppasd.pdf", main="PPA-SD", k=k, addlegend = TRUE)
    mu1 <- 10^sapply(retval, function(i) {i[["mu"]][order(i[["mu"]])[1]]})
    mu2 <- 10^sapply(retval, function(i) {i[["mu"]][order(i[["mu"]])[2]]})
    mu3 <- 10^sapply(retval, function(i) {i[["mu"]][order(i[["mu"]])[3]]})
    sigma1 <- sapply(retval, function(i) {i[["sigma"]][order(i[["mu"]])[1]]})
    sigma2 <- sapply(retval, function(i) {i[["sigma"]][order(i[["mu"]])[2]]})
    sigma3 <- sapply(retval, function(i) {i[["sigma"]][order(i[["mu"]])[3]]})
    lambda1 <- sapply(retval, function(i) {i[["lambda"]][order(i[["mu"]])[1]]})
    lambda2 <- sapply(retval, function(i) {i[["lambda"]][order(i[["mu"]])[2]]})
    lambda3 <- sapply(retval, function(i) {i[["lambda"]][order(i[["mu"]])[3]]})
    if (k==2) {
        return (data.frame(mu1=round(mu1,0), sigma1=round(sigma1,0), mu2=round(mu2, 0), lambda1=round(lambda1,2), sigma2=round(sigma2,0), lambda2=round(lambda2,2))) 
    }
    if (!is.null(mu3) && length(mu3) > 0) {
        return (data.frame(mu1=round(mu1,0), sigma1=round(sigma1,0), mu2=round(mu2, 0), lambda1=round(lambda1,2), sigma2=round(sigma2,0), lambda2=round(lambda2,2), mu3=mu3, sigma3=sigma3, lambda3=lambda3)) 
    } 
    
}

