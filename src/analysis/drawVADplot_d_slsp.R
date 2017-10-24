
drawVADplot_d_slsp <- function(object, outputfile="output/slsp/soavadfig_d.pdf", ltsdvals, decision, decisionCutoff, height=4, width=4, top=FALSE) {
  if (!is.null(outputfile)) {
    #pdf(file=outputfile, height = height, width = width, colormodel = "cmyk")
    tiff(filename=outputfile, height=height, width=width, units="in", res=500)
    par_mf <- par("mfrow", "mfcol")
    if (all(unlist(par_mf) == 1)) {
      par(mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 3, 1) + 0.1, 
          tck = -0.02)
    }
   # set_nice_margins()
  }
  pal <- c( col.alpha("blue", 0.7), "black", "black", col.alpha("black", 0.8), "green", "red")
  par(cex = 1.4)
  
  plotWithVAD(object=object, 
              ltsdvals=ltsdvals, 
              start=-1, 
              end=20, 
              drawdraft=FALSE,
              draftskip=100,
              decision=decision, 
              fullxlim = TRUE, 
              decisionCutoff = decisionCutoff, 
              framewidth = 0.1, pal=pal, ylim=ylim, top=top)
  sapply(c(1:length(decision)), function(x) {
    if (decision[x]!=TRUE && decision[x+1]!=TRUE) {
      # message("Decision ",x,"and",x+1," are both true")
      polygon(x=c(x*(length(object)/length(decision)),
                  x*(length(object)/length(decision)),
                  (1+x)*(length(object)/length(decision)),
                  (1+x)*(length(object)/length(decision))), 
              y=c(max(ylim)-0.1,min(ylim)+0.1,min(ylim)+0.1,max(ylim)-0.1), col=col.alpha(acol="yellow", alpha = 0.4), border=NA)
      
    }
    
  })
  legtext <- c("measurement", "envelope ", "pause", "cutoff")
  if (top) {
  add_legend("topleft",
             horiz = TRUE,
             bty = "n",
             cex = 1.0,
             legend=legtext, 
             x.intersp=0,
             text.width=c(0.9,0.5,0.4,0.33),
             fill = c(pal[1], pal[4],  col.alpha(acol="yellow", alpha = 0.6), pal[3]),
             lty = c(NA, NA,NA, 3), density=c(NA, NA,  NA ,0), 
             border=rep(NA, 4),
             seg.len=1)
  }
  if (!is.null(outputfile)) {
    dev.off()
  }
} 
