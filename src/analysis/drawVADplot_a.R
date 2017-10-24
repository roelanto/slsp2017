drawVADplot_a <- function(object, pal=c( col.alpha("blue", 0.4), "black", "black", col.alpha("black", 1.0), "green", "red")
, outputfile="output/soa17/soavadfig_a.png") {
  if (!is.null(outputfile)) {
    png(file=outputfile, height = height, width = width, units = "cm", res=300)
    set_nice_margins()
  }
  legtext <- c("measurement")
  
  plotWithVAD(object=object, 
              ltsdvals=NULL, 
              start=-1, 
              end=20,  
              drawdraft=TRUE, 
              decision=NULL, 
              fullxlim = TRUE, 
              decisionCutoff = NULL, 
              framewidth = 0.1, pal=pal, ylim=ylim)
  add_legend("topleft",
             horiz = TRUE,
             bty = "n",
             cex = 0.7,
             legend=legtext, 
             x.intersp=0.4,
             #text.width=c(0.3,0.3),
             fill = c(pal[1]),
             border=rep(NA),
             seg.len=1)
  
  if (!is.null(outputfile)) {
    dev.off()
  }
} 
