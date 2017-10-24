drawVADplot_b <- function(object, pal=c( col.alpha("blue", 0.4), "black", "black", col.alpha("black", 1.0), "green", "red"), outputfile="output/soa17/soavadfig_b.png", ltsdvals) {
  if (!is.null(outputfile)) {
    png(file=outputfile, height = height, width = width, units = "cm", res=300)
    set_nice_margins()
  }
  plotWithVAD(object=object, 
              ltsdvals=ltsdvals, 
              start=-1, 
              end=20, 
              drawdraft=TRUE, 
              decision=NULL, 
              fullxlim = TRUE, 
              decisionCutoff = NULL, 
              framewidth = 0.1 , pal=pal, ylim=ylim)
  legtext <- c("measurement", "envelope ")
  add_legend("topleft",
             horiz = TRUE,
             bty = "n",
             cex = 0.7,
             legend=legtext, 
             x.intersp=0.4,
             #text.width=c(0.3,0.3),
             fill = c(pal[1], pal[4]),
             border=rep(NA,2 ),
             seg.len=1)
  
  if (!is.null(outputfile)) {
    dev.off()
  }
} 

drawVADplot_b_slsp <- function(object, 
                               pal=c( col.alpha("blue", 0.4), "black", "black", col.alpha("black", 1.0), "green", "red"), 
                               outputfile="output/soa17/soavadfig_b.png", ltsdvals
                               , height=4, width=4) {
  if (!is.null(outputfile)) {
    pdf(file = outputfile, height=height, width=width)
    set_nice_margins()
  }
  plotWithVAD(object=object, 
              ltsdvals=ltsdvals, 
              start=-1, 
              end=20, 
              drawdraft=TRUE, 
              decision=NULL, 
              fullxlim = TRUE, 
              decisionCutoff = NULL, 
              framewidth = 0.1 , pal=pal, ylim=ylim)
  legtext <- c("measurement", "envelope ")
  add_legend("topleft",
             horiz = TRUE,
             bty = "n",
             cex = 0.7,
             legend=legtext, 
             x.intersp=0.4,
             #text.width=c(0.3,0.3),
             fill = c(pal[1], pal[4]),
             border=rep(NA,2 ),
             seg.len=1)
  
  if (!is.null(outputfile)) {
    dev.off()
  }
} 