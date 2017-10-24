drawVADplot_c <- function(object, pal=c( col.alpha("blue", 0.4), "black", "black", col.alpha("black", 1.0), "green", "red"), outputfile="output/soa17/soavadfig_c.png", ltsdvals, decision, decisionCutoff) {
  if (!is.null(outputfile)) {
    png(file=outputfile, height = height, width = width, units = "cm", res=300)
    set_nice_margins()
  }
  plotWithVAD(object=object, 
              ltsdvals=ltsdvals, 
              start=-1, 
              end=20, 
              drawdraft=TRUE, 
              decision=decision, 
              fullxlim = TRUE, 
              decisionCutoff = decisionCutoff, 
              framewidth = 0.1,
              ylim=ylim)
  legtext <- c("measurement", "envelope ", "speech", "non-speech", "pause", "cutoff")
  add_legend("topleft",
             horiz = TRUE,
             bty = "n",
             cex = 0.7,
             legend=legtext, 
             x.intersp=0,
             text.width=c(0.3,0.3,0.25,0.22,0.23,0.22),
             fill = c(pal[1], pal[4], pal[5], pal[6], col.alpha(acol="yellow", alpha = 0.4), pal[3]),
             lty = c(NA, NA, NA, NA, NA, 3), density=c(NA, NA, NA, NA, NA ,0), 
             border=rep(NA, 7),
             seg.len=1)
  
  if (!is.null(outputfile)) {
    dev.off()
  }
} 


drawVADplot_c_slsp <- function(object, pal=c( col.alpha("blue", 0.4), "black", "black", col.alpha("black", 1.0), "green", "red"), outputfile="output/soa17/soavadfig_c.png", ltsdvals, decision, decisionCutoff, height=4, width=4) {
  if (!is.null(outputfile)) {
    pdf(file = outputfile, height=height, width=width)
    set_nice_margins()
  }
  plotWithVAD(object=object, 
              ltsdvals=ltsdvals, 
              start=-1, 
              end=20, 
              drawdraft=TRUE, 
              decision=decision, 
              fullxlim = TRUE, 
              decisionCutoff = decisionCutoff, 
              framewidth = 0.1,
              ylim=ylim)
  legtext <- c("measurement", "envelope ", "speech", "non-speech", "pause", "cutoff")
  add_legend("topleft",
             horiz = TRUE,
             bty = "n",
             cex = 0.7,
             legend=legtext, 
             x.intersp=0,
             text.width=c(0.3,0.3,0.25,0.22,0.23,0.22),
             fill = c(pal[1], pal[4], pal[5], pal[6], col.alpha(acol="yellow", alpha = 0.4), pal[3]),
             lty = c(NA, NA, NA, NA, NA, 3), density=c(NA, NA, NA, NA, NA ,0), 
             border=rep(NA, 7),
             seg.len=1)
  
  if (!is.null(outputfile)) {
    dev.off()
  }
} 