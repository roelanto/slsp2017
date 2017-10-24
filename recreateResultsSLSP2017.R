libs = c('rethinking','seewave', 'tuneR','parallel', 'xtable', 'stringr', 'ggplot2', 'stats', 'MASS', 'colorspace', 'pitchtools')

## install_libraries: if TRUE,
## all needed libraries are installed.
install_libraries = FALSE 
if(install_libraries){
  install.packages(libs)
}
for (lib in libs) {
  library(lib, character.only = TRUE)
}

rm(list=ls())
source("src/util/papersize.R")
source('src/analysis/drawVADplot_a.R')
source('src/analysis/drawVADplot_b.R')
source('src/analysis/drawVADplot_c.R')
source('src/analysis/drawVADplot_d_slsp.R')
source('src/util/libraries.R')
source('src/util/constants.R')
source("src/util/plotWithVAD.R")
source("src/util/computeQuality.r")
source("src/util/computeDecision.R")
source("src/util/computeDecisionCutoff.R")
source("src/util/computeDecisionWindow.R")
source("src/util/computeAVGS.R")
source("src/util/add_legend.R")
source("src/util/computeLTSE.R")
source("src/util/computeLTSD.R")
source("src/analysis/plotPauseDensity.R")
source("src/analysis/myplotmixEM.R")
source("src/analysis/drawFreqMG.R")
source("src/analysis/plotPauseFrequency.R")

nfft <- 128
freqrange <- c(0, 22)
bands <- seq(from=1, to=max(freqrange), length.out = nfft+1)
framelengthInSeconds <- 0.01
windowlengthInFrames <- 6
from=0
to=120
ylim=c(-0.7,0.7)
audiopath <- "data_and_models/audio/"
figurespath <- "results/figures/"

if (!exists("objectcontrol")) {
    objectcontrol <- readWave(paste0(audiopath, "control_PDOSG.wav"), from=10, to=20, units="seconds")
}

if (!exists("objectppa")) {
    objectppa <- readWave(paste0(audiopath, "ppa_FPHSV3.wav"), from=220, to=230, units="seconds")
}

if (!exists("avgscontrol")) {
  avgscontrol <- computeAVGS(objectcontrol)
}

if (!exists("avgsppa")) {
  avgsppa <- computeAVGS(objectppa)
}


if (!exists("ltsematrixcontrol")) {
  ltsematrixcontrol <- computeLTSE(object=objectcontrol)
}

if (!exists("ltsematrixppa")) {
  ltsematrixppa <- computeLTSE(object=objectppa)
}
if(!exists("ltsdvalscontrol")) {
  ltsdvalscontrol <- computeLTSD(objectcontrol, ltsematrix = ltsematrixcontrol, avgs=avgscontrol)
}
  if(!exists("ltsdvalsppa")) {
  ltsdvalsppa <- computeLTSD(objectppa, ltsematrix = ltsematrixppa, avgs=avgsppa)
}
factor <- 0.6
ltsdcontrol.c <- (ltsdvalscontrol - mean(ltsdvalscontrol)) /  max(abs(PI(ltsdvalscontrol, prob=0.99999)))
ltsdppa.c <- (ltsdvalsppa - mean(ltsdvalsppa)) /  max(abs(PI(ltsdvalsppa, prob=0.99999)))
decisionCutoff <- computeDecisionCutoff(ltsdcontrol.c, factor = factor)
decision <- computeDecision( ltsdcontrol.c, factor=factor)
decisionCutoffppa <- computeDecisionCutoff(ltsdppa.c, factor = factor)
decisionppa <- computeDecision( ltsdppa.c, factor=factor)

add_legend <- function(...) {
  opar <- par(fig=c(0.1, 1, 0, 0.93), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

width=inunits("inches", 184)
height=inunits("inches", 89)

drawVADplot_d_slsp(object = objectcontrol, ltsdvals = ltsdcontrol.c, decision = decision, decisionCutoff = decisionCutoff, outputfile = paste0(figurespath, "vad_d.tiff"), width=width, height=height, top=TRUE)

drawVADplot_d_slsp(object = objectppa, ltsdvals = ltsdppa.c, decision = decisionppa, decisionCutoff = decisionCutoffppa, outputfile = paste0(figurespath, "vad_d_ppa.tiff"), width=width, height=height, top=FALSE)

load(file="data_and_models/aggregate_data/vadresults.Rdata") # loads res

slidewidth <- 25
slideheight <- 14

par(mfrow=c(1,6))
plotPauseDensity(x=vadresults, colname="normalized_numshortpauses2", xlim= c(0,1.5))
plotPauseFrequencyslsp(x=vadresults, k=2, pdfpath = figurespath, width=inunits("inches", 85), height=inunits("inches", 85))


