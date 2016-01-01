library(shiny)
library(Ranadu)


nplots <- c(1, 3:17, 19:20, 22:23,30)    # project default
psq <- c(1,1, 1,2, 3,1, 4,1, 5,1, 5,2, 5,3, 5,4, 6,1, 7,1, 7,2,
         8,1, 9,1, 9,2, 10,1, 10,2, 11,1, 12,1, 13,1, 15,1, 15,2,
         16,1, 16,2, 16,3, 17,1, 19,1, 19,2, 
         20,1, 20,2, 20,3, 20,4, 21,1, 22,1, 22,2, 22,3, 22,4,
         23,1, 23,2, 23,3, 23,4, 30,1, 30,2)
dim(psq) <- c(2,42)

testPlot <- function (k) {
  return(k %in% nplots || nplots == 0)
}

SmoothInterp <- function (x) {
  ## skip if there are fewer than 100 measurements
  if (length (x[!is.na(x)]) < 100) {return (x)}
  d <- zoo::na.approx (as.vector(x), maxgap=100, na.rm = FALSE)
  d[is.na(d)] <- 0
  return (signal::filter(signal::sgolay(3, 61), d))
}


## make plot functions available
for (np in 1:2) {
  if (testPlot(np)) {
    eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
  }
}
for (np in 3:30) {
  if (file.exists (sprintf ("./PlotFunctions/RPlot%d.R", np))) {
    if (testPlot(np)) {
      eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
    }
  }
}
# functions used later:
hline <<- function(y, col='black', lwd=1, lty=2) {
  abline(h=y, col=col, lwd=lwd, lty=lty)
}

formatTime <- function (time) {
  t <- as.POSIXlt (time)
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}

saveConfig <- function() {
  print ('entered saveConfig')
  print (str(VRPlot))
}

## get VRPlot and chp/shp:
## load a starting-point version
Project <- 'PACDEX'
source ('Configuration.R')
# print (str(VRPlot))
## this leaves VRPlot defined
FI <- DataFileInfo (sprintf ('%s%s/%srf10.nc', DataDirectory (), Project, Project))
LAT <- FI$Variables[grepl ('^LAT', FI$Variables)]
LON <- FI$Variables[grepl ('^LON', FI$Variables)]
ALT <- FI$Variables[grepl ('ALT', FI$Variables)]
WD <- FI$Variables[grepl ('WD', FI$Variables)]
WS <- FI$Variables[grepl ('WS', FI$Variables) & !grepl ('FLOW', FI$Variables)]
AT <- FI$Variables[grepl ('^AT', FI$Variables) & !grepl ('ATTACK', FI$Variables)
                   & !grepl ('ATX', FI$Variables)]
DP <- FI$Variables[grepl ('^DP_', FI$Variables)]
EWW <- FI$Variables[grepl ('^EW', FI$Variables) & !grepl ('EWX', FI$Variables)]
CAVP <- FI$Variables[grepl ('CAVP', FI$Variables)]
PS <- FI$Variables[grepl ('^PS', FI$Variables)]
QC <- FI$Variables[grepl ('^QC', FI$Variables) & !grepl ('TEMP', FI$Variables)]
TAS <- FI$Variables[grepl ('TAS', FI$Variables)]
MACH <- FI$Variables[grepl ('MACH', FI$Variables)]
EW <- FI$Variables[grepl ('VEW', FI$Variables)]
NS <- FI$Variables[grepl ('VNS', FI$Variables)]
PITCH <- FI$Variables[grepl ('PITCH', FI$Variables)]
ROLL <- FI$Variables[grepl ('ROLL', FI$Variables)]
THDG <- FI$Variables[grepl ('THDG', FI$Variables)]
ACINS <- FI$Variables[grepl ('ACINS', FI$Variables)]
VSPD <- FI$Variables[grepl ('VSPD', FI$Variables)]
CONC <- FI$Variables[grepl ('^CONC', FI$Variables)]
DBAR <- FI$Variables[grepl ('DBAR', FI$Variables)]
LWC <- FI$Variables[grepl ('LWC', FI$Variables)]
if ('RICE' %in% FI$Variables) {LWC <- c(LWC, 'RICE')}
HSKP <- c()
if (any (grepl ('TCNTD_', FI$Variables))) {
  HSKP <- c(HSKP, FI$Variables[grepl ('TCNTD', FI$Variables)])
}
if (any (grepl ('REJDOF_', FI$Variables))) {
  HSKP <- c(HSKP, FI$Variables[grepl ('REJDOF', FI$Variables)])
}
if (any (grepl ('AVGTRNS_', FI$Variables))) {
  HSKP <- c(HSKP, FI$Variables[grepl ('AVGTRNS', FI$Variables)])
}
if (any (grepl ('CDPLSRP_', FI$Variables))) {
  HSKP <- c(HSKP, FI$Variables[grepl ('CDPLSRP', FI$Variables)])
}
THETA <- FI$Variables[grepl ('THETA', FI$Variables)]
chp <- list ()
slp <- list ()
chp[[1]] <- c(LAT,LON,WD,WS)
chp[[2]] <- c(ALT,'PSXC')
chp[[3]] <- AT
chp[[4]] <- AT
chp[[5]] <- c(DP,'ATX')
chp[[6]] <- DP
chp[[7]] <- CAVP
chp[[8]] <- c(EWW,'EWX')
chp[[9]] <- PS
chp[[10]] <- QC
chp[[11]] <- c(TAS,MACH)
chp[[12]] <- c(PS,QC)
chp[[13]] <- c(WD,WS,'WIC')
chp[[14]] <- c(WD,WS)
chp[[15]] <- c(EW,NS,'GGQUAL')
chp[[16]] <- c(EW,NS)
chp[[17]] <- VRPlot[[11]]
chp[[18]] <- c(PITCH,ROLL,THDG)
chp[[19]] <- c(ACINS,VSPD,ALT)
chp[[20]] <- CONC
chp[[21]] <- VRPlot[[15]]
chp[[22]] <- DBAR
chp[[23]] <- LWC
chp[[24]] <- HSKP
chp[[25]] <- c('PSXC','ATX', 'DPXC',PS,AT,DP)
chp[[26]] <- THETA
chp[[27]] <- THETA

for (i in c(1:19,25:27)) {
  j <- psq[1, i]
  sl <- as.vector(chp[[i]])
  slp[[i]] <- sl[sl %in% VRPlot[[j]]]
}
for (i in 20:24) {
  j <- psq[1, i]
  sl <- as.vector(chp[[i]])
  k <- pmatch (VRPlot[[j]], sl)
  sl <- sl[k]
  sl <- sl[!is.na(sl)]
  slp[[i]] <- sl
}
