
## a batch script that can be used like the previous Review.R programs.
## usage: 
## cd ~/RStudio/DataReview
## Rscript Review.R  #defaults to next pdf-plot and all times
## --or--
## Rscript Review.R 5 150000 200000  #to specify rf05 and 15:00:00 to 20:00:00

suppressMessages (
  library(shiny, quietly=TRUE, warn.conflicts=FALSE)
)
suppressMessages (suppressWarnings (
  library(Ranadu, quietly=TRUE, warn.conflicts=FALSE))
)

Project <- 'PREDICT'
Project <- 'TORERO'
Project <- 'DC3'
run.args <- commandArgs (TRUE)
if (length (run.args) > 0) {
  Flight <- as.numeric (run.args[1])
} else {
  ## find max rf in data directory, use as default if none supplied via command line:
#   Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), Project),
#                           sprintf ("%s_rf...nc", Project)), decreasing = TRUE)[1]
  ## use next plot file instead as default
  Fl <- sort (list.files (sprintf ("~/RStudio/DataReview"),
                          sprintf ("%srf..Plots.pdf", Project)), decreasing = TRUE)[1]
  if (is.na (Fl)) {
    Flight <- 1
  } else {
    Flight <- sub (Project, '',  sub ("Plots.pdf", '', Fl))
    Flight <- as.numeric(substr(Flight, 3, 4))+1
  }
}
STime <- 0
ETime <- 0  ## interpreted as end-of-flight time
if (length (run.args) > 1) {STime <- as.integer (run.args[2])}
if (length (run.args) > 2) {ETime <- as.integer (run.args[3])}
nplots <- c(1, 3:17, 19:23)    # project default
psq <- c(1,1, 1,2, 3,1, 4,1, 5,1, 5,2, 5,3, 5,4, 6,1, 7,1, 7,2,  #11
         8,1, 9,1, 9,2, 10,1, 10,2, 11,1, 12,1, 13,1, 14,1, 15,1, 15,2, #22
         16,1, 16,2, 16,3, 17,1, 19,1, 19,2, #28
         20,1, 20,2, 20,3, 20,4, 21,1, 21,2, 21,3, 21,4, #36
         22,1, 22,2, 22,3, 22,4, 23,1, 23,2, #42
         24,1, 25,1, 26,1, 27,1, 28,1, 29,1, 30,1) #49
L <- length (psq)/2
dim(psq) <- c(2,L)
netCDFfile <- NA
CCDP <- NA

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

## assemble a list of projects for which an appropriately named rf01
## exists in the data directory:

# PJ <- c('ORCAS', 'CSET', 'NOREASTER', 'HCRTEST',
#         'DEEPWAVE', 'CONTRAST', 'SPRITE-II', 'MPEX', 'DC3',
#         'TORERO', 'HIPPO-5', 'HIPPO-4', 'HIPPO-3', 'HIPPO-2',
#         'HIPPO-1','PREDICT', 'START08', 'PACDEX', 'TREX')
# for (P in PJ) {
#   if (grepl('HIPPO', P)) {
#     fn <- sprintf ('%sHIPPO/%srf01.nc', DataDirectory (), P)
#   } else {
#     fn <- sprintf ('%s%s/%srf01.nc', DataDirectory (), P, P)
#     if (!file.exists (fn)) {
#       fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), P, P)
#     }
#   }
#   if (!file.exists (fn)) {PJ[PJ==P] <- NA}
# }
# PJ <- PJ[!is.na(PJ)]
# 
# ## now test that there is an entry in the Configuration.R file for PJ:
# lines <- readLines ('Configuration.R')
# for (P in PJ) {
#   if (!any (grepl (P, lines))) {PJ[PJ == P] <- NA}
# }
# PJ <- PJ[!is.na(PJ)]
# rm (lines)

## make plot functions available
for (np in 1:2) {
  if (testPlot(np)) {
    eval(parse(text=sprintf("source(\"~/RStudio/DataReview/PlotFunctions/RPlot%d.R\")", np)))
  }
}
for (np in 3:30) {
  if (file.exists (sprintf ("~/RStudio/DataReview/PlotFunctions/RPlot%d.R", np))) {
    if (testPlot(np)) {
      eval(parse(text=sprintf("source(\"~/RStudio/DataReview/PlotFunctions/RPlot%d.R\")", np)))
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

# saveConfig <- function() {
#   print ('entered saveConfig')
#   file.copy('Configuration.R','Configuration.R.backup', overwrite=TRUE)
#   lines <- readLines ('Configuration.R')
#   unlink ('Configuration.R')
#   startLine <- which (grepl (sprintf ('%s', Project), lines))
#   endLine <- which (grepl ('Project', lines[(startLine+1):length(lines)]))[1]
#   if (is.na(endLine)) {
#     endLine <- length (lines)
#   } else {
#     endLine <- endLine + startLine - 1
#   }
#   linesOut <- lines[1:startLine]
#   while (grepl ('offset',lines[startLine+1])) {
#     startLine <- startLine + 1
#     linesOut <- c(linesOut, lines[startLine])
#   }
#   v <- VRPlot$PV1
#   linesOut <- c(linesOut, paste('  VRPlot <- list(PV1 = ',list(v), ')', sep=''))
#   for (i in 2:length(VRPlot)) {
#     v <- as.character(VRPlot[[i]])
#     if (i %in% c(15:16,20:22)) {
#       v <- sub ('_.*', '_', v)
#     }
#     if (length (v) > 1) {
#       linesOut <- c(linesOut, 
#                     paste(sprintf ('  VRPlot$PV%d <- ',i),list(v),sep=''))
#     } else {
#       if (is.na(v) || (length (v) == 0)) {
#         linesOut <- c(linesOut, sprintf ('  VRPlot$PV%d <- c(NA)', i))
#       } else {
#         linesOut <- c(linesOut, sprintf ('  VRPlot$PV%d <- "%s"', i, v))
#       }
#     }
#   }
#   linesOut <- c(linesOut, '}\n ')
#   linesOut <- c(linesOut, lines[endLine:(length(lines))])
#   writeLines(linesOut, 'Configuration.R')
#   print (sprintf ('saved configuration for %s in Configuration.R', Project))
#   source ('Configuration.R')
#   print (str(VRPlot))
# }

savePDF <- function(Data, inp) {
  print ('entered savePDF')
  plotfile = sprintf("%s%s%02dPlots.pdf", inp$Project, inp$typeFlight, inp$Flight)
  unlink (plotfile)
  cairo_pdf (filename = plotfile, onefile=TRUE)
  ## enable something like the next to get individual png files instead of one large pdf
  #### png (file = sprintf ("./Figures/WINTER%s-%%02d.png", Flight))
  print (sprintf ("saving plots to file %s", plotfile))
  DataV <- limitData (Data, inp)
  t1 <- inp$times[1]
  t <- as.POSIXlt (t1, tz='UTC')
  StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
  DataV <- DataV[(DataV$Time > inp$times[1]) & (DataV$Time < inp$times[2]), ]
  for (np in 1:30) {
    if (file.exists (sprintf ("~/RStudio/DataReview/PlotFunctions/RPlot%d.R", np))) {
      if (testPlot(np) && (length(VRPlot[[np]]) > 0)) {
        print(paste('Plot',np))
        ## eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
        if (np == 1) {
          RPlot1 (DataV, sprintf ('%s%02d', inp$typeFlight, inp$Flight))
        } else {
          eval(parse(text=sprintf("RPlot%d(DataV)", np)))
        }
      }
    }
  }
  dev.off()
  #   suppressWarnings(if (length (system ('which evince', intern=TRUE)) > 0) {
  #     system (sprintf ('evince %s', plotfile))
  #   })
#   if (suppressWarnings(library(rstudio, logical.return=TRUE))) {
#     rstudio::viewer (plotfile, height='maximize')
#   }
  print ('finished savePDF')
}
savePNG <- function(Data, inp) {
  print ('entered savePNG')
  #   plotfile = sprintf("%s%sPlots.pdf", inp$Project, inp$Flight)
  #   unlink (plotfile)
  # cairo_pdf (filename = plotfile, onefile=TRUE)
  ## enable something like the next to get individual png files instead of one large pdf
  png (file = sprintf ("./PNG/%s%s%02dPlot%%02d.png", inp$Project, 
                       inp$typeFlight, inp$Flight), width=800, height=800)
  print (sprintf ("saving png plots to subdirectory PNG"))
  DataV <- limitData (Data, inp)
  t1 <- inp$times[1]
  t <- as.POSIXlt (t1)
  StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
  DataV <- DataV[(DataV$Time > inp$times[1]) & (DataV$Time < inp$times[2]), ]
  for (np in 1:30) {
    if (file.exists (sprintf ("./PlotFunctions/RPlot%d.R", np))) {
      if (testPlot(np) && (length(VRPlot[[np]]) > 0)) {
        print(paste('Plot',np))
        ## eval(parse(text=sprintf("source(\"PlotFunctions/RPlot%d.R\")", np)))
        if (np == 1) {
          RPlot1 (DataV, sprintf ('%s%02d', inp$typeFlight, inp$Flight))
        } else {
          eval(parse(text=sprintf("RPlot%d(DataV)", np)))
        }
      }
    }
  }
  dev.off()
  #   suppressWarnings(if (length (system ('which evince', intern=TRUE)) > 0) {
  #     system (sprintf ('evince %s', plotfile))
  #   })
  print ('finished savePNG')
}

saveRdata <- function (Data, inp) {
  print ('entered saveRdata')
  fn <- sprintf ('%s%s/%s%s%02d.Rdata', DataDirectory (),
                 inp$Project, inp$Project, inp$typeFlight,
                 inp$Flight)
  save (Data, file=fn)
  print (sprintf ('saved data.frame to %s', fn))
}

SeekManeuvers <- function (Data) {
  source ("~/RStudio/DataReview/PlotFunctions/SpeedRunSearch.R")
  source ("~/RStudio/DataReview/PlotFunctions/CircleSearch.R")
  source ("~/RStudio/DataReview/PlotFunctions/PitchSearch.R")
  source ("~/RStudio/DataReview/PlotFunctions/YawSearch.R")
  source ("~/RStudio/DataReview/PlotFunctions/ReverseHeadingSearch.R")
  print ('list of maneuvers:')
  PitchSearch (Data)
  YawSearch (Data)
  SpeedRunSearch (Data)
  CircleSearch (Data)
  ReverseHeadingSearch (Data)
  print ('end of maneuver list')
}

## get VRPlot and chp/shp:
## load a starting-point version
loadVRPlot <- function (Project, Production, Flight, psq) {
  source ('~/RStudio/DataReview/Configuration.R')
  # print (sprintf ('in loadVRPlot, Project=%s', Project))
  # print (VRPlot)
  # print (str(VRPlot))
  ## this leaves VRPlot defined
  if (length(VRPlot) < 30) {
    nm <- names (VRPlot)
    for (i in (length(VRPlot)+1):30) {
      VRPlot[[i]] <- c('TASX', 'ATX')
      nm[i] <- sprintf ('PV%d', i)
    }
    names(VRPlot) <- nm
  }
  fn <- sprintf ('%s%s/%srf%02d.nc', DataDirectory (), Project, Project, 
                 Flight)
#   if (!file.exists (fn)) {
#     fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), Project, Project)
#   }
  ## if Production load production-file info
#   if (Production) {
#     print (sprintf ('production section in global, Production=%d',
#                     Production))
#     dr <- sprintf ('%s../raf/Prod_Data/%s', DataDirectory (), Project)
#     scmd <- sprintf ('ls -lt `/bin/find %s -ipath "\\./movies" -prune -o -ipath "\\./*image*" -prune -o -name %s%s%02d.Rdata`',
#                      dr, Project, 'rf', Flight)
#     fl <- system (scmd, intern=TRUE)[1]
#     if ((length (fl) > 0) && (!grepl ('total', fl))) {
#       fn <- sub ('.* /', '/', fl[1])
#     }
#   }
  
  if (!file.exists (fn)) {
    warning ('netCDF file for requested flight number not present')
    return (VRPlot)
  }
  # print (sprintf ('setting chp/slp from %s', fn))
  FI <<- DataFileInfo (fn)
  LAT <- FI$Variables[grepl ('^LAT', FI$Variables)]
  LON <- FI$Variables[grepl ('^LON', FI$Variables)]
  ALT <- FI$Variables[grepl ('ALT', FI$Variables)]
  WD <- FI$Variables[grepl ('WD', FI$Variables)]
  WS <- FI$Variables[grepl ('WS', FI$Variables) & !grepl ('FLOW', FI$Variables)]
  AT <- FI$Variables[grepl ('^AT', FI$Variables) & !grepl ('ATTACK', FI$Variables)]
  DP <- FI$Variables[grepl ('^DP', FI$Variables)]
  EWW <- FI$Variables[grepl ('^EW', FI$Variables)]
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
  RAD <- FI$Variables[grepl ('^RS', FI$Variables) 
                      | (grepl ('^IR', FI$Variables) & !grepl ('IRIG', FI$Variables))]
  CONC <- FI$Variables[grepl ('^CONC', FI$Variables)]
  DBAR <- FI$Variables[grepl ('DBAR', FI$Variables)]
  LWC <- FI$Variables[grepl ('LWC', FI$Variables) & !grepl ('UFLWC', FI$Variables)]
  if ('RICE' %in% FI$Variables) {LWC <- c(LWC, 'RICE')}
  THETA <- FI$Variables[grepl ('THETA', FI$Variables)]
  im <- pmatch (c ("TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_"),
                FI$Variables)
  im <- im[!is.na(im)]
  HSKP <- FI$Variables[im]
  im <- match(c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL"),
              FI$Variables)
  im <- im[!is.na(im)]
  CHEM <- FI$Variables[im]
  im <- pmatch (c ("USHFLW_", "USMPFLW_", "UREF_", "USCAT_"),
                FI$Variables)
  im <- im[!is.na(im)]
  USH <- FI$Variables[im]
  chp <- list ()
  slp <- list ()
  chp[[1]] <- c(LAT,LON,WD,WS)
  chp[[2]] <- c(ALT,'PSXC')
  chp[[3]] <- AT
  chp[[4]] <- chp[[3]]
  chp[[5]] <- c(DP,'ATX',CAVP,EWW)
  chp[[6]] <- chp[[5]]
  chp[[7]] <- chp[[5]]
  chp[[8]] <- chp[[5]]
  chp[[9]] <- PS
  chp[[10]] <- c(QC,TAS,MACH)
  chp[[11]] <- chp[[10]]
  chp[[12]] <- c(PS,QC,'AKRD')
  chp[[13]] <- c(WD,WS,'WIC')
  chp[[14]] <- chp[[13]]
  chp[[15]] <- c(EW,NS,'GGQUAL')
  chp[[16]] <- chp[[15]]
  chp[[17]] <- VRPlot[[11]]
  chp[[18]] <- c(PITCH,ROLL,THDG)
  chp[[19]] <- c(ACINS,VSPD,ALT)
  chp[[20]] <- RAD
  chp[[21]] <- c(CONC)
  if (length (USH) > 0) {chp[[21]] <- c(CONC, USH)}
  chp[[22]] <- chp[[21]]
  chp[[23]] <- c(DBAR,LWC,HSKP)
  chp[[24]] <- chp[[23]]
  chp[[25]] <- chp[[23]]
  chp[[26]] <- c('PSXC','ATX', 'DPXC',PS,AT,DP)
  chp[[27]] <- THETA
  chp[[28]] <- THETA
  chp[[29]] <- FI$Variables[grepl ('CCDP_', FI$Variables) 
                            | grepl ('CS100_', FI$Variables)]
  chp[[30]] <- chp[[29]]
  chp[[31]] <- chp[[29]]
  chp[[32]] <- chp[[29]]
  chp[[33]] <- FI$Variables[grepl ("CUHSAS_", FI$Variables)
                            | grepl ('CS200_', FI$Variables)]
  chp[[34]] <- chp[[33]]
  chp[[35]] <- chp[[33]]
  chp[[36]] <- chp[[33]]
  chp[[37]] <- FI$Variables[grepl ('C1DC_', FI$Variables)]
  chp[[38]] <- chp[[37]]
  chp[[39]] <- chp[[37]]
  chp[[40]] <- chp[[37]]
  chp[[41]] <- CHEM
  chp[[42]] <- CHEM
  chp[[43]] <- sort(FI$Variables)
  chp[[44]] <- sort(FI$Variables)
  chp[[45]] <- c('TASX', 'ATX')
  chp[[46]] <- c('TASX', 'ATX')
  chp[[47]] <- c('TASX', 'ATX')
  chp[[48]] <- c('TASX', 'ATX')
  chp[[49]] <- c('TASX', 'ATX')
  chp <<- chp
  for (i in c(1:20,26:28,41:49)) {
    j <- psq[1, i]
    sl <- as.vector(chp[[i]])
    slp[[i]] <- sl[sl %in% VRPlot[[j]]]
  }
  for (i in c(21:25,29:40)) {
    j <- psq[1, i]
    sl <- as.vector(chp[[i]])
    k <- pmatch (VRPlot[[j]], sl)
    sl <- sl[k]
    sl <- sl[!is.na(sl)]
    slp[[i]] <- sl
  }
  slp <<- slp
  return (VRPlot)
}
if (Project != 'PREDICT') {
  fname <- sprintf ('%s%s/%srf%02d.nc', DataDirectory (), Project, Project, Flight)
} else {
  fname <- sprintf ('%s%s/%srf%02dHW.nc', DataDirectory (), Project, Project, Flight)
}
# if (!file.exists (fname)) {
#   fname <- sprintf ('%s%s/%stf01.nc', DataDirectory (), Project, Project)
# }
if (!file.exists (fname)) {warning ('netCDF file for requested flight was not found')}
FI <- DataFileInfo (fname)

limitData <- function (Data, input) {
  DataV <- Data
  namesV <- names(DataV)
  namesV <- namesV[namesV != "Time"]
  if (input$limits) {
    t <- !is.na (DataV$TASX) & (DataV$TASX < 110)
    t <- t | (abs(DataV$ROLL) > 5)
    t[is.na(t)] <- FALSE
    DataV[t, namesV] <- NA
  }
  return (DataV)
}

makeVRPlot <- function (slp, psq) {
  VR <- list(PV1=c(slp[[1]], slp[[2]]))
  VR$PV2 <- VR$PV1
  for (j in 3:30) {
    V <- vector()
    for (i in j:(length(psq)/2)) {
      if (psq[1,i] == j) {
        V <- (c (V, as.vector(slp[[i]])))
      }
    }
    if (j %in% 15:16) {
      V <- sub ('_.*$', '_', V)
    }
    VR[j] <- list(V)
  }
  names(VR) <- sprintf ('PV%d', 1:30)
  return (VR)
}

VRPlot <- loadVRPlot(Project, FALSE, Flight, psq)

inp <- list (Project=Project)
inp$Flight <- Flight
inp$typeFlight <- 'rf'
inp$limits <- FALSE
VarList <- vector()
# VRPlot <- VRP ()
for (i in 1:length(VRPlot)) {
  for (j in 1:length (VRPlot[[i]])) {
    VarList <- c(VarList, VRPlot[[i]][j])
  }
}
VarList <- unique (VarList)
VarList <- VarList[!is.na(VarList)]
Data <- getNetCDF (fname, VarList, F=Flight)

## protect against NAs
Dt <- Data[!is.na (Data$Time), 'Time']
if (STime != 0 || ETime != 0) {
  i1 <- getIndex (Dt, STime)
  i2 <- getIndex (Dt, ETime)
  if (length(i1) == 0 || i1 <= 0) {i1 <- 1}
  if (length(i2) == 0 || i2 <= 0) {i2 <- length(Dt)}
} else {
  i1 <- 1
  i2 <- nrow (Data)
}
inp$times <- c(Dt[i1], Dt[i2])

SE <- getStartEnd (Data$Time)
i <- getIndex (Data$Time, SE[1])
FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, inp$typeFlight,
                  inp$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                  strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                  strftime(Data$Time[getIndex(Data$Time,SE[2])],
                           format="%H:%M:%S", tz='UTC'))
FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
AddFooter <<- function() {
  CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
  mtext(paste(FigFooter,'generated by ', CallingFunction,
              FigDatestr),1,outer=T,cex=0.75)
}

savePDF (Data, inp)
