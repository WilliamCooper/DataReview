### plot 5: humidity
RPlot5 <- function (data, Seq=NA) { 
  ## if EW_VXL or CAVP missing, fill in:
  if (!("EW_VXL" %in% names(data))) {
    if ("DP_VXL" %in% names(data)) {data$EW_VXL <- MurphyKoop (data$DP_VXL)}
    if ("DPV_VXL" %in% names(data)) {data$EW_VXL <- MurphyKoop (data$DPV_VXL)}
  }
  ## special in HIPPO-2 to avoid VCSEL spikes
  # data$EW_VXL[data$EW_VXL > 20] <- NA
    
  if (!("CAVP_DPL" %in% names(data))) {
    data$CAVP_DPL <- data$PSFC*(1.065+0.001575*data$QCFC
                     - 1.2498*MachNumber (data$PSFC, data$QCFC)^2)
  } else {  # calculate from formula
    data$CAVPF_DPL <- data$PSFC*(1.065+0.001575*data$QCFC
                     - 1.2498*MachNumber (data$PSFC, data$QCFC)^2)
  }
  if (!("CAVP_DPR" %in% names (data))) {
    data$CAVP_DPR <- data$PSFC*(1.0162 +0.003024*data$QCFC
                     - 1.34521*MachNumber (data$PSFC, data$QCFC)^2)
  } else {
    data$CAVPF_DPR <- data$PSFC*(1.0162 +0.003024*data$QCFC
                                - 1.34521*MachNumber (data$PSFC, data$QCFC)^2)
  }
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
  ls <- which (VRPlot[[5]] == "ATX")
  plotWAC (data[, c("Time", VRPlot[[5]][1:ls])], 
           ylab=expression (paste ("dew point  DPy  [", degree, "C]")), 
           lty=c(1,1,2,1), lwd=c(2,1.5,1,3), legend.position='bottom', 
           col=c('blue', 'red', 'darkgreen', 'black'))
  # pulling legend out of plotWAC to increase font size
  # legend('bottomright',c("DP_DPL", "DP_DPR", "DP_VXL", "ATX"),col=c("blue","red","darkgreen","black"),text.col=c("blue","red","darkgreen","black"),lty=c(1,1,2,1),lwd=c(2,1.5,1,3))
  labl <- VRPlot[[5]]
  labl <- sub("DP_", "", labl)
  titl <- "Mean diff "
  for (i in 2:(ls-1)) {
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
                    mean(data[, VRPlot[[5]][i]] -
                           data[, VRPlot[[5]][1]], na.rm=TRUE))
  }
  title(titl, cex.main=0.8)
  AddFooter ()
  if (!is.na(Seq) && (Seq == 1)) {return()}
# BBS 2/2/15 edited to match sensor colors and make VXL the dependent variable
# also, high transparency was making DPR look better than it actually was
  colr <- c("blue", "darkgreen", "darkorange", "cyan")
  for (i in 2:(ls-1)) {
    if (i == 2) {
      plot(data[, c(VRPlot[[5]][1], VRPlot[[5]][i])], pch=20, col=colr[i-1], xlab=expression (paste ("DP_VXL  [", degree, "C]")),
           ylab=expression (paste ("dew point  DPy  [", degree, "C]")))
      lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
      lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
    } else {
      points (data[, c(VRPlot[[5]][1], VRPlot[[5]][i])], pch=20, col=colr[i-1], cex=0.5)
    }
  }
  legend ('bottomright', legend=c(VRPlot[[5]][2:(ls-1)]), col=colr,
          text.col=colr, pt.cex=c(1., 0.5, 0.5, 0.5))
  title("dashed orange lines: +/-1C error bands", cex.main=0.8)
  AddFooter ()
  if (!is.na(Seq) && (Seq == 2)) {return()}
  # DP cavity pressures and VCSEL laser intensity:
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,5))
  op <- par (mar=c(2,4,1,2.5)+0.1)
  if (("CAVPE_DPL" %in% names(data))) {
    plotWAC (data[, c("Time", "CAVP_DPR", "CAVP_DPL", "CAVPE_DPR", "CAVPE_DPL", "PSFC")], 
             lwd=c(1,1,2,2,1), lty=c(1,1,2,2,1), ylab='CAVP [hPa]',legend.position='topleft')
  } else if ("CAVPF_DPL" %in% names (data)) {
    plotWAC (data[, c("Time", "CAVP_DPR", "CAVP_DPL", "CAVPF_DPR", "CAVPF_DPL", "PSFC")], 
             lwd=c(1,1,2,2,1), lty=c(1,1,2,2,1), ylab='CAVP [hPa]',legend.position='topleft') 
  } else {
    plotWAC (data[, c("Time", "CAVP_DPR", "CAVP_DPL", "PSFC")], 
             lwd=c(1,1,2,2,1), lty=c(1,1,2,2,1), ylab='CAVP [hPa]',legend.position='topleft')    
  }
  # pulling legend out of plotWAC to increase font size
  # legend('bottomright',c("CAVP_DPR", "CAVP_DPL", "PSFC"),col=c("blue","darkgreen","red"),text.col=c("blue","darkgreen","red"),lty=c(1,2,1),lwd=c(2,1,1),cex=0.75)
  title (sprintf ("mean above PSFC: %.1f (DPL) and %.1f (DPR)", 
                  mean (data$CAVP_DPL - data$PSFC, na.rm=TRUE),
                  mean (data$CAVP_DPR - data$PSFC, na.rm=TRUE)), cex.main=0.75)
  op <- par (mar=c(5,4,1,2.5)+0.1)
  if ("LSRINT_VXL" %in% names(data)) {
    plotWAC (data[, c("Time", "LSRINT_VXL")], ylim=c(0,4000),ylab="LSRINT_VXL")
    abline (h=1000, col='red', lty=2); abline (h=2700, col='red', lty=2)
  }
  AddFooter ()
  if (!is.na(Seq) && (Seq == 3)) {return()}
  # vapor pressure and mixing ratio
  op <- par (mar=c(2,5,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  ## get EW variables
  VEW <- VRPlot[[5]]
  VEW <- VEW[which ("EW" == substr(VEW, 1, 2))]
  if (!("EW_VXL" %in% VEW) && ("EW_VXL" %in% names(data))) {VEW <- c(VEW, "EW_VXL")}
  plotWAC (data[, c("Time", c(VEW))], ylab="EWy [hPa]", 
           logxy='y', ylim=c(1e-2, 100),legend.position='bottom',cex.lab=1.5,cex.axis=1.5)
  lineWAC (data$Time, MurphyKoop (data$ATX, data$PSFC), col='cyan', lty=2)
  # pulling legend out of plotWAC to increase font size
  # legend('bottomright',c("EW@ATX","EW_DPL", "EW_DPR", "EW_VXL"),col=c("cyan","blue","darkgreen","red"),text.col=c("cyan","blue","darkgreen","red"),lty=c(2,1,1,1),lwd=c(2,1,1,1))
  title ("cyan line: equilibrium vapor pressure at ATX")
  if ("MR" %in% names(data)) {
    MRVAR <- sub ("EW", "MR", VEW)
    for (i in 1:length(MRVAR)) {
      data[, MRVAR[i]] <- 0.622 * data[, VEW[i]] / (data$PSFC-data[, VEW[i]]) * 1000
    }
    if ("H2OMR_GMD" %in% names(data)) {
      MRVAR <- c(MRVAR, "H2OMR_GMD")
      data$H2OMR_GMD <- data$H2OMR_GMD / 1000. * 0.622
    }
    plotWAC (data[, c("Time", MRVAR)], ylab="mixing ratio [g/kg]",
             logxy='y', ylim=c(0.01, 100),cex.lab=1.5,cex.axis=1.5)
  }
  op <- par (mar=c(5,5,1,1)+0.1)
  RHVAR <- sub("EW", "RH", VEW)
  for (i in 1:length (RHVAR)) {
    data[, RHVAR[i]] <- 100 * data[, VEW[i]] / MurphyKoop (data$ATX, data$PSFC)
  }
  plotWAC (data[, c("Time", RHVAR)], lty=c(1,1,2), lwd=1, 
           ylab="relative humidity [%]",cex.lab=1.5,cex.axis=1.5, 
           legend.position='topright', ylim=c(0,150))
  # pulling legend out of plotWAC to increase font size
  # legend('topright',c("RHDPL", "RHDPR", "RHVXL"),col=c("blue","darkgreen","red"),text.col=c("blue","darkgreen","red"),lty=c(1,1,2),lwd=1)
  abline (h=100, col='red', lty=2)
  AddFooter ()
}

