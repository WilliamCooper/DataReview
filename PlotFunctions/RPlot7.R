
### plot 7: dynamic pressure; also TAS and MACH
RPlot7 <- function (data, Seq=NA) { 
  op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  if (is.na (Seq) || Seq == 1) {
    QC <- VRPlot[[7]]
    n2 <- which("QCFC" == QC)
    QC <- QC[1:(n2-1)]
    plotWAC (data[, c("Time", QC)],
             col=c('blue', 'darkgreen'), ylab='QCy [hPa]', 
             legend.position='top', ylim=c(0,200))
    points (data$Time, (data[, QC[2]]-data[, QC[1]])*10+120, type='l', col='red')
    axis (4, at=c(100,120,140), labels=c("-2", "0", "2"), col='red', col.axis='red', cex.axis=0.7)
    abline (h=100, col='red', lty=2); abline (h=140, col='red', lty=2)
    ltext <- sprintf("red: (%s-%s)*10+120", QC[2], QC[1])
    legend("bottomleft", legend=c(ltext, 
                                  "dashed red: +/- 2 hPa [diff]"), cex=0.75)
    op <- par (mar=c(5,4,1,2)+0.1)
    QC <- VRPlot[[7]]
    n3 <- which("QC_A" == QC)
    QC <- QC[n2:n3]
    plotWAC (data[, c("Time", QC)],
             col=c('blue', 'darkgreen', 'cyan'), ylab=' corrected QCyC [hPa]',
             legend.position='top', ylim=c(0,200))
    points (data$Time, (data[, QC[length(QC)]]-data[, QC[1]])*10+120, type='l', col='red')
    axis (4, at=c(100,120,140), labels=c("-2", "0", "2"), col='red', col.axis='red', cex.axis=0.7)
    abline (h=140, col='red', lty=2); abline (h=100, col='red', lty=2)
    ltext <- sprintf("red: (%s-%s)*10+200", QC[length(QC)], QC[1])
    legend("bottomleft", c(ltext, 
                           "dashed red: +/- 2 hPa [diff]"), cex=0.75)
    labl <- QC
    labl <- sub("QC", "", labl)
    titl <- "Mean diff: "
    for (i in 2:length(labl)) {
      titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
                      mean(data[, VRPlot[[7]][i+n2-1]] -
                             data[, VRPlot[[7]][n2]], na.rm=TRUE))
    }
    title(titl, cex.main=0.8)
    AddFooter ()
    if (!is.na(Seq) && (Seq == 1)) {return()}
  }
  # add TAS and MACH plots:
  op <- par (mar=c(2,4,1,1)+0.1)
  TAS <- VRPlot[[7]]
  TAS <- TAS[which("TAS" == substr(TAS, 1, 3))]
  plotWAC (data[, c("Time", TAS)], 
           col=c('blue', 'darkorange', 'darkgreen', 'cyan'), ylab='TASy [m/s]', 
           legend.position='top')
  points(data$Time, (data[, TAS[length(TAS)]] - data[, TAS[1]]) *20+200, type='l',
         col='red')  
  ltext <- sprintf("red: (%s-%s)*20+200", TAS[length(TAS)], TAS[1])
  legend("bottomleft", c(ltext, "dashed red: +/- 1 m/s [diff]"), cex=0.75)
  hline(220, 'red'); hline(180, 'red')
  labl <- TAS
  labl <- sub("TAS", "", labl)
  titl <- "Mean diff: "
  for (i in 2:length(labl)) {
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
                    mean(data[, TAS[i]] - data[, TAS[1]], na.rm=TRUE))
  }
  title(titl, cex.main=0.8)
  op <- par (mar=c(5,4,1,1)+0.1)
  MACH <- VRPlot[[7]]
  MACH <- MACH[which("MACH" == substr(MACH, 1, 4))]
  plotWAC (data[, c("Time", MACH)], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='MACHy', 
           legend.position='top')
  AddFooter ()
}
