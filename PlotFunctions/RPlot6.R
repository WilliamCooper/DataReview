### plot 6: ambient pressures
RPlot6 <- function (data, ...) { 
  op <- par (mar=c(5,4,1,2)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:1, ncol = 1), widths = 1, heights = 5)
  plotWAC (DF <- data[, c("Time", VRPlot[[6]])], 
           col=c('blue', 'skyblue', 'darkgreen'), ylab='pressure  PSy [hPa]')
  points (data$Time, (data[, VRPlot[[6]][2]]-data[, VRPlot[[6]][1]])*50+600, type='l', col='red')
  axis (4, at=c(500,600,700), labels=c("-2", "0", "2"), col='red', col.axis='red')
  abline (h=500, col='red', lty=2); abline (h=700, col='red', lty=2)
  # legend ("bottomleft", 
  #         legend=c("(PSFC-PS_A)*50+600", "+/-2 hPa"),
  #         lty=c(1,2), cex=0.75,
  #         col=c('red', 'red'))
  labl <- VRPlot[[6]]
  labl <- sub("PS", "", labl)
  titl <- "Mean diff: "
  for (i in 2:length(labl)) {
    titl <- sprintf("%s%s-%s: %.2f; ", titl, labl[i],labl[1],
                    mean(data[, VRPlot[[6]][i]] -
                           data[, VRPlot[[6]][1]], na.rm=TRUE))
  }
  title(titl, cex.main=0.8)
  AddFooter ()
}
