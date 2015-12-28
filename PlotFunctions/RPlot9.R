### plot 9: wind
RPlot9 <- function (data, Seq=NA) {
  ## needs WDC, WSC, WIC, IWD, IWS, ADIFR, QCF, PSF
  op <- par (mar=c(2,5,1,1)+0.1,oma=c(1.1,0,0,0))
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  # set high transparency (30) to avoid obscuring first trace
  tgreen <- rgb(0,200,0,120,maxColorValue=255)
  line.colors=c('blue', tgreen, 'red')
  line.widths <- c(1,1,2)
  line.types <- c(1,3,2)
  plotWAC (data[, c("Time", "WDC", "IWD")], 
           col=line.colors, lwd=line.widths, lty=line.types, ylab=expression (paste ("WDC [",degree,"]")),legend.position=NA,cex.axis=1.5,cex.lab=1.5)
  legend('bottomright',c("WDC", "IWD"),col=c("blue",tgreen),text.col=c("blue",tgreen),lty=c(1,3),lwd=c(1,1))
  hline (0); hline (90); hline (180); hline (270); hline (360)
  plotWAC (data[, c("Time", "WSC", "IWS")], 
           col=line.colors, lwd=line.widths, lty=line.types, ylab="WSC [m/s]",legend.position=NA,cex.axis=1.5,cex.lab=1.5)
  legend('bottomright',c("WSC", "IWS"),col=c("blue",tgreen),text.col=c("blue",tgreen),lty=c(1,3),lwd=c(1,1))
  op <- par (mar=c(5,5,2,1)+0.1)
  cf <- c(5.151, 15.654, 7.299)
  data$AK <- cf[1] + data$ADIFR/data$QCF * (cf[2] 
                                      + cf[3] * MachNumber(data$PSF, data$QCF))
  # next line is no longer necessary; only in original HIPPO processing
  # data$VSPD_G <- (data$VSPD_G + 0.06) / 1.02
  # data$WIX <- data$WIC + (data$AK-data$AKRD)*pi*data$TASF/180. + (data$VSPD_A-data$VSPD)
  # data$WIXS <- SmoothInterp (data$WIX)
  plotWAC (data[, c("Time", "WIC")], ylab="vertical wind WIC [m/s]",cex.axis=1.5,cex.lab=1.5)
  title (sprintf ("flight-average vertical wind: WIC %.02f", 
                  mean (data$WIC, na.rm=TRUE)), cex.main=1.5)
  hline (2); hline (-2); hline (0,'red')
  AddFooter ()
  if (!is.na(Seq) && (Seq == 1)) {return()}
  op <- par (mar=c(2,4,1,1)+0.1)
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  data$IUX <- data$IWS * sin (data$IWD*pi/180)
  data$UIC <- data$WSC * sin (data$WDC*pi/180)
  plotWAC (data[, c("Time", "UIC", "IUX")], col=line.colors, lwd=line.widths, lty=line.types, 
           ylab="easterly wind [m/s]",legend.position=NA)
  legend('bottom',c("UIC", "IUX"),col=c("blue",tgreen),text.col=c("blue",tgreen),lty=c(1,3),lwd=c(1,1),cex=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  data$IVY <- -data$IWS * cos (data$IWD*pi/180)
  data$VIC <- -data$WSC * cos (data$WDC*pi/180)
  plotWAC (data[, c("Time", "VIC", "IVY")], col=line.colors, lwd=line.widths, lty=line.types, 
           ylab="southerly wind [m/s]",legend.position=NA)
  legend('bottom',c("VIC", "IVY"),col=c("blue",tgreen),text.col=c("blue",tgreen),lty=c(1,3),lwd=c(1,1),cex=0.75)
  AddFooter ()
}

