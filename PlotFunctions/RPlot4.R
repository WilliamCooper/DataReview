### plot 4: plot differences, individual pairs of temperatures
RPlot4 <- function (data, ...) { 
  op <- par (mar=c(5,5,2,4)+0.1,oma=c(1.1,0,0,0))
  labelled=F
  layout(matrix(1:4, nrow=2, ncol = 2), widths = c(5,5), heights = c(6,6))
  for (i in 2:length(VRPlot[[4]])) {
    # protection against all-bad:
    if (any(!is.na(data[, VRPlot[[4]][1]]) & !is.na(data[, VRPlot[[4]][i]]))) {
      ylb <- expression (paste (VRPlot[[4]][i], "  [", degree, "C]"))
      ylb[[1]][2] <- VRPlot[[4]][i]  ## needed substitution to get ylab
      plot (data[, c(VRPlot[[4]][1], VRPlot[[4]][i])], pch=20,ylab=ylb)
      lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
      lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
      par(new=TRUE)
      plot (data[, VRPlot[[4]][1]], 
            data[, VRPlot[[4]][i]] - data[, VRPlot[[4]][1]], 
            type='l', col='red',axes=F,xlab='',ylab='',ylim=c(-2,2))
      axis (4,col='red',col.axis='red')
      abline(h=c(-1,1),col='red',lty=2)
      rside <- expression (paste (VRPlot[[4]][i], '-', VRPlot[[4]][1], "  [", 
                                  degree, "C]"))
      rside[[1]][2] <- VRPlot[[4]][i]
      rside[[1]][4] <- VRPlot[[4]][1]
      mtext(rside,4,3,col='red',cex=0.8)
      #    DF$ATH2 <- (data$ATH2 - data$ATH1)*5
      #    points (DF, col='red', type='l', lwd=2)
      #    lines (c(-70,30), c(5,5), col='red', lty=2)
      #    lines (c(-70,30), c(-5,-5), col='red', lty=2)
      # this info somewhat redundant with legend now, also cex=0.5 was too small to read in pngs but 1.0 too big for plot
      #    legend ("bottomright", legend=c("red: y=(ATH2-ATH1)*5", 
      #                                    "dashed lines: +/-1C error bands"), 
      #            box.col='red', text.col='red', cex=0.5)
      fm <- lm(data[, VRPlot[[4]][i]]~data[, VRPlot[[4]][1]])
      coef <- coefficients (fm)
      if (coef[1] < 0.) {
        t <- sprintf ("%s=%.3f(%s)%.3f\nmean diff %s-%s=%.2f +/- %.2f", 
                      VRPlot[[4]][i], coef[2], VRPlot[[4]][1], coef[1],
                      VRPlot[[4]][i], VRPlot[[4]][1],
                      mean (data[, VRPlot[[4]][i]]-data[, VRPlot[[4]][1]], na.rm=TRUE),
                      sd (data[, VRPlot[[4]][i]]-data[, VRPlot[[4]][1]], na.rm=TRUE))
      } else {
        t <- sprintf ("%s=%.3f(%s)+%.3f\nmean diff %s-%s=%.2f +/-%.2f", 
                      VRPlot[[4]][i], coef[2], VRPlot[[4]][1], coef[1],
                      VRPlot[[4]][i], VRPlot[[4]][1],
                      mean (data[, VRPlot[[4]][i]]-data[, VRPlot[[4]][1]], na.rm=TRUE),
                      sd (data[, VRPlot[[4]][i]]-data[, VRPlot[[4]][1]], na.rm=TRUE))
      }
      title(t, cex.main=0.75)
      AddFooter ()
    }
  }
}

