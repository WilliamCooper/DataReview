### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
RPlot16 <- function (data, Seq=NA) {
  if ("DBAR1DC_" %in% VRPlot[[16]]) {
    layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  } else {
    layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  }
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  DB <- VRPlot[[16]]
  DB <- DB[which(("DBARU" == substr(DB, 1, 5)) | ("DBARP" == substr(DB, 1, 5)))]
  va <- vector()
  for (c in DB) {
    nm <- names(data)[grepl(c, names(data))]
    v <- sub("_", "", c)
    data[, v] <- SmoothInterp(data[, nm])
    va <- c(va, v)
  }
  if (!"DBAR1DC_" %in% VRPlot[[16]]) {
    op <- par (mar=c(5,4,1,1)+0.1)
  }
  if (length (va) > 0) {
    plotWAC (data[, c("Time", va)], ylim=c(0,0.5), ylab="DBARU/P", 
             legend.position="topright")
  }
  title ("1-min filter", cex.main=0.75)
  DB <- VRPlot[[16]]
  DB <- DB[which (("DBAR" == substr(DB, 1, 4)) & ("DBARU" != substr(DB, 1, 5))
                  & ("DBARP" != substr(DB, 1, 5)))]
  va <- vector()
  for (c in DB) {
    nm <- names(data)[grepl(c, names(data))]
    v <- sub("_", "", c)
    data[, v] <- SmoothInterp(data[, nm])
    va <- c(va, v)
  }
  plotWAC(data[, c("Time", va)], ylim=c(0,30), ylab="DBAR", legend.position="topright")
  title ("1-min filter", cex.main=0.75) 
  op <- par (mar=c(5,4,1,1)+0.1)
  if ("DBAR1DC_" %in% VRPlot[[16]]) {
    nm <- names(data)[grepl("DBAR1DC_", names(data))]
    plotWAC(data[, c("Time", nm)])
  }
  AddFooter()
  if (!is.na(Seq) && (Seq == 1)) {return()}
  ## Water measurements:
  op <- par (mar=c(2,4,1,1)+0.1)
  LW <- VRPlot[[16]]
  LW <- LW[which(("PLWC" == substr(LW, 1, 4)) & ("PLWC" != LW))]
  va3 <- vector()
  for (c in LW) {
    nm <- names(data)[grepl(c, names(data))]
    v <- sub("_", "", c)
    data[, v] <- SmoothInterp(data[, nm])
    va3 <- c(va3, v)
  }
  plotWAC (data[, c("Time", va3)], ylim=c(0,1), ylab="PLWCy", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  op <- par (mar=c(2,4,1,1)+0.1)
  if ("PLWC" %in% names(data) && "RICE" %in% names(data)) {
    plotWAC (data[, c("Time", "PLWC", "RICE")], ylim=c(0,25), ylab="PLWC (Watts)")
    hline (10); hline (15)
  }
  op <- par (mar=c(5,4,1,1)+0.1)
  if ("PLWC1DC_" %in% VRPlot[[16]]) {
    nm <- names(data)[grepl("PLWC1DC_", names(data))]
    plotWAC(data[, c("Time", nm)])
  }
  AddFooter()
  if (!is.na(Seq) && (Seq == 2)) {return()}
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  if ("TCNTD_" %in% VRPlot[[16]] && "REJDOF_" %in% VRPlot[[16]]) {
    TCNTD <- data[, names(data)[grepl("TCNTD_", names(data))]]
    REJDOF <- data[, names(data)[grepl("REJDOF_", names(data))]]
    DOFACC <- TCNTD / (TCNTD + REJDOF)
    DOFACC <- SmoothInterp (DOFACC)
    plotWAC (data.frame (data$Time, DOFACC), ylab="DOF acceptance fraction")
    hline (0.2, 'red')
  }
  if ("AVGTRNS_" %in% VRPlot[[16]]) {
    AVGTRNS <- data[, names(data)[grepl("AVGTRNS_", names(data))]]
    AVT <- SmoothInterp (AVGTRNS)
    plotWAC (data.frame (data$Time, AVT), ylim=c(0, 2))
  }
  op <- par (mar=c(5,4,1,1)+0.1)
  if ("CDPLSRP_" %in% VRPlot[[16]]) {
    CDPLSRP <- data[, names(data)[grepl("CDPLSRP_", names(data))]]
    op <- par (mar=c(5,4,1,1)+0.1)
    plotWAC (data.frame(data$Time, CDPLSRP), ylab="CDP laser power", ylim=c(0,4))
  }
  AddFooter ()
#   nm7 <- names(data)[grepl("REJDOF_", names(data))]
#   REJDOF <- data[, nm7]
#   nm8 <- names(data)[grepl("AVGTRNS_", names(data))]
#   AVGTRNS <- data[, nm8]
#   nm9 <- names(data)[grepl("CDPLSRP_", names(data))]
#   CDPLSRP <- data[, nm9]
    hline(3, 'red')
# 
#   # DBAR:
#   op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
#   DBARD <- SmoothInterp(DBARD)
#   plotWAC (data.frame (data$Time, DBARD), ylim=c(0,30), ylab="DBAR", legend.position="topright")
#   title ("1-min filter", cex.main=0.75)
#   #DF <- data[, c("Time", "DBAR3_RPO", "DBARP_RWOOP")]
#   # DF <- data[, c("Time", "DBARP_RWOOP", "DBARU_RWOOU")]
#   #DF$DBAR3_RPO <- SmoothInterp(data$DBAR3_RPO)
#   # DF$DBARP_RWOOP <- SmoothInterp(data$DBARP_RWOOP)
# 
#   op <- par (mar=c(5,4,1,1)+0.1)
#   DBAR1DC <- SmoothInterp(DBAR1DC)
#   plotWAC (data.frame (data$Time, DBAR1DC))
#   title ("1-min filter", cex.main=0.75)
#   AddFooter ()
#   # PLWC:
#   op <- par (mar=c(2,4,1,1)+0.1)
#   PLWCD <- SmoothInterp(PLWCD)
#   PLWCC <- SmoothInterp(data$PLWCC)
#   plotWAC (data.frame(data$Time, PLWCD, PLWCC), ylim=c(0,1), ylab="PLWCy", legend.position="topright")
#   title ("1-min filter", cex.main=0.75)
#   
#   op <- par (mar=c(5,4,1,1)+0.1)
#   PLWC1DC <- SmoothInterp(PLWC1DC)
#   plotWAC (data.frame(data$Time, PLWC1DC), ylim=c(0,1))
#   title ("1-min filter", cex.main=0.75)
#   AddFooter ()
#   
#   # CDP housekeeping
#   layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
#   op <- par (mar=c(2,4,1,1)+0.1)
#   DOFACC <- TCNTD / (TCNTD + REJDOF)
#   DOFACC <- SmoothInterp (DOFACC)
#   plotWAC (data.frame (data$Time, DOFACC), ylab="DOF acceptance fraction")
#   hline (0.2, 'red')
#   AVT <- SmoothInterp (AVGTRNS)
#   plotWAC (data.frame (data$Time, AVT), ylim=c(0, 2))
#   op <- par (mar=c(5,4,1,1)+0.1)
#   plotWAC (data.frame(data$Time, CDPLSRP), ylab="CDP laser power", ylim=c(0,4))
#   AddFooter ()
}

