
RPlot24 <- function(data, ...) {
  ## be sure that the variables are in 'VarList'. If not, where VarList
  ## is defined, add the new variable to the variable names or follow the
  ## present definition with VarList <- c(VarList, "NewVariable1", "NewVariable2")
  # use two-pane layout
  # layout(matrix(1:1, ncol = 1), widths = 1, heights = c(5,6))
  # op <- par (mar=c(5,4,1,2.5)+0.1,oma=c(1.1,0,0,0))
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  plotWAC (data[, c("Time", VRPlot$PV24[1])])
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", VRPlot$PV24[2])])
  AddFooter ()
}
