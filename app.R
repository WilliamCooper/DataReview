library(shiny)
library(Ranadu)


nplots <- c(1, 3:17, 19:20, 22:23,30)    # project default
psq <- c(1,1, 1,2, 3,1, 4,1, 5,1, 5,2, 5,3, 5,4, 6,1, 7,1, 7,2,
         8,1, 9,1, 9,2, 10,1, 10,2, 11,1, 12,1, 13,1, 15,1, 15,2,
         16,1, 16,2, 16,3, 17,1, 19,1, 19,2, 
         20,1, 20,2, 20,3, 20,4, 21,1, 22,1, 22,2, 22,3, 22,4,
         23,1, 23,2, 23,3, 23,4, 30,1, 30,2)
dim(psq) <- c(2,42)

# functions used later:
hline <- function(y, col='black', lwd=1, lty=2) {
  abline(h=y, col=col, lwd=lwd, lty=lty)
}

formatTime <- function (time) {
  t <- as.POSIXlt (time)
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, t$sec)
  return (tt)
}

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


ui <- fluidPage (
  # titlePanel (tags$h1 ('Data Review')),
  fluidRow ( 
    column (2, wellPanel (selectInput (inputId='Project', label='Project', 
               choices=c('DC3','START08','CONTRAST', 'MPEX'), width='100px'))),
    column (2, wellPanel (numericInput (inputId='Flight', label='Flight', value=6, min=1, max=50, 
              step=1, width='70px'))),
    column (2, wellPanel (numericInput (inputId='plot', label='plot', value=1, min=1, max=42, 
               step=1, width='70px'))),
    column(4, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    ))),
#     column (4, sliderInput (inputId='times', label='time range', 
#                             min=as.POSIXct (0, origin='2012-05-29', tz='UTC'), 
#                             max=as.POSIXct (86400+10*3600, origin='2012-05-29', tz='UTC'), 
#                             value=c(as.POSIXct(75600, origin='2012-05-29', tz='UTC'),
#                                     as.POSIXct(79200, origin='2012-05-29', tz='UTC')),
#                             step=300, timezone='+0000'))),
  sidebarLayout (sidebarPanel(width=2, 
                              textOutput ('M1'),
                              selectInput ('Rplot', label='plot class',
                                           choices=c('track','temperature','humidity',
                                                     'pressure',
                                                     'wind',
                                                     'particles',
                                                     'skew-T',
                                                     'potential T',
                                                     'CDP',
                                                     'radiation',
                                                     'UHSAS/PCASP',
                                                     '2DC', 
                                                     'air chemistry'))),
                 mainPanel(plotOutput (outputId='display')))
)

server <- function (input, output, session) {
  
  observe({
    vp <- switch (input$Rplot,
      'track' = 1,
      'temperature' = 3,
      'humidity' = 5,
      'pressure' = 9,
      'wind' = 13,
      'particles' = 20,
      'skew-T' = 25,
      'potential T' = 26,
      'CDP' = 28,
      'radiation' = 32, 
      'UHSAS/PCASP' = 33,
      '2DC' = 37,
      'air chemistry' = 41
    )
    updateNumericInput (session, 'plot', value=vp)
  })
  
  data <- reactive({
    Project <<- input$Project
    source ('~/RStudio/Reprocessing/Configuration.R')
    VarList <- vector()
    for (i in 1:length(VRPlot)) {
      for (j in 1:length (VRPlot[[i]])) {
        VarList <- c(VarList, VRPlot[[i]][j])
      }
    }
    ## these are needed for translation to new cal coefficients
    ## VarList <- c(VarList, "RTH1", "RTH2", "RTF1")
    fname <<- sprintf ('%s%s/%srf%02d.nc', DataDirectory (), input$Project, 
                      input$Project, input$Flight)
    getNetCDF (fname, VarList)
  })
#   uiOutput(outputId='timey', inline=FALSE, container=div,
#   renderUI (
#     sliderInput (inputId='timex', label='time range', 
#                  min=as.POSIXct (0, origin='2012-05-29', tz='UTC'), 
#                  max=as.POSIXct (86400+10*3600, origin='2012-05-29', tz='UTC'), 
#                  value=c(as.POSIXct(70000, origin='2012-05-29', tz='UTC'),
#                          as.POSIXct(79200, origin='2012-05-29', tz='UTC')),
#                  step=300, timezone='+0000'),
#                  env=parent.frame()
#   )
#   )
  output$M1 <- renderText ({
    switch (psq[1, input$plot],
    'Track Plot and z-t',
    'Track Plot and z-t',
    'Temperature history',
    'Temp. scatterplots',
    'Humidity plots',
    'pressure',
    'dynamic p/TAS/M',
    'total pressure',
    'wind',
    'Schuler/comp f.',
    'AKRD/SSRD',
    'IRU comparison',
    'more IRU',
    'plot not available',
    'concentrations',
    'dbar/lwc/housek.',
    'skew-T diagram',
    'plot not available',
    'potential T',
    'CDP',
    'radiation',
    'UHSAS/PCASP',
    '2DC (1D sizes)', ' ', ' ', ' ', ' ', ' ', ' ',
    'air chemistry'
    )
  })
  
  output$ui <- renderUI({
    sliderInput("times", "Time Range",
                min=data()$Time[1], max=data()$Time[nrow(data())], 
#                 max=as.POSIXct (86400+10*3600, origin='2012-05-29', tz='UTC'),
                value=c(data()$Time[1], data()$Time[nrow(data())]),
#                 value=c(as.POSIXct(70000, origin='2012-05-29', tz='UTC'),
#                         as.POSIXct(79200, origin='2012-05-29', tz='UTC')),
                step=300, timezone='+0000')
  }) 

  output$display <- renderPlot ({
    ## get variables needed for this project:
    Data <- data()
    SE <- getStartEnd (Data$Time)
    i <- getIndex (Data$Time, SE[1])
    FigFooter=sprintf("%s RF%02d %s %s-%s UTC,",Project,input$Flight,strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),strftime(Data$Time[getIndex(Data$Time,SE[2])], format="%H:%M:%S", tz='UTC'))
    FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
    AddFooter <<- function() {
      CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
      mtext(paste(FigFooter,'generated by ', CallingFunction, FigDatestr),1,outer=T,cex=0.75)
      
    }
    
    namesV <- names(Data)
    namesV <- namesV[namesV != "Time"]
    for (n in namesV) {
      Data[!is.na(Data[ ,n]) & abs(Data[,n]+32767) < 1, n] <- NA
    }
    Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
    DataV <- Data

    # DataV <- Data[setRange(Data$Time, StartTime, EndTime), ]
    namesV <- names(DataV)
    namesV <- namesV[namesV != "Time"]
    t <- !is.na (DataV$TASX) & (DataV$TASX < 110)
    DataV[t, namesV] <- NA
    ## guard against inf. VCSEL limits, as for rf08
    if (min(DataV$DP_VXL, na.rm=TRUE) == Inf) {
      DataV$DP_VXL <- rep(0, nrow(DataV))
    }
    if (min(DataV$DP_DPR, na.rm=TRUE) == Inf) {
      DataV$DP_DPR <- rep(0, nrow(DataV))
    }
    if (min(DataV$DP_DPL, na.rm=TRUE) == Inf) {
      DataV$DP_DPL <- rep(0, nrow(DataV))
    }
    DataV$DPXC[DataV$DPXC < -1000] <- NA
    if (psq[1, input$plot] %in% c(20, 22, 23)) {
      t1 <- input$times[1]
      # print (class(t1))
      t <- as.POSIXlt (t1)
      # print (class(t))
      StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
      # print (StartTime)
    }
    eval(parse(text=sprintf("RPlot%d(Data, Seq=%d)", 
                            psq[1, input$plot], psq[2, input$plot])))
    }, width=800, height=640)
}

shinyApp(ui=ui, server=server)
