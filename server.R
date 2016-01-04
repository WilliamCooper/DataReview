
server <- function (input, output, session) {
  
  Trace <- FALSE
  
  output$ui2 <- renderUI ({
    #     print (sprintf ('entered ui2 with plot %d', input$plot))
    #     print (chp[[input$plot]]);print (slp[[input$plot]])
    if (Trace) {print (sprintf ('entered ui2 with plot %d', input$plot))}
    if (input$addVar != 'add var') {
      chp[[input$plot]] <<- c(chp[[input$plot]],input$addVar)
    }
    PVar <<- slp[[input$plot]]
    selectInput ('PlotVar', label='variables', selectize=FALSE, multiple=TRUE,
                 choices=chp[[input$plot]], selected=slp[[input$plot]], size=10)
  })
  
  observe ({
    if (Trace) {
      print ('entered observe for PlotVar')
      print (sprintf ('PVar is %s', PVar))
      print (sprintf ('PlotVar is %s', input$PlotVar))
    }
    if (is.na (any (match(PVar, input$PlotVar))) 
        || is.na (any (match(input$PlotVar, PVar)))) {
      reac$newdisplay <- FALSE
    } else {
      reac$newdisplay <- TRUE
    }
    input$plot
    isolate (np <- input$plot)
    if (length (input$PlotVar) < 1) {return()}
    PVar <- input$PlotVar
    if (any (!(PVar %in% names (data ())))) {
      print ('need new data')
      reac$newdata <- TRUE
    }
    jp <- psq[1, np]
    if (Trace) {print (sprintf ('redefined global VRPlot[[%d]]', jp))}
    VRPlot[[jp]] <<- PVar  
  })
  
  observe({
    vp <- switch (input$Rplot,
                  'track' = 1,
                  'temperature' = 3,
                  'humidity' = 5,
                  'pressure' = 9,
                  'wind' = 13,
                  'radiation' = 20,
                  'particles' = 21,
                  'skew-T' = 26,
                  'potential T' = 27,
                  'CDP' = 29,
                  'UHSAS/PCASP' = 33,
                  '2DC' = 37,
                  'air chemistry' = 41,
                  'extras' = 43
    )
    updateNumericInput (session, 'plot', value=vp)
  })
  
  reac <- reactiveValues (newdata=FALSE, newdisplay=FALSE)
  
  observeEvent (input$reconfigure, saveConfig (), handler.env=.GlobalEnv)
  observeEvent (input$savePDF, 
                savePDF (Data=data(), inp=input))
##                handler.env=.GlobalEnv)
  
  VRP <- reactive ({
    if (Trace) {print ('entered VRP')}
    if (input$Project != Project) {
      Project <<- Project <- input$Project
      print (sprintf ('set new project: %s', Project))
      VRPlot <<- loadVRPlot (Project, psq)
      FI <<- DataFileInfo (sprintf ('%s%s/%srf05.nc', 
                                   DataDirectory (), input$Project, input$Project))
    }
  })
  
  data <- reactive({
    if (Trace) {print ('entered data')}
    Project <<- Project <- input$Project
    reac$newdata
    reac$newdata <- FALSE
    VarList <- vector()
    # VRPlot <- VRP ()
    for (i in 1:length(VRPlot)) {
      for (j in 1:length (VRPlot[[i]])) {
        VarList <- c(VarList, VRPlot[[i]][j])
      }
    }
    VarList <<- VarList  ## just saving for outside-app use
    ## these are needed for translation to new cal coefficients
    ## VarList <- c(VarList, "RTH1", "RTH2", "RTF1")
    fname <<- sprintf ('%s%s/%srf%02d.nc', DataDirectory (), input$Project, 
                       input$Project, input$Flight)
    getNetCDF (fname, VarList)
  })
  
  
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
            'radiation',
            'concentrations',
            'dbar/lwc/housek.',
            'skew-T diagram',
            'plot not available',
            'potential T',
            'CDP',
            'UHSAS/PCASP',
            '2DC (1D sizes)', 
            'air chemistry'
    )
  })
  
  output$ui <- renderUI({
    if (Trace) {print ('setting time')}
    sliderInput("times", label=NA,
                min=data()$Time[1], max=data()$Time[nrow(data())], 
                #                 max=as.POSIXct (86400+10*3600, origin='2012-05-29', tz='UTC'),
                value=c(data()$Time[1], data()$Time[nrow(data())]),
                #                 value=c(as.POSIXct(70000, origin='2012-05-29', tz='UTC'),
                #                         as.POSIXct(79200, origin='2012-05-29', tz='UTC')),
                animate=TRUE,
                step=300, round=2, timeFormat='%T', dragRange=TRUE, timezone='+0000')
  }) 
  
  output$display <- renderPlot ({
    if (is.null(input$times[1])) {return ()}
    if (Trace) {
      print ('display entry, reac$newdisplay is:')
      print (reac$newdisplay)
    }
    if (reac$newdisplay) {
      input$PlotVar
      Project <- input$Project
      VRPlot <- VRP ()
      if (Trace) {print ('entered display')}
      # VRPlot <<- VRPlot
      Data <- data()
      SE <- getStartEnd (Data$Time)
      i <- getIndex (Data$Time, SE[1])
      FigFooter=sprintf("%s RF%02d %s %s-%s UTC,",Project,input$Flight,strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),strftime(Data$Time[getIndex(Data$Time,SE[2])], format="%H:%M:%S", tz='UTC'))
      FigDatestr=strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
      AddFooter <<- function() {
        CallingFunction <- sub ("\\(.*\\)", "", deparse (sys.call (-1)))
        mtext(paste(FigFooter,'generated by ', CallingFunction, 
                    FigDatestr),1,outer=T,cex=0.75)
      }
      
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      ## see global.R functions:
      DataV <- limitData (Data, input)
      ## guard against inf. VCSEL limits
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
      if (psq[1, input$plot] %in% c(20:22)) {
        t1 <- input$times[1]
        # print (class(t1))
        t <- as.POSIXlt (t1)
        # print (class(t))
        StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
        # print (StartTime)
      }
      if (input$limits) {
        eval(parse(text=sprintf("RPlot%d(DataV, Seq=%d)", 
                              psq[1, input$plot], psq[2, input$plot])))
      } else {
        eval(parse(text=sprintf("RPlot%d(Data, Seq=%d)", 
                                psq[1, input$plot], psq[2, input$plot])))
      }
      if (Trace) {print ('finished display')}
    }
  }, width=920, height=680)
  
  output$stats <- renderDataTable ({
    if (Trace) {print ('entered stats')}
    Ds <- limitData (data(), input)
    Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[(Ds$Time > input$times[1]) & (Ds$Time < input$times[2]), ]
    Dstats <- data.frame ()
    Dstats['Time', 1] <- 'Time'
    Dstats['Time', 2] <- NA
    Dstats['Time', 3] <- NA
    Dstats['Time', 4] <- formatTime (Ds$Time[1])
    Dstats['Time', 5] <- formatTime (Ds$Time[nrow(Ds)])
    for (nm in names(Ds)) {
      if (nm == 'Time') {next}
      Dstats[nm, 1] <- nm
      Dstats[nm, 2] <- mean (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 3]   <- sd   (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 4]  <- min  (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 5]  <- max  (Ds[, nm], na.rm=TRUE)
    }
    names(Dstats) <- c('variable', 'mean', 'sd', 'min', 'max')
    row.names (Dstats) <- names(Ds)
    # Dstats[2:nrow(Dstats), 2:5] <- format(Dstats[2:nrow(Dstats),2:5], digits=5, scientific=FALSE)
    for (k in 2:5) {
      Dstats[2:nrow(Dstats), k] <- sprintf('%.3f', as.numeric(Dstats[2:nrow(Dstats), k]))
    }
    if (Trace) {print (str(Dstats))}
    Dstats
  }, options=list(paging=FALSE, searching=FALSE))
  
  output$listing <- renderDataTable ({
    if (Trace) {print ('entered listing')}
    Ds <- limitData (data(), input)
    Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[(Ds$Time > input$times[1]) & (Ds$Time < input$times[2]), ]
    Ds
  }, options=list(paging=TRUE, searching=TRUE))
}



