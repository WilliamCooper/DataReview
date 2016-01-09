
server <- function (input, output, session) {
  
  ## if this is set TRUE then messages will print in the console
  ## indicating which functions are entered, to trace the sequence
  ## of interactions when window entries are changed.
  Trace <- FALSE
  
  output$ui2 <- renderUI ({
    #     print (sprintf ('entered ui2 with plot %d', input$plot))
    #     print (chp[[input$plot]]);print (slp[[input$plot]])
    if (Trace) {print (sprintf ('entered ui2 with plot %d', input$plot))}
    if (input$addVar != 'add var') {
      chp[[input$plot]] <<- c(chp[[input$plot]],input$addVar)
    }
    PVar <<- slp[[input$plot]]
    updateSelectInput (session, 'addVar', selected='add var')
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
    ## need to change VRPlot to have the specified variables
    if (Trace) {
      print (sprintf ('redefined global VRPlot[[%d]]', jp))
      print (PVar)
    }
    VRPlot[[jp]] <<- PVar  
  })
  
  #   st <- c('track', 'track',
  #           'temperature', 'temperature',
  #           rep('humidity', 4),
  #           rep('pressure', 4),
  #           rep('wind', 7),
  #           'radiation',
  #           rep('particles', 5),
  #           'skew-T',
  #           rep('potential T',2),
  #           rep('CDP',4),
  #           rep('UHSAS/PCASP',4),
  #           rep('2DC', 4),
  #           rep('air chemistry',2),
  #           rep('extras',2))
  
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
  observeEvent (input$savePNG, 
                savePNG (Data=data(), inp=input))
  ##                handler.env=.GlobalEnv), 
  observeEvent (input$ncplot, OpenInProgram (data(), warnOverwrite=FALSE))
  observeEvent (input$Xanadu, OpenInProgram (data(), 'Xanadu', warnOverwrite=FALSE))
  observeEvent (input$maneuvers, SeekManeuvers (data ()))
  
  VRP <- reactive ({
    if (Trace) {print (sprintf ('entered VRP, Project=%s %s', 
                                input$Project, Project))}
    if (input$Project != Project) {
      Project <<- Project <- input$Project
      print (sprintf ('set new project: %s', Project))
      VRPlot <<- loadVRPlot (Project, psq)
      if (grepl ('HIPPO', Project)) {
        fn <- sprintf ('%sHIPPO/%srf01.nc', DataDirectory (), Project)
      } else {
        fn <- sprintf ('%s%s/%srf01.nc', DataDirectory (), Project, Project)
      }
      if (!file.exists (fn)) {
        if (grepl ('HIPPO', Project)) {
          fn <- sprintf ('%sHIPPO/%stf01.nc', DataDirectory (), Project)
        } else {
          fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), Project, Project)
        }
      }
      if (!file.exists (fn)) {
        warning ('need tf01 or rf01 to initialize')
      } else {
        FI <<- DataFileInfo (fn)
      }
    }
  })
  
  data <- reactive({
    if (Trace) {print ('entered data')}
    # Project <<- Project <- input$Project
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
    ## these would be needed for translation to new cal coefficients
    ## VarList <- c(VarList, "RTH1", "RTH2", "RTF1")
    if (grepl ('HIPPO', input$Project)) {
      fname <<- sprintf ('%sHIPPO/%s%s%02d.nc', DataDirectory (), input$Project, 
                         input$typeFlight, input$Flight)
    } else {
      fname <<- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), input$Project, 
                         input$Project, input$typeFlight, input$Flight)
    }
    if (file.exists(fname)) {
      fname.last <<- fname
      return (getNetCDF (fname, VarList))
    } else {
      warning (sprintf ('the file %s does not exist', fname))
      if (exists ('fname.last')) {
        return (getNetCDF (fname.last, VarList))
      } else {
        ## try tf01
        fname.last <<- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), input$Project, 
                                input$Project, 'tf', input$Flight)
        if (file.exists (fname.last)) {
          warning (sprintf ('switched to tf%02d because rf%02d does not exist', 
                   input$Flight, input$Flight))
          updateRadioButtons (session, 'typeFlight', label=NULL, selected='tf')
          return (getNetCDF (fname.last, VarList))
        } else {
          return ()
        }
      }
    }
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
            'air chemistry',
            'extra2',
            'extra'
    )
  })
  
  output$ui <- renderUI({
    if (Trace) {print ('setting time')}
    step <- 60
    minT <- data()$Time[1]
    minT <- minT - as.integer (minT) %% step
    maxT <- data()$Time[nrow(data())]
    maxT <- maxT - as.integer (maxT) %% step + step
    sliderInput("times", label=NA,
                min=minT, max=maxT, 
                #                 max=as.POSIXct (86400+10*3600, origin='2012-05-29', tz='UTC'),
                value=c(data()$Time[1], data()$Time[nrow(data())]),
                #                 value=c(as.POSIXct(70000, origin='2012-05-29', tz='UTC'),
                #                         as.POSIXct(79200, origin='2012-05-29', tz='UTC')),
                animate=TRUE,
                step=step,  timeFormat='%T', dragRange=TRUE, timezone='+0000')
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
      FigFooter=sprintf("%s %s%02d %s %s-%s UTC,", Project, input$typeFlight, 
                        input$Flight, strftime(Data$Time[i], format="%Y-%m-%d", tz='UTC'),
                        strftime(Data$Time[i], format="%H:%M:%S", tz='UTC'),
                        strftime(Data$Time[getIndex(Data$Time,SE[2])], 
                                 format="%H:%M:%S", tz='UTC'))
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
      if (all(is.na(DataV$DP_VXL))) {
        DataV$DP_VXL <- rep(0, nrow(DataV))
      }
      if (all(is.na(DataV$DP_DPR))) {
        DataV$DP_DPR <- rep(0, nrow(DataV))
      }
      if (all(is.na(DataV$DP_DPL))) {
        DataV$DP_DPL <- rep(0, nrow(DataV))
      }
      DataV$DPXC[!is.na(DataV$DPXC) & (DataV$DPXC < -1000)] <- NA
      if (psq[1, input$plot] %in% c(20:22)) {
        t1 <- input$times[1]
        # print (class(t1))
        t <- as.POSIXlt (t1)
        # print (class(t))
        StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
        # print (StartTime)
      }
      if (fname != fname.last) {
        warning (sprintf ('requested data file (%s) not found', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
      } else {
        if (input$limits) {
          eval(parse(text=sprintf("RPlot%d(DataV, Seq=%d)", 
                                  psq[1, input$plot], psq[2, input$plot])))
        } else {
          eval(parse(text=sprintf("RPlot%d(Data, Seq=%d)", 
                                  psq[1, input$plot], psq[2, input$plot])))
        }
      }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      if (Trace) {print ('finished display')}
    }
  }, width=920, height=680)
  
  output$stats <- renderDataTable ({
    if (Trace) {print ('entered stats')}
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]])]
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



