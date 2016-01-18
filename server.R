
server <- function (input, output, session) {
  
  ## if this is set TRUE then messages will print in the console
  ## indicating which functions are entered, to trace the sequence
  ## of interactions when window entries are changed.
  Trace <- FALSE
  
  ################ OBSERVERS ########################
  
  observe ({                              ## PlotVar
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
    if ((length(data ()) < 2) || any (!(PVar %in% names (data ())))) {
      print ('need new data')
      reac$newdata <- TRUE
    }
    jp <- psq[1, np]
    ## need to change VRPlot to have the specified variables
    if (Trace) {
      print (sprintf ('redefined global VRPlot[[%d]]', jp))
      print (PVar)
    }
    # reac$newdisplay <- TRUE
    VRPlot[[jp]] <<- PVar
  }, priority=-5)
  
  observe({                             ## Rplot
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
  
  observeEvent (input$reconfigure, saveConfig ())
  observeEvent (input$savePDF,
                savePDF (Data=data(), inp=input))
  observeEvent (input$savePNG,
                savePNG (Data=data(), inp=input))
  observeEvent (input$saveRdata,
                saveRdata (Data=data(), inp=input))
  observeEvent (input$ncplot, OpenInProgram (data(), warnOverwrite=FALSE))
  observeEvent (input$Xanadu, OpenInProgram (data(), 'Xanadu', warnOverwrite=FALSE))
  observeEvent (input$maneuvers, SeekManeuvers (data ()))
  observeEvent (input$manual, seeManual ())
  
  observe ({                              ## typeFlight
    if (Trace) {print (sprintf ('entered typeFlight observer with value %s', input$typeFlight))}
    typeFlight <<- input$typeFlight
    reac$newdata <- TRUE
  })
  
  observe ({                              ## VRP
    if (Trace) {print (sprintf ('entered VRP, Project=%s %s',
                                input$Project, Project))}
    if (input$Project != Project) {
      Project <<- Project <- input$Project
      if (Trace) {print (sprintf ('set new project: %s', Project))}
      typeFlight <<- flightType ()
      
      if (grepl ('HIPPO', Project)) {
        fn <- sprintf ('%sHIPPO/%srf01.nc', DataDirectory (), Project)
      } else {
        fn <- sprintf ('%s%s/%srf01.nc', DataDirectory (), Project, Project)
      }
      if (!file.exists (fn)) {
        fn <- sub ('\\.nc', '.Rdata', fn)
      }
      if (!file.exists (fn)) {
        if (grepl ('HIPPO', Project)) {
          fn <- sprintf ('%sHIPPO/%stf01.nc', DataDirectory (), Project)
        } else {
          fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), Project, Project)
        }
      }
      if (!file.exists (fn)) {
        fn <- sub ('\\.nc', '.Rdata', fn)
      }
      if (!file.exists (fn)) {
        warning ('need tf01 or rf01 to initialize')
      } else {
        FI <<- DataFileInfo (fn)
        if (Trace) {print (sprintf ('using file %s for FI', fn))}
      }
      VRPlot <<- loadVRPlot (Project, FALSE, 1, psq)
    }
  }, priority=10)
  
  observe ({                             ## time
    if (Trace) {print ('setting time')}
    if (Trace) {print (sprintf ('Project and Flight: %s %s%02d',
                                isolate(input$Project),
                                isolate (input$typeFlight),
                                isolate (input$Flight)))}
    #     reac$newdisplay
    #     reac$newdisplay <- TRUE
    Data <- data ()
    if (length (Data) < 2) {
      reac$newdata <- TRUE
      if (Trace) {print ('error, need data first')}
      return ()
    }
    step <- 60
    minT <- Data$Time[1]
    minT <- minT - as.integer (minT) %% step
    maxT <- Data$Time[nrow(Data)]
    maxT <- maxT - as.integer (maxT) %% step + step
    if (Trace) {print (sprintf ('slider values %s %s', formatTime (minT),
                                formatTime (maxT)))}
    updateSliderInput(session, inputId='times', label=NULL,
                      value=c(minT, maxT),
                      min=minT, max=maxT)
    times <<- c(minT, maxT)
  }, priority=0)
  
  
  observe ({                          ## global time
    if (Trace) {print ('entering global-time observer')}
    times <<- input$times
  })
  
  ################ REACTIVES ########################
  
  reac <- reactiveValues (newdata=FALSE, newdisplay=FALSE)
  
  flightType <- reactive ({              ## typeFlight
    ## reset typeFlight to rf
    updateRadioButtons (session, 'typeFlight', label=NULL, selected='rf')
    'rf'
  })
  
  data <- reactive({                     ## data
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
    VarList <- unique (VarList)
    VarList <- VarList[!is.na(VarList)]
    VarList <<- VarList  ## just saving for outside-app use
    ## these would be needed for translation to new cal coefficients
    ## VarList <- c(VarList, "RTH1", "RTH2", "RTF1")
    if (grepl ('HIPPO', input$Project)) {
      fname <<- sprintf ('%sHIPPO/%s%s%02d.nc', DataDirectory (), input$Project,
                         typeFlight, input$Flight)
    } else {
      fname <<- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), input$Project,
                         input$Project, typeFlight, input$Flight)
    }
    #     if (input$Production) {
    #       print (sprintf ('Production section, input$Production=%d', input$Production))
    #       dr <- sprintf ('%s../raf/Prod_Data/%s', DataDirectory (), Project)
    #       scmd <- sprintf ('ls -lt `/bin/find %s -ipath "\\./movies" -prune -o -ipath "\\./*image*" -prune -o -name %s%s%02d.nc`',
    #                        dr, Project, input$typeFlight, input$Flight)
    #       fl <- system (scmd, intern=TRUE)[1]
    #       if ((length (fl) > 0) && (!grepl ('total', fl))) {
    #         fname <- sub ('.* /', '/', fl[1])
    #       }
    #       scmd <- sub ('\\.nc', '.Rdata', scmd)
    #       fl <- system (scmd, intern=TRUE)[1]
    #       if ((length (fl) > 0) && (!grepl ('total', fl))) {
    #         fname <- sub ('.* /', '/', fl[1])
    #       }
    #     }
    if (Trace) {print (sprintf ('in data, fname=%s', fname))}
    reac$newdisplay <- TRUE
    if (file.exists(fname)) {
      D <- getNetCDF (fname, VarList)
      if (length (D) > 1) {
        fname.last <<- fname
        return (D)
      } else {
        print (sprintf ('fname=%s', fname))
        print (VarList)
        ## stopping to prevent looping
        stop ('variable not found; stopping to avoid looping')
      }
    } else {
      warning (sprintf ('the file %s does not exist', fname))
      fnRdata <- sub ('\\.nc', '.Rdata', fname)
      if (file.exists (fnRdata)) {
        warning ('using Rdata file instead')
        fl <- load (file=fnRdata)
        FI <<- DataFileInfo (fnRdata)
        loadVRPlot (Project, Production=FALSE, input$Flight, psq)
        fname.last <<- fname
        # print (sprintf ('data returned with dimensions %d', dim(Data)))
        return (Data)
      }
      ## try tf01
      fn <- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), input$Project,
                     input$Project, 'tf', input$Flight)
      if (file.exists (fn)) {
        warning (sprintf ('switched to tf%02d because rf%02d does not exist',
                          input$Flight, input$Flight))
        updateRadioButtons (session, 'typeFlight', label=NULL, selected='tf')
        typeFlight <<- 'tf'
        return (getNetCDF (fn, VarList))
      } else {
        if (Trace) {print ('error in data, returning -1')}
        return (-1)
      }
    }
  })
  
  
  ################ OUTPUTS ########################
  
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
  
  output$display <- renderPlot ({  ## display
    # input$typeFlight
    if (is.null(input$times[1])) {
      if (Trace) {print ('in display but input time is NULL')}
      return ()
    }
    if (Trace) {
      print ('display entry, reac$newdisplay is:')
      print (reac$newdisplay)
    }
    if (reac$newdisplay) {
      input$PlotVar
      Project <- input$Project
      # VRPlot <- VRP ()
      if (Trace) {print ('entered display')}
      # VRPlot <<- VRPlot
      Data <- data()
      if (length (Data) < 2) {
        warning (sprintf ('variable error in (%s)', fname))
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
        return ()
      }
      
      if (Trace) {
        print (sprintf ('input$times %s %s', formatTime (input$times[1]),
                        formatTime (input$times[2])))
        print (sprintf ('global times are %s %s',
                        formatTime (times[1]), formatTime (times[2])))
      }
      namesV <- names(Data)
      namesV <- namesV[namesV != "Time"]
      for (n in namesV) {
        Data[!is.na(Data[ ,n]) & (abs(Data[,n]+32767) < 1), n] <- NA
      }
      # Data <- Data[(Data$Time > input$times[1]) & (Data$Time < input$times[2]), ]
      Data <- Data[(Data$Time > times[1]) & (Data$Time < times[2]), ]
      if (nrow (Data) <= 0) {
        plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
        text (0.5, 0.8, sprintf ('loading requested data file (%s)', fname))
        reac$newdisplay <- TRUE
        reac#newdata <- TRUE
        return()
      }
      ## see global.R functions:
      DataV <- limitData (Data, input)
      ndv <- names (DataV)
      ## guard against inf. VCSEL limits
      if (('DP_VXL' %in% ndv) && all(is.na(DataV$DP_VXL))) {
        DataV$DP_VXL <- rep(0, nrow(DataV))
      }
      if (('DP_DPR' %in% ndv) && all(is.na(DataV$DP_DPR))) {
        DataV$DP_DPR <- rep(0, nrow(DataV))
      }
      if (('DP_DPL' %in% ndv) && all(is.na(DataV$DP_DPL))) {
        DataV$DP_DPL <- rep(0, nrow(DataV))
      }
      DataV$DPXC[!is.na(DataV$DPXC) & (DataV$DPXC < -1000)] <- NA
      if (psq[1, input$plot] %in% c(20:22)) {
        t1 <- times[1]    #input$times[1]
        # print (class(t1))
        t <- as.POSIXlt (t1, tz='UTC')
        # print (class(t))
        StartTime <<- as.integer (10000*t$hour+100*t$min+t$sec)
        # print (StartTime)
      }
      #       if (fname != fname.last) {
      #         warning (sprintf ('requested data file (%s) not found', fname))
      #         plot (0,0, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, ann=FALSE)
      #         text (0.5, 0.8, sprintf ('requested data file (%s) not found', fname))
      #       } else {
      # if (Trace) {print (str(Data))}
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
      if (input$limits) {
        eval(parse(text=sprintf("RPlot%d(DataV, Seq=%d)",
                                psq[1, input$plot], psq[2, input$plot])))
      } else {
        eval(parse(text=sprintf("RPlot%d(Data, Seq=%d)",
                                psq[1, input$plot], psq[2, input$plot])))
      }
      # }
      #       si <- input$plot
      #       updateSelectInput (session, 'Rplot', selected=st[si])
      if (Trace) {print ('finished display')}
    }
  }, width=920, height=680)
  
  output$stats <- renderDataTable ({
    if (Trace) {print ('entered stats')}
    input$times
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]])]
    Ds <- Ds[(Ds$Time >= times[1]) & (Ds$Time <= times[2]), ]
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
  
  output$hist <- renderPlot ({
    input$PlotVar
    input$times
    layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(8,8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    if (Trace) {print ('entered hist')}
    Ds <- limitData (data(), input)
    # Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]])]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      hist (Ds[ ,nm], freq=FALSE, breaks=50, xlab=nm, 
            ylab='probability density', main=NA)
    }
  }, width=920, height=680)
  
  output$barWvsZ <- renderPlot ({
    if (Trace) {print ('entered barXvsZ')}
    input$PlotVar
    input$times
    layout (matrix(1:6, ncol=3), widths=c(5,5,5), heights=c(8,8))
    op <- par (mar=c(5.2,5,1,1)+0.1,oma=c(1.1,0,0,0))
    Ds <- limitData (data(), input)
    
    Ds <- Ds[, c('Time', VRPlot[[psq[1, input$plot]]], 'GGALT')]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    Ds <- Ds[!is.na (Ds$GGALT), ]
    kount <- 0
    for (nm in names (Ds)) {
      if (nm == 'Time') {next}
      if (nm == 'GGALT') {next}
      kount <- kount + 1
      if (kount > 6) {break}
      DB <- data.frame ('Z1'=Ds[, nm])
      DB[Ds$GGALT > 1000, 'Z1'] <- NA
      for (j in 2:15) {
        zmax <- j*1000
        zmin <- zmax-1000
        V <- sprintf ('Z%d', j)
        DB[,V] <- Ds[, nm]
        DB[(Ds$GGALT < zmin) | (Ds$GGALT > zmax), V] <- NA
      }
      boxplot (DB, horizontal=TRUE, outline=TRUE, 
               xlab=nm, ylab='altitude [km]', names=NULL)
    }
  }, width=920, height=680)
  
  output$listing <- renderDataTable ({
    if (Trace) {print ('entered listing')}
    Ds <- limitData (data(), input)
    Ds <- Ds[, c('Time', slp[[input$plot]])]
    Ds <- Ds[(Ds$Time > times[1]) & (Ds$Time < times[2]), ]
    Ds
  }, options=list(paging=TRUE, searching=TRUE))
  
  outputOptions (output, 'display', priority=-10)
  outputOptions (output, 'stats', priority=-10)
  outputOptions (output, 'listing', priority=-10)
  outputOptions (output, 'hist', priority=-10)
  outputOptions (output, 'barWvsZ', priority=-10)
}



