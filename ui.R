library(shiny)
library(Ranadu)

minT <- as.POSIXct(0, origin='2012-05-29', tz='UTC')
maxT <- as.POSIXct(3600*8, origin='2012-05-29', tz='UTC')
step <- 60
ui <- fluidPage (
  # titlePanel (tags$h1 ('Data Review')),
  navlistPanel (tabPanel ('project, flight, and plot', fluidRow (
    column (3, wellPanel (
      selectInput (inputId='Project', label=NULL,
                   choices=PJ, width='100px'),
      actionButton ('reconfigure', 'save config'))
    ),
    column (5, wellPanel (
      fluidRow (
        column (4, numericInput (inputId='Flight', label='Flight', value=1,
                                 min=1, max=99, step=1, width='80px')),
        column (2, radioButtons ('typeFlight', label=NULL, choices=c('rf', 'tf', 'ff'),
                                 width='70px', inline=TRUE)),
        # column (2, checkboxInput ('Production', label='PR')),
        column (4, numericInput (inputId='plot', label='plot', value=1,
                                 min=1, max=49, step=1, width='80px'))))),
    column(4, wellPanel (
      fluidRow (
        column (4, actionButton (inputId='savePDF', label='PDF', icon=icon('file-pdf-o'))),
        column (4, actionButton (inputId='savePNG', label='PNG', icon=icon('file-image-o'))),
        column (4, actionButton (inputId='saveRdata', label='R', icon=icon('file-archive-o')))
      ),
      fluidRow (
        selectInput ('addVar', label=NULL,
                     choices=c('add var',sort(FI$Variables)))))))),
    tabPanel ('time range, restrictions', 
    column(6, wellPanel(
      sliderInput("times", label=NA, min=minT, max=maxT,
                  value=c(minT, maxT),
                  animate=TRUE,
                  step=step,  
                  timeFormat='%T', dragRange=TRUE,
                  timezone='+0000'))),
    column (6, wellPanel ( fluidRow (
      column (3, numericInput ('minTAS', 'tas min', 110, width='90px')),
      column (3, numericInput ('maxROLL', 'roll', 5, width='90px')),
      column (3, numericInput ('minZ', 'Zmin-km', 2, width='90px')),
      column (3, numericInput ('maxROC', 'abs ROC', 5, width='90px')))
    ))), widths=c(3,9)),
  

  sidebarLayout (sidebarPanel(width=3,
                              textOutput ('M1'),
                              selectInput ('Rplot', label='plot class',
                                           selectize=FALSE, size=14,
                                           choices=c('track','temperature','humidity',
                                                     'pressure',
                                                     'wind',
                                                     'radiation',
                                                     'particles',
                                                     'skew-T',
                                                     'potential T',
                                                     'CDP',
                                                     'UHSAS/PCASP',
                                                     '2DC',
                                                     'air chemistry',
                                                     'extras')),
                              checkboxInput ('limits','apply restrictions'),
                              uiOutput("ui2"),
                              actionButton ('ncplot', 'see in ncplot'),
                              actionButton ('Xanadu', 'see in Xanadu'),
                              actionButton ('maneuvers', 'see maneuvers')),
                 mainPanel( tabsetPanel (tabPanel ('plot', plotOutput (outputId='display')),
                                         tabPanel ('stats', dataTableOutput ('stats')),
                                         tabPanel ('histograms', plotOutput (outputId='hist')),
                                         tabPanel ('soundings', plotOutput (outputId='barWvsZ')),
                                         tabPanel ('listing', dataTableOutput ('listing')))))
)