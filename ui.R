library(shiny)
library(Ranadu)

minT <- as.POSIXct(0, origin='2012-05-29', tz='UTC')
maxT <- as.POSIXct(86400, origin='2012-05-29', tz='UTC')
step <- 60
ui <- fluidPage (
  # titlePanel (tags$h1 ('Data Review')),
  fluidRow ( 
    column (2, wellPanel (
      selectInput (inputId='Project', label=NULL, 
                   choices=PJ, width='100px'),
      actionButton ('reconfigure', 'save config'))
    ),
    column (4, wellPanel (
      fluidRow (
        column (4, numericInput (inputId='Flight', label='Flight', value=1, 
                                 min=1, max=99, step=1, width='80px')),
        column (2, radioButtons ('typeFlight', label=NULL, choices=c('rf', 'tf', 'ff'), 
                                 width='70px', inline=TRUE)),
        column (2, checkboxInput ('production', label='PR')),
        column (4, numericInput (inputId='plot', label='plot', value=1, 
                                 min=1, max=49, step=1, width='80px'))))),
    column(4, wellPanel(
      # This outputs the dynamic UI component
      # uiOutput("ui"))),
      sliderInput("times", label=NA, min=minT, max=maxT,
                  value=c(minT, maxT),
                  animate=TRUE,
                  step=step,  timeFormat='%T', dragRange=TRUE, 
                  timezone='+0000'))),
    column(2, wellPanel (
      fluidRow (
        column (6, actionButton (inputId='savePDF', label=NULL, icon=icon('file-pdf-o'))),
        column (6, actionButton (inputId='savePNG', label=NULL, icon=icon('file-image-o')))
      ),
      selectInput ('addVar', label=NULL, 
                   choices=c('add var',sort(FI$Variables)))))),
  
  sidebarLayout (sidebarPanel(width=2, 
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
                                         tabPanel ('listing', dataTableOutput ('listing')))))
)