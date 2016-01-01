library(shiny)
library(Ranadu)



ui <- fluidPage (
  # titlePanel (tags$h1 ('Data Review')),
  fluidRow ( 
    column (2, wellPanel (
      selectInput (inputId='Project', label=NULL, 
                   choices=c('PACDEX', 'DC3', 'ORCAS', 'START08','CONTRAST', 'MPEX'), width='100px'),
                   actionButton ('reconfigure', 'save config'))
      ),
    column (2, wellPanel (numericInput (inputId='Flight', label='Flight', value=10, min=1, max=50, 
              step=1, width='70px'))),
    column (2, wellPanel (numericInput (inputId='plot', label='plot', value=1, min=1, max=42, 
               step=1, width='70px'))),
    column(4, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui"))),
    column(2, wellPanel (actionButton ('savePDF', 'save PDF'),
                         actionButton ('addVar', 'add var')))),

  sidebarLayout (sidebarPanel(width=2, 
                              textOutput ('M1'),
                              selectInput ('Rplot', label='plot class',
                                           selectize=FALSE, size=13,
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
                                                     'air chemistry')),
                              uiOutput("ui2")),
                 mainPanel( tabsetPanel (tabPanel ('plot', plotOutput (outputId='display')),
                                         tabPanel ('stats', dataTableOutput ('stats')),
                                         tabPanel ('listing', dataTableOutput ('listing')))))
)