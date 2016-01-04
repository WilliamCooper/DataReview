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
    column (2, wellPanel (numericInput (inputId='Flight', label='Flight', value=1, 
                                        min=1, max=50, step=1, width='70px'))),
    column (2, wellPanel (numericInput (inputId='plot', label='plot', value=1, 
                                        min=1, max=49, step=1, width='70px'))),
    column(4, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui"))),
    column(2, wellPanel (actionButton (inputId='savePDF', 'save PDF'),
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
                              uiOutput("ui2")),
                 mainPanel( tabsetPanel (tabPanel ('plot', plotOutput (outputId='display')),
                                         tabPanel ('stats', dataTableOutput ('stats')),
                                         tabPanel ('listing', dataTableOutput ('listing')))))
)