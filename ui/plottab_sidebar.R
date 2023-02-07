fluidRow(
  column(12, "",
         h4('Plot settings'),
  ),
  column(12, "",
         fluidRow(
           column(6, "",
                  radioButtons("plotctrl", 'Control', 
                               choices = list("Show Control" = 1, "Don't Show Control" = 2), 
                               selected = 1),
                  radioButtons("plotlog", 'log scale',
                               choices = list("original" = 1, "log" = 2), 
                               selected = 1)
           ),
           column(6, "",
                  radioButtons("ploterr", 'Error bars',
                               choices = list("min-max" = 1, "STD" = 2), 
                               selected = 1),
                  radioButtons("plotgrp", 'Group by', 
                               choices = list("Condition" = 1, "Cell type" = 2), 
                               selected = 1)
           ),
           # actionButton('makeplotb', 'Make Plot')
         )
  ),
  column(12, "",
         hr(style = "border: 1px solid #aaaaaa;")
  ),
  column(12, "",
    h4('Download Results'),
  ),
  column(12, "",
         fluidRow(
           column(6, "",
                  downloadButton("download_tab", label = 'Processed data'),
                  br(), " ", br()
           ),
           column(6, "",
                  " "
           ),
         )
         # fluidRow(
         #    downloadButton("download_tab", label = 'Processed data'),
         #    br(), " ", br()
         #  )
  ),
  column(12, "",
         hr(style = "border: 1px solid #aaaaaa;")
  ),
  column(12, "",
         h4('Download Figure'),
  ),
  column(12, "",
         fluidRow(
           column(6, "",
            numericInput('width', 'Width', 4, min = 4, width = '50%'),
            selectInput("pltfrmt", "Select output format", c('png', 'pdf')),
           ),
           column(6, "",
            numericInput('height', 'Height', 4, min = 4, width = '50%'),
            strong('Download'),br(),
            downloadButton("download_plt", label = 'Plot')
           ),
         )
  )
  # br(), hr(style = "border-top: 10px solid #000000;"), br(), 
  
  # shinydashboard::box(title = "Download", status = "primary", 
  #                     background = 'blue',
  #                     solidHeader = TRUE, # collapsible = TRUE, 
  #                     downloadButton("download_tab", label = 'Processed data'),
  #                     downloadButton("download_plt", label = 'Plot'))
)
