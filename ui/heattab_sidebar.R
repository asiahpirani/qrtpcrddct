fluidRow(
  column(12, "",
         h4('Heatmap settings'),
  ),
  column(12, "",
         fluidRow(
           column(6, "",
                  radioButtons("heatctrl", 'Control', 
                               choices = list("Don't Show Control" = 2, "Show Control" = 1), 
                               selected = 2),
                  radioButtons("heatlog", 'log scale',
                               choices = list("original" = 1, "log" = 2), 
                               selected = 1)
           ),
           column(
             6, "",
             radioButtons(
               "heatori",
               "Heatmap orientation",
               choices = c(
                 "Groups as rows" = 1,
                 "Genes as rows" = 2
               ),
               selected = 1
             )
           ),
           # actionButton('makeplotb', 'Make Plot')
         )
  ),
  column(12, "",
         hr(style = "border: 1px solid #aaaaaa;")
  ),
  column(12, "",
         h4('Download Heatmap'),
  ),
  column(12, "",
         fluidRow(
           column(6, "",
                  numericInput('heatwidth', 'Width', 4, min = 4, width = '50%'),
                  selectInput("heatfrmt", "Select output format", c('png', 'pdf')),
           ),
           column(6, "",
                  numericInput('heatheight', 'Height', 4, min = 4, width = '50%'),
                  strong('Download'),br(),
                  downloadButton("download_heat", label = 'Heatmap')
           ),
         )
  )
)
