

fluidRow(
  column(12, "",
         fluidRow(
           column(12, "",
                  fileInput("dilinfile", h3("Input File")),
                  actionButton('dilloadb', 'Load')
           )
         )
  ),
  column(12, "",
         hr(style = "border: 1px solid #aaaaaa;")
  ),
  column(12, "",
         h4('Select:'),
  ),
  column(12, "",
         fluidRow(
           column(6, "",
                  # disabled(selectInput("dilrepselect", "Biological replicates\' column", c())),
                  # disabled(selectInput("dilcondselect", "Conditions\' column", c())), 
                  disabled(selectInput('dilgeneselect', 'Genes', c())),
                  disabled(selectInput('dilcpselect', 'CP cycles', c())),
           ),
           column(6, "", # offset = 2, 
                  # disabled(selectInput("diltechselect", "Technical replicates\' column", c())),
                  # disabled(selectInput("diltimeselect", "Time points\' column", c())), 
                  disabled(selectInput('dilcdnaselect', 'cDNA concentration', c())),
           )
         )
  ),
  # column(12, "",
  #        hr(style = "border: 1px solid #aaaaaa;")
  # ),
  column(12, "",
         disabled(actionButton('dilprocessb', 'Process', ))
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
                  disabled(numericInput('dilwidth', 'Width', 6, min = 4, width = '50%')),
                  disabled(selectInput("dilpltfrmt", "Select output format", c('png', 'pdf'))),
           ),
           column(6, "",
                  disabled(numericInput('dilheight', 'Height', 4, min = 4, width = '50%')),
                  strong('Download'),br(),
                  disabled(downloadButton("download_dilplt", label = 'Plot'))
           ),
         )
  )
)
