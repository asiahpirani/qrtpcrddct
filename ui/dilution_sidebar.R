

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
                  disabled(selectInput("dilrepselect", "Biological replicates\' column", c())),
                  disabled(selectInput("dilcondselect", "Conditions\' column", c())), 
                  disabled(selectInput('dilgeneselect', 'Gene names\' column', c())),
                  disabled(selectInput('dilcpselect', 'CP cycles', c())),
           ),
           column(6, "", # offset = 2, 
                  disabled(selectInput("diltechselect", "Technical replicates\' column", c())),
                  disabled(selectInput("diltimeselect", "Time points\' column", c())), 
                  disabled(selectInput('dilcdnaselect', 'cDNA concentration', c())),
           )
         )
  ),
  column(12, "",
         hr(style = "border: 1px solid #aaaaaa;")
  ),
  column(12, "",
         disabled(actionButton('dilprocessb', 'Process', ))
  )
)
