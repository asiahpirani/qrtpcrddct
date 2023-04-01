

fluidRow(
  column(12, "",
         fluidRow(
           column(12, "",
             radioButtons("radio", h4("Select input"),
                          choices = list("Load sample" = 0,
                                         "Upload data" = 1, 
                                         "Paste Data" = 2), selected = 0),
             fileInput("infile", h3("Input File")),
             textAreaInput("textarea", h3("Input Data"), "", height = "200px", width = "1000px"),
             actionButton('loadb', 'Load')
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
                 disabled(selectInput("repselect", "Biological replicates\' column", c())),
                 disabled(selectInput('houseselect', 'Reference genes', c(), multiple = T)),
                 disabled(selectInput("condselect", "Conditions\' column", c())), 
                 disabled(selectizeInput('condincselect', 'Conditions\' to include', c(), multiple = T, options = list(minItems = 1))),
                 disabled(selectInput("ctrlselect", "Control condition", choices = c(""), selected = ""))
          ),
          column(6, "", # offset = 2, 
                 disabled(selectInput("techselect", "Technical replicates\' column", c())),
                 disabled(selectInput('geneselect', 'Target genes', c(), multiple = T)),
                 disabled(selectInput("timeselect", "Time points\' column", c())), 
                 disabled(selectizeInput('timeincselect', 'Time points\' to include', c(), multiple = T, options = list(minItems = 1))),
                 disabled(selectInput("timectrlselect", "T0 Time point", choices = c(""), selected = ""))
          )
        )
    ),
  column(12, "",
         hr(style = "border: 1px solid #aaaaaa;")
  ),
  column(12, "",
         h4('Amplification efficiency:'),
  ),
  column(12, "",
         fluidRow(
           column(12, "",
                  disabled(radioButtons("effradio", h4("Select input"),
                               choices = list("Default (=2)" = 0,
                                              "Rotor-Gene" = 1, 
                                              "Dilution Method" = 2), selected = 0)),
           )
         ),
         fluidRow(
           column(12, "",
             conditionalPanel(condition = 'input.effradio == 1', 
                              h4('Select Rotor-Gene efficiency columns'),
                              uiOutput('rotor_ph')
             ),
             # conditionalPanel(condition = 'input.effradio == 2', 
             #                  numericInput('test', 'test', 2)
             # )
           )
         )
  ),
  column(12, "",
         disabled(actionButton('processb', 'Process', ))
  )
 )
