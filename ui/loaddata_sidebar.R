

fluidRow(
 column(12, "",
        fluidRow(
          column(6, "",
                 radioButtons("radio", h3("Select input"),
                              choices = list("Load sample" = 0,
                                             "Upload data" = 1, 
                                             "Paste Data" = 2), selected = 0),
                 fileInput("infile", h3("Input File")),
                 textAreaInput("textarea", h3("Input Data"), "", height = "200px", width = "1000px"),
                 actionButton('loadb', 'Load'),
                 disabled(actionButton('processb', 'Process', )),
                 # numericInput('n', 'n', value=3)
          ),
          column(6, "", # offset = 2, 
                 selectInput("repselect", "Select replicates\' column", c()),
                 selectInput("condselect", "Select conditions\' column", c()), 
                 selectInput('houseselect', 'Select Housekeeping genes', c(), multiple = T),
                 selectInput('geneselect', 'Select Target genes', c(), multiple = T),
                 selectInput("ctrlselect", "Select Control condition", c())
          )
        )
    )
 )
