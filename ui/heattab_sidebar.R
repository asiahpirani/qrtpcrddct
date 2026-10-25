fluidRow(
  column(12, "",
         h4('Heatmap settings'),
  ),
  column(12, "",
         fluidRow(
           column(6, "",
                  radioButtons("heatctrl",  "Calibrator group", 
                               choices = list( "Hide calibrator group" = 2, "Show calibrator group" = 1), 
                               selected = 2),
                  radioButtons("heatlog", "Expression scale",
                               choices = list("Fold change" = 1,  "log2 fold change" = 2), 
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
  ),
  
  div(
    id = "pca_download_section",
    
    column(
      12, "",
      hr(style = "border: 1px solid #aaaaaa;")
    ),
    
    column(
      12, "",
      h4("Download PCA")
    ),
    
    column(
      12, "",
      
      fluidRow(
        
        column(
          6, "",
          
          numericInput(
            "pcawidth",
            "Width",
            6,
            min = 4,
            width = "50%"
          ),
          
          selectInput(
            "pcafrmt",
            "Select output format",
            c("png", "pdf")
          )
        ),
        
        column(
          6, "",
          
          numericInput(
            "pcaheight",
            "Height",
            4,
            min = 4,
            width = "50%"
          ),
          
          strong("Download"),
          br(),
          
          downloadButton(
            "download_pca",
            label = "PCA"
          )
        )
        
      )
    ),
    
    column(
      12, "",
      
      br(),
      
      strong("Download PCA scores"),
      
      br(),
      
      downloadButton(
        "download_pca_scores",
        label = "PCA scores (CSV)"
      )
    )
    
  )
)


