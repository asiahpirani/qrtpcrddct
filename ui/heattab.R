fluidRow(
  
  h3("Heatmap"),
  
  tabPanel(
    "Heatmap",
    plotOutput("heatmap")
  ),
  
  
  div(
    id = "pca_section",
    
    h3("PCA"),
    
    tabPanel(
      "PCA",
      plotOutput("pcaplot")
    )
  )
  
)