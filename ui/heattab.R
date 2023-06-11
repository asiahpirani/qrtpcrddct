
fluidRow(
  h3("Heatmap"),  
  tabPanel("Heatmap", plotOutput("heatmap")),
  h3("PCA"),  
  tabPanel("PCA", plotOutput("pcaplot"))
)
