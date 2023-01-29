sidebarLayout(
  sidebarPanel(width = 5,
               fluidRow(
                 column(12, "",
                        fluidRow(
                          column(5, "",
                                 radioButtons("plotctrl", 'Control', 
                                              choices = list("Show Control" = 1, "Don't Show Control" = 2), 
                                              selected = 1),
                                 radioButtons("plotlog", 'log scale',
                                              choices = list("original" = 1, "log" = 2), 
                                              selected = 1)
                          ),
                          column(5, "",
                                 radioButtons("ploterr", 'Error bars',
                                              choices = list("min-max" = 1, "STD" = 2), 
                                              selected = 1),
                                 radioButtons("plotgrp", 'Group by', 
                                              choices = list("Condition" = 1, "Cell type" = 2), 
                                              selected = 1)
                          ),
                          actionButton('makeplotb', 'Make Plot')
                        )))),
  mainPanel(h3("Plot"), width = 5, 
            tabPanel("Plot", plotOutput("plot"))
  )
)