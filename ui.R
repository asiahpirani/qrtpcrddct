require(shiny)
require(shinyjs)
require(ggplot2)

title_str = "qRT-PCR \\(\\Delta\\Delta\\text{CT}\\) Analysis"

# Define UI ----
ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
  titlePanel(title_str),
  navbarPage(
    title_str, id='mainpagetab',
    tabPanel("Load Data",
             sidebarLayout(
               sidebarPanel(width = 5,
                            fluidRow(
                              column(12, "",
                                     fluidRow(
                                       column(5, "",
                                              radioButtons("radio", h3("Select input"),
                                                           choices = list("Upload data" = 1, 
                                                                          "Paste Data" = 2), selected = 1),
                                              fileInput("infile", h3("Input File")),
                                              textAreaInput("textarea", h3("Input Data"), "", height = "200px", width = "1000px"),
                                              actionButton('loadb', 'Load'),
                                              actionButton('makeplot', 'Process')),
                                       column(5, "", offset = 2, 
                                              selectInput("repselect", "Select replicates\' column", c()),
                                              selectInput("condselect", "Select conditions\' column", c()), 
                                              selectInput('houseselect', 'Select Housekeeping genes', c(), multiple = T),
                                              selectInput('geneselect', 'Select Target genes', c(), multiple = T),
                                              selectInput("ctrlselect", "Select Control condition", c())
                                       ))))
               ),
               mainPanel(h3("Data preview"), width = 5, 
                         tableOutput("tabres")
               )
             )
    ),
    tabPanel("Plot", 
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
                                       )
                                     )))),
               mainPanel(h3("Plot"), width = 5, 
                         tabPanel("Plot", plotOutput("plot"))
               )
             ),
    )
  )
)
