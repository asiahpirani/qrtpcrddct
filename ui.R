require(shiny)
require(shinyjs)
require(shinyFeedback)
require(ggplot2)

title_str = "qRT-PCR \\(\\Delta\\Delta\\text{CT}\\) Analysis"
load_tab_title = "Load Data"
plot_tab_title = "Plot"

# Define UI ----
ui <- fluidPage(
  useShinyFeedback(),
  useShinyjs(),
  withMathJax(),
  titlePanel(title_str),
  sidebarLayout(
    sidebarPanel(width = 3,
      conditionalPanel(condition = paste('input.mainpagetab == "', load_tab_title, '"', sep=''), 
        source(file.path("ui", "loaddata_sidebar.R"),  local = TRUE)$value
      ),
      conditionalPanel(condition = paste('input.mainpagetab == "', plot_tab_title, '"', sep=''), 
                       source(file.path("ui", "plottab_sidebar.R"),  local = TRUE)$value
      )
    ),
    mainPanel(
      tabsetPanel(
        # navbarPage(
        # title_str,
        id='mainpagetab',
        tabPanel(load_tab_title, 
                 source(file.path("ui", "loaddata.R"),  local = TRUE)$value
        ),
        tabPanel(plot_tab_title, 
                 source(file.path("ui", "plottab.R"),  local = TRUE)$value
        )
      )
    )
  )
)
