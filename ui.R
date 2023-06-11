require(shiny)
require(shinyjs)
require(shinyFeedback)
require(ggplot2)

source(file.path('global_vars.R'),  local = TRUE)

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
      conditionalPanel(condition = paste('input.mainpagetab == "', dilution_tab_title, '"', sep=''), 
                       source(file.path("ui", "dilution_sidebar.R"),  local = TRUE)$value
      ),
      conditionalPanel(condition = paste('input.mainpagetab == "', plot_tab_title, '"', sep=''), 
                       source(file.path("ui", "plottab_sidebar.R"),  local = TRUE)$value
      ),
      conditionalPanel(condition = paste('input.mainpagetab == "', heatmap_tab_title, '"', sep=''), 
                       source(file.path("ui", "heattab_sidebar.R"),  local = TRUE)$value
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
        tabPanel(dilution_tab_title, 
                 source(file.path("ui", "dilution.R"),  local = TRUE)$value
        ),
        tabPanel(plot_tab_title, 
                 source(file.path("ui", "plottab.R"),  local = TRUE)$value
        ),
        tabPanel(heatmap_tab_title,
                 source(file.path("ui", "heattab.R"),  local = TRUE)$value)
      )
    )
  )
)
