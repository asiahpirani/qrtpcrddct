
require(shiny)
require(shinyjs)
require(shinyFeedback)
require(ggplot2)

isLoaded = F
isProcessed = F

makeDeltaDelta = function(data, cond, ctrl, housekeeping, target, addctrl, addlog, addmin, eff = 2)
{
  conditions = data[, cond]
  uconditions = unique(conditions)
  uconditions = uconditions[-which(uconditions==ctrl)]
  uconditions = c(ctrl, uconditions)
  hk = data[, housekeeping]
  if (length(housekeeping) > 1)
  {
    hk = apply(hk, 1, mean)
  }
  tg = as.data.frame(data[, target])
  colnames(tg) = target
  delta = tg - hk
  
  res = c()
  for (tg in target)
  {
    dd = delta[, tg]
    ctrlmean = mean(dd[conditions == ctrl])
    deltadelta = dd - ctrlmean
    res = cbind(res, deltadelta)
  }
  colnames(res) = target
  
  # conditions = conditions[conditions != ctrl]
  # deltadelta = deltadelta[conditions != ctrl]
  res = eff^-res
  res_agg = c()
  res_names = c()
  for (cnd in uconditions)
  {
    for (tg in target)
    {
      res_mean = mean(res[conditions == cnd, tg])
      res_min  = min(res[conditions == cnd, tg])
      res_max  = max(res[conditions == cnd, tg])
      if (addmin == 2)
      {
        ss = sd(res[conditions == cnd, tg])
        res_min = res_mean-ss
        res_max = res_mean+ss
      }
      if (cnd == ctrl)
      {
        res_mean = 1
        res_min = 1
        res_max = 1
      }
      if (addctrl != 2 || cnd != ctrl)
      {
        res_agg  = rbind(res_agg, c(res_mean, res_min, res_max))
        res_names = rbind(res_names, c(cnd, tg))
      }
    }
  }
  res_agg = as.data.frame(res_agg)
  colnames(res_agg) = c('mean', 'min', 'max')
  if (addlog == 2)
  {
    res_agg = log(res_agg, base = 2)
  }
  res_agg = cbind(Conditions=res_names[,1], Target=res_names[,2], res_agg)
  return(res_agg)
}

makeOnePlot = function(data, cond, ctrl, houses, genes, addctrl, addlog, addmin, addgrp)
{
  if (addgrp == 1)
  {
    aa = aes(x=Conditions, y=mean, fill=Target)
  }
  else
  {
    aa = aes(x=Target, y=mean, fill=Conditions)
  }
  hh = 1
  yl = expression(paste(Delta, Delta, 'CT'))
  if (addlog == 2)
  {
    hh = 0
    yl = expression(paste('log ',Delta, Delta, 'CT'))
  }
  p = ggplot(data, aa) +
    geom_bar(stat="identity", position=position_dodge()) +
    ylab(yl) + xlab('') +
    geom_errorbar(aes(ymin=min, ymax=max), width=.2,
                  position=position_dodge(.9)) +
    geom_hline(yintercept=hh, linetype="dashed", color = "green")
    # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p)
}

# Define server logic ----
server <- function(input, output) {
  disable("makeplot")
  
  observeEvent(eventExpr = input$radio, handlerExpr = {
    if(input$radio == 0)
    {
      hideElement(id = 'infile')
      hideElement(id = 'textarea')
    }
    else if(input$radio == 1) 
    {
      showElement(id = 'infile')
      hideElement(id = 'textarea')
    } 
    else if (input$radio == 2)
    {
      showElement(id = 'textarea')
      hideElement(id = 'infile')
    }
  })
  
  my_tab = eventReactive(input$loadb, 
  {
    if(input$radio == 0) # load default
    {
      file <- file.path("example", "data.csv")
      loadeddata <- read.csv(file)
    }
    else if(input$radio == 1) # Upload data
    {
      file <- input$infile
      check = !is.null(file)
      print(check)
      feedbackWarning(inputId = 'infile', show=!check, text = "Please select an input file.")
      # validate(need(check, "Please select an input file."))
      req(check)
      ext <- tools::file_ext(file$datapath)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      loadeddata <- read.csv(file$datapath)
    }
    else if(input$radio == 2) # Paste Data
    {
      check = input$textarea != ''
      feedbackWarning('textarea', !check, "Please provide input.")
      req(check)
      loadeddata <- read.table(text = input$textarea, sep='\t', header = T)
    }
    
    updateSelectInput(inputId = 'repselect', choices = colnames(loadeddata))
    updateSelectInput(inputId = 'condselect', choices = colnames(loadeddata))
    updateSelectInput(inputId = 'houseselect', choices = colnames(loadeddata))
    updateSelectInput(inputId = 'geneselect', choices = colnames(loadeddata))
    
    enable("processb")
    
    loadeddata
  })
  
  output$tabres <- renderTable(my_tab())
  
  observeEvent(eventExpr = input$condselect, handlerExpr = {
    condcol = input$condselect
    if (condcol != "")
    {
      cid = which(colnames(my_tab()) == condcol)
      updateSelectInput(inputId = 'ctrlselect', choices = unique(my_tab()[,cid]))
    }
  })

  processAndPlot = function()
  {
    cond_check  = input$condselect != ""
    ctrl_check  = input$ctrlselect != ""
    house_check = !is.null(input$houseselect)
    gene_check  = !is.null(input$geneselect)
    
    feedbackWarning(inputId = 'condselect',  show=!cond_check,  text = "Please select the condition column.")
    feedbackWarning(inputId = 'ctrlselect',  show=!ctrl_check,  text = "Please select the Control label.")
    feedbackWarning(inputId = 'houseselect', show=!house_check, text = "Please select the house keeping gene(s).")
    feedbackWarning(inputId = 'geneselect',  show=!gene_check,  text = "Please select the target gene(s).")
    req(cond_check)
    req(ctrl_check)
    req(house_check)
    req(gene_check)
    
    data = makeDeltaDelta(my_tab(), input$condselect, input$ctrlselect, input$houseselect, input$geneselect,
                          input$plotctrl, input$plotlog, input$ploterr)
    proc_data <<- data
    p = makeOnePlot(data, input$condselect, input$ctrlselect, input$houseselect, input$geneselect,
                    input$plotctrl, input$plotlog, input$ploterr, input$plotgrp)
    
    proc_plot <<- p
    
    updateNavbarPage(inputId = 'mainpagetab', selected = 'Plot')
    output$plot = renderPlot(p, res = 96)
  }
  
  observeEvent(ignoreInit = T, c(input$processb,
                                 input$plotctrl, input$plotlog, 
                                 input$ploterr, input$plotgrp), 
    handlerExpr = {
      processAndPlot()
  })
  
  observeEvent(input$houseselect, handlerExpr = {
    hideFeedback("houseselect")
  })
  observeEvent(input$geneselect, handlerExpr = {
    hideFeedback("geneselect")
  })
  observeEvent(input$infile, handlerExpr = {
    hideFeedback("infile")
  })
  observeEvent(input$textarea, handlerExpr = {
    hideFeedback("textarea")
  })
  
  output$download_tab <- downloadHandler(
    filename = function(){'delta_delta.csv'},
    content = function(file)
    {
      write.csv(proc_data, file)
    }
  )
  
  output$download_plt <- downloadHandler(
    filename = function(){paste('delta_delta_bar.', input$pltfrmt, sep='')},
    content = function(file)
    {
      if (input$pltfrmt == 'pdf')
      {
        fnc = pdf
      }
      else if (input$pltfrmt == 'png')
      {
        fnc = function(...){png(..., units='in', res=300)}
      }
      fnc(file = file, width = input$width, height = input$height)
      plot(proc_plot)
      dev.off()
    }
  )
}
