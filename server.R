
require(shiny)
require(shinyjs)
require(shinyFeedback)
require(ggplot2)
require(purrr)
require(dplyr)


isLoaded = F
isProcessed = F

makeDeltaDelta = function(data, cond_col, times_col, rep_col, tech_col,
                          ctrl, timecntrl, housekeeping, target,
                          addctrl, addlog, addmin, 
                          eff_matrix)
{
  
  if (tech_col != 'NA')
  {
    group_names = c(rep_col)
    if (times_col != 'NA')
    {
      group_names = c(times_col, group_names)
    }
    if (cond_col != 'NA')
    {
      group_names = c(cond_col, group_names)
    }
    
    var_names = genenames
    if (effradio == 1)
    {
      var_names = c(var_names, geneeff)
    }
    data = data %>% group_by_at(group_names) %>% 
      summarise(across(var_names,mean)) %>% 
      as.data.frame()
  }
  
  print(head(data))
  
  conditions = data[, cond_col]
  uconditions = unique(conditions)
  uconditions = uconditions[-which(uconditions==ctrl)]
  uconditions = c(ctrl, uconditions)
  
  if (times_col != 'NA')
  {
    times = data[, times_col]
    utimes = unique(times)
  }
  
  
  
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
  # disable("makeplot")
  
  hideTab(inputId = 'mainpagetab', target = 'Plot')
  
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
  
  all_genes = reactive({c(input$houseselect, input$geneselect)})
  
  # output$rotor_ph = renderUI({fluidRow()})
  
  observeEvent(eventExpr = input$effradio, handlerExpr = {
    if(input$effradio == 0)
    {
    }
    else if(input$effradio == 1) 
    {
      output$rotor_ph = renderUI({
        fluidRow(
          column(12, "",
            map(all_genes(), ~ selectInput(.x, .x, choices=c('', colnames(my_tab()))))
          )
        )
      })
    } 
    else if (input$effradio == 2)
    {
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
    updateSelectInput(inputId = 'techselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'condselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'timeselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'houseselect', choices = colnames(loadeddata))
    updateSelectInput(inputId = 'geneselect', choices = colnames(loadeddata))
    
    enable("processb")
    enable("repselect")
    enable("techselect")
    enable("houseselect")
    enable("geneselect")
    enable("condselect")
    enable("timeselect")
    enable("ctrlselect")
    enable("timectrlselect")
    enable('effradio')
    
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
  
  observeEvent(eventExpr = input$timeselect, handlerExpr = {
    timecol = input$timeselect
    if (timecol != "")
    {
      cid = which(colnames(my_tab()) == timecol)
      updateSelectInput(inputId = 'timectrlselect', choices = unique(my_tab()[,cid]))
    }
  })

  processAndPlot = function()
  {
    cond_check  = input$condselect != "NA"
    time_check  = input$timeselect != "NA"
    ctrl_check  = input$ctrlselect != ""
    timectrl_check  = input$timectrlselect != ""
    house_check = !is.null(input$houseselect)
    gene_check  = !is.null(input$geneselect)
    
    feedbackWarning(inputId = 'condselect',  show=(!cond_check && !time_check),  text = "Please select the condition or time column.")
    feedbackWarning(inputId = 'timeselect',  show=(!cond_check && !time_check),  text = "Please select the time or condition column.")
    feedbackWarning(inputId = 'ctrlselect',  show=(!ctrl_check && cond_check),  text = "Please select the Control label.")
    feedbackWarning(inputId = 'timectrlselect',  show=(!timectrl_check && time_check),  text = "Please select the T0 label.")
    feedbackWarning(inputId = 'houseselect', show=!house_check, text = "Please select the house keeping gene(s).")
    feedbackWarning(inputId = 'geneselect',  show=!gene_check,  text = "Please select the target gene(s).")
    
    req(cond_check||time_check)
    req(ctrl_check||!cond_check)
    req(timectrl_check||!time_check)
    req(house_check)
    req(gene_check)
    
    if (input$effradio == 1)
    {
      for (g in all_genes())
      {
        c_check  = input[[g]] != ""
        feedbackWarning(inputId = g,  show=!c_check,  text = "Please select the efficiency column.")
      }
      for (g in all_genes())
      {
        req(c_check)
      }
    }
    
    eff = 2
    eff_matrix = matrix(eff, dim(my_tab())[1], length(all_genes()))
    colnames(eff_matrix) = all_genes()
    if (input$effradio == 1)
    {
      for (g in all_genes())
      {
        eff_matrix[, g] = my_tab()[, input[[g]]]
      }
    }
    if (input$effradio == 2)
    {
    }
    
    data = makeDeltaDelta(my_tab(), input$condselect, input$timeselect, 
                          input$repselect, input$techselect,
                          input$ctrlselect, input$timectrlselect,
                          input$houseselect, input$geneselect,
                          input$plotctrl, input$plotlog, input$ploterr, 
                          eff_matrix)
      
    
    proc_data <<- data
    p = makeOnePlot(data, input$condselect, input$ctrlselect, input$houseselect, input$geneselect,
                    input$plotctrl, input$plotlog, input$ploterr, input$plotgrp)
    
    proc_plot <<- p
    
    showTab(inputId = 'mainpagetab', target = 'Plot')
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
  
  observeEvent(input$timeselect, handlerExpr = {
    hideFeedback("timeselect")
  })
  observeEvent(input$condselect, handlerExpr = {
    hideFeedback("condselect")
  })
  observeEvent(input$timectrlselect, handlerExpr = {
    hideFeedback("timectrlselect")
  })
  observeEvent(input$ctrlselect, handlerExpr = {
    hideFeedback("ctrlselect")
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
