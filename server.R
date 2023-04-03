
require(shiny)
require(shinyjs)
require(shinyFeedback)
require(ggplot2)
require(purrr)
require(dplyr)
require(tidyr)


source(file.path('global_vars.R'),  local = TRUE)

isLoaded = F
isProcessed = F

makeDeltaDelta = function(data, cond_col, uconditions, times_col, utimes, rep_col, tech_col,
                          ctrl, timecntrl, housekeeping, target,
                          eff_matrix)
{
  print(uconditions)
  print(utimes)
  all_genes = c(housekeeping, target)
  for (g in all_genes)
  {
    data[, g] = eff_matrix[, g]^data[, g]
  }
  g_mean = function(vec)
  {
    exp(mean(log(vec)))
  }
  g_sd = function(vec)
  {
    m = g_mean(vec)
    exp(sqrt(sum(log(vec/m)^2)/length(vec)))
  }
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
    
    var_names = all_genes
    data = data %>% group_by_at(group_names) %>% 
      summarise(across(var_names,g_mean)) %>% 
      as.data.frame()
  }
  
  reps = data[, rep_col]
  ureps = unique(reps)
  
  get_unique = function(uconditions, ctrl)
  {
    # uconditions = unique(conditions)
    uconditions = uconditions[-which(uconditions==ctrl)]
    uconditions = c(ctrl, uconditions)
    uconditions
  }
  
  if (cond_col != 'NA')
  {
    conditions = data[, cond_col]
    uconditions = get_unique(uconditions, ctrl)
  }
  if (times_col != 'NA')
  {
    times  = data[, times_col]
    utimes = get_unique(utimes, timecntrl)
  }
  
  hk = data[, housekeeping]
  if (length(housekeeping) > 1)
  {
    hk = apply(hk, 1, g_mean)
  }
  tg = as.data.frame(data[, target])
  colnames(tg) = target
  delta = tg / hk
  
  res = c()
  for (tg in target)
  {
    dd = delta[, tg]
    if (cond_col != 'NA')
    {
      ids1 = conditions == ctrl
      ids  = ids1
    }
    if (times_col != 'NA')
    {
      ids2 = times == timecntrl
      ids  = ids2
    }
    if (times_col != 'NA' && cond_col != 'NA')
    {
      ids = ids1 & ids2
    }
    ctrlmean = g_mean(dd[ids])
    deltadelta = dd / ctrlmean
    res = cbind(res, deltadelta)
  }
  colnames(res) = target
  
  # conditions = conditions[conditions != ctrl]
  # deltadelta = deltadelta[conditions != ctrl]
  # res = eff^-res
  res = 1/res
  
  get_agg = function(vec, islog)
  {
    # if(islog)
    # {
    #   res_mean = mean(vec)
    # }
    # else
    # {
    #   res_mean = g_mean(vec)
    # }
    # res_min  = min(vec)
    # res_max  = max(vec)
    # if(islog)
    # {
    #   ss = sd(vec)
    # }
    # else
    # {
    #   ss = g_sd(vec)
    # }
    
    res_mean = g_mean(vec)
    if(islog)
    {
      res_mean = log2(res_mean)
    }
    
    res_min  = min(vec)
    res_max  = max(vec)
    ss = g_sd(vec)
    if(islog)
    {
      res_min = log2(res_min)
      res_max = log2(res_max)
      ss = log2(ss)
    }
    res_sdn  = res_mean-ss
    res_sdp  = res_mean+ss
    agg = c(res_mean, res_min, res_max, res_sdn, res_sdp)
    if(islog)
    {
      names(agg) = c('log.mean', 'log.min', 'log.max', 'log.-sd', 'log.+sd')
    }
    else
    {
      names(agg) = c('mean', 'min', 'max', '-sd', '+sd')
    }
    return(agg)
  }
  
  run_all_agg_1 = function(res, conditions, uconditions, targets, reps, ureps)
  {
    res_agg = c()
    res_names = c()
    for (cnd in uconditions)
    {
      for (tg in target)
      {
        vec  = res[conditions == cnd, tg]
        lvec = log2(vec)
        agg  = get_agg(vec, F)
        # lagg = get_agg(lvec, T)
        lagg = get_agg(vec, T)
        # names(lagg) = paste('log.', names(lagg), sep='')
        
        temp = rep(NA, length(ureps))
        names(temp) = ureps
        creps = reps[conditions == cnd]
        temp[creps] = vec
        
        res_agg   = rbind(res_agg, c(temp, agg, lagg))
        res_names = rbind(res_names, c(cnd, tg))
      }
    }
    res_agg = as.data.frame(res_agg)
    res_agg = cbind(Conditions=res_names[,1], Target=res_names[,2], res_agg)
    return(res_agg)
  }
  
  run_all_agg_2 = function(res, conditions, uconditions, times, utimes, targets, reps, ureps)
  {
    res_agg = c()
    res_names = c()
    for (cnd in uconditions)
    {
      for (tm in utimes)
      {
        for (tg in target)
        {
          vec  = res[conditions == cnd & times == tm, tg]
          lvec = log2(vec)
          agg  = get_agg(vec, F)
          # lagg = get_agg(lvec, T)
          lagg = get_agg(vec, T)
          # names(lagg) = paste('log.', names(lagg), sep='')
          
          temp = rep(NA, length(ureps))
          names(temp) = ureps
          creps = reps[conditions == cnd & times == tm]
          temp[creps] = vec
          
          res_agg   = rbind(res_agg, c(temp, agg, lagg))
          res_names = rbind(res_names, c(cnd, tm, tg))
        }
      }
    }
    res_agg = as.data.frame(res_agg)
    res_agg = cbind(Conditions=res_names[,1], Times=res_names[,2], Target=res_names[,3], res_agg)
    return(res_agg)
  }
  
  if(times_col != 'NA' && cond_col != 'NA')
  {
    res_agg = run_all_agg_2(res, conditions, uconditions, times, utimes, targets, reps, ureps)
  }
  if(times_col == 'NA' && cond_col != 'NA')
  {
    res_agg = run_all_agg_1(res, conditions, uconditions, targets, reps, ureps)
  }
  if(times_col != 'NA' && cond_col == 'NA')
  {
    res_agg = run_all_agg_1(res, times, utimes, targets, reps, ureps)
    colnames(res_agg)[1] = 'Times'
  }
  
  return(res_agg)
}

makeOnePlot = function(data, cond_select, time_select, ctrl, timectrl, houses, genes, addctrl, addlog, addmin, addgrp, plotori)
{
  
  if (addctrl == 2)
  {
    if (cond_select!="NA" && time_select!="NA")
    {
      data = data %>% filter(Conditions != ctrl | Times != timectrl)
    }
    else
    {
      if (cond_select!="NA")
      {
        data = data %>% filter(Conditions != ctrl)
      }
      if (time_select!="NA")
      {
        data = data %>% filter(Times != timectrl)
      }
    }
  }
  
  if (addlog == 2)
  {
    yy = 'log.mean'
  }
  else
  {
    yy = 'mean'
  }
  
  # list("Genes" = 1, 
  #      "Conditions" = 2,
  #      "Times" = 3,
  #      "Conditions & Genes (color by Genes)" = 4,
  #      "Conditions & Genes (color by Conditions)" = 5,
  #      "Times & Genes (color by Genes)" = 6,
  #      "Times & Genes (color by Times)" = 7,
  #      "Conditions & Times (color by Times)" = 8,
  #      "Conditions & Times (color by Conditions)" = 9)
  
  aa = switch(addgrp, 
              '1'=aes(x=Target, y=!!sym(yy), fill=Target),         # "Genes" = 1, 
              '2'=aes(x=Conditions, y=!!sym(yy), fill=Conditions), # "Conditions" = 2,
              '3'=aes(x=Times, y=!!sym(yy), fill=Times),           # "Times" = 3,
              '4'=aes(x=Conditions, y=!!sym(yy), fill=Target),     # "Conditions & Genes (color by Genes)" = 4,
              '5'=aes(x=Target, y=!!sym(yy), fill=Conditions),     # "Conditions & Genes (color by Conditions)" = 5,
              '6'=aes(x=Times, y=!!sym(yy), fill=Target),          # "Times & Genes (color by Genes)" = 6,
              '7'=aes(x=Target, y=!!sym(yy), fill=Times),          # "Times & Genes (color by Times)" = 7,
              '8'=aes(x=Conditions, y=!!sym(yy), fill=Times),      # "Conditions & Times (color by Times)" = 8,
              '9'=aes(x=Times, y=!!sym(yy), fill=Conditions)       # "Conditions & Times (color by Conditions)" = 9)
              )
  
  hh = 1
  yl = expression(paste(Delta, Delta, 'CT'))
  if (addlog == 2)
  {
    hh = 0
    yl = expression(paste('log ',Delta, Delta, 'CT'))
  }
  if (addmin == 1)
  {
    m1 = 'min'
    m2 = 'max'
  }
  else
  {
    m1 = '-sd'
    m2 = '+sd'
  }
  if (addlog == 2)
  {
    m1 = paste('log.', m1, sep='')
    m2 = paste('log.', m2, sep='')
  }
  p = ggplot(data, aa) +
    geom_bar(stat="identity", position=position_dodge())
  
  
  cond_time_cnt = 0
  if (cond_select != 'NA')
  {
    cond_time_cnt = cond_time_cnt + 1
  }
  if (time_select != 'NA')
  {
    cond_time_cnt = cond_time_cnt + 2
  }
  
  if (cond_time_cnt == 3)
  {
    
    # c('Conditions x Times'=1, 'Times x Conditions'=2)
    # c('Target x Times'=3, 'Times x Target'=4)
    # c('Conditions x Target'=5, 'Target x Conditions'=6)
    gg = switch(plotori,
                '1'=facet_grid(vars(Conditions), vars(Times)),
                '2'=facet_grid(vars(Times), vars(Conditions)),
                '3'=facet_grid(vars(Target), vars(Times)),
                '4'=facet_grid(vars(Times), vars(Target)),
                '5'=facet_grid(vars(Conditions), vars(Target)),
                '6'=facet_grid(vars(Target), vars(Conditions))
    )
  }
  
  p = switch(paste(addgrp, cond_time_cnt),
             "1 1" = p+facet_wrap(~Conditions), # "Genes" = 1, 
             "1 2" = p+facet_wrap(~Times),      # "Genes" = 1, 
             "1 3" = p+gg,                      # "Genes" = 1, 
             "2 1" = p+facet_wrap(~Target),     # "Conditions" = 2,
             "2 2" = p,                         # "Conditions" = 2, # shouldn't happen
             "2 3" = p+gg,                      # "Conditions" = 2,
             "3 1" = p,                         # "Times" = 3,      # shouldn't happen
             "3 2" = p+facet_wrap(~Target),     # "Times" = 3,
             "3 3" = p+gg,                      # "Times" = 3,
             "4 1" = p,                         # "Conditions & Genes (color by Genes)" = 4,
             "4 2" = p,                         # "Conditions & Genes (color by Genes)" = 4, # shouldn't happen
             "4 3" = p+facet_wrap(~Times),      # "Conditions & Genes (color by Genes)" = 4,
             "5 1" = p,                         # "Conditions & Genes (color by Conditions)" = 5,
             "5 2" = p,                         # "Conditions & Genes (color by Conditions)" = 5, # shouldn't happen
             "5 3" = p+facet_wrap(~Times),      # "Conditions & Genes (color by Conditions)" = 5,
             "6 1" = p,                         # "Times & Genes (color by Genes)" = 6, # shouldn't happen
             "6 2" = p,                         # "Times & Genes (color by Genes)" = 6,
             "6 3" = p+facet_wrap(~Conditions), # "Times & Genes (color by Genes)" = 6,
             "7 1" = p,                         # "Times & Genes (color by Times)" = 7, # shouldn't happen
             "7 2" = p,                         # "Times & Genes (color by Times)" = 7,
             "7 3" = p+facet_wrap(~Conditions), # "Times & Genes (color by Times)" = 7,
             "8 1" = p,                         # "Conditions & Times (color by Times)" = 8, # shouldn't happen
             "8 2" = p,                         # "Conditions & Times (color by Times)" = 8, # shouldn't happen
             "8 3" = p+facet_wrap(~Target),     # "Conditions & Times (color by Times)" = 8,
             "9 1" = p,                         # "Conditions & Times (color by Conditions)" = 9) # shouldn't happen
             "9 2" = p,                         # "Conditions & Times (color by Conditions)" = 9) # shouldn't happen
             "9 3" = p+facet_wrap(~Target),     # "Conditions & Times (color by Conditions)" = 9)
             )

  p = p +
    ylab(yl) + xlab('') +
    geom_errorbar(aes(ymin=!!sym(m1), ymax=!!sym(m2)), width=.2,
                  position=position_dodge(.9)) +
    geom_hline(yintercept=hh, linetype="dashed", color = "green")
    # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p)
}

# Define server logic ----
server <- function(input, output, session) {
  # disable("makeplot")
  
  hideTab(inputId = 'mainpagetab', target = plot_tab_title)
  hideTab(inputId = 'mainpagetab', target = dilution_tab_title)
  hideElement(id = 'plotori')
  
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
      hideTab(inputId = 'mainpagetab', target = dilution_tab_title)
    }
    else if(input$effradio == 1) 
    {
      hideTab(inputId = 'mainpagetab', target = dilution_tab_title)
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
      showTab(inputId = 'mainpagetab', target = dilution_tab_title)
      updateNavbarPage(inputId = 'mainpagetab', selected = dilution_tab_title)
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
    
    updateSelectInput(inputId = 'repselect',   choices = colnames(loadeddata))
    updateSelectInput(inputId = 'techselect',  choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'condselect',  choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'timeselect',  choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'houseselect', choices = colnames(loadeddata))
    updateSelectInput(inputId = 'geneselect',  choices = colnames(loadeddata))
    
    updateSelectInput(inputId = 'ctrlselect',     choices = c(""), selected="")
    updateSelectInput(inputId = 'timectrlselect', choices = c(""), selected="")
    
    print('check update')
    print(input$ctrlselect)
    print(input$timectrlselect)
    
    enable("processb")
    enable("repselect")
    enable("techselect")
    enable("houseselect")
    enable("geneselect")
    enable("condselect")
    enable("condincselect")
    enable("timeselect")
    enable("timeincselect")
    enable("ctrlselect")
    enable("timectrlselect")
    enable('effradio')
    
    loadeddata
  })
  
  observeEvent(ignoreInit = T, input$dilloadb, 
  handlerExpr = {
    file <- input$dilinfile
    check = !is.null(file)
    feedbackWarning(inputId = 'dilinfile', show=!check, text = "Please select an input file.")
    # validate(need(check, "Please select an input file."))
    req(check)
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    loadeddata <- read.csv(file$datapath)
    
    updateSelectInput(inputId = 'dilrepselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'diltechselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'dilcondselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'diltimeselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'dilgeneselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'dilcpselect', choices = c('NA',colnames(loadeddata)))
    updateSelectInput(inputId = 'dilcdnaselect', choices = c('NA',colnames(loadeddata)))
    
    enable("dilprocessb")
    enable("dilrepselect")
    enable("diltechselect")
    enable("dilgeneselect")
    enable("dilcondselect")
    enable("diltimeselect")
    enable("dilcpselect")
    enable("dilcdnaselect")
    
    my_dil_tab <<- loadeddata
    
    output$diltabres <- renderTable({loadeddata})
  })
  
  processDil = function()
  {
    # rep_check  = input$dilrepselect != 'NA'
    # tech_check = input$diltechselect != 'NA'
    # cond_check = input$dilcondselect != 'NA'
    # time_check = input$diltimeselect != 'NA'
    gene_check = input$dilgeneselect != 'NA'
    cp_check   = input$dilcpselect != 'NA'
    cdna_check = input$dilcdnaselect != 'NA'
    

    # feedbackWarning(inputId = 'dilcondselect',  show=(!cond_check && !time_check), text = "Please select the condition or time column.")
    # feedbackWarning(inputId = 'diltimeselect',  show=(!cond_check && !time_check), text = "Please select the time or condition column.")
    # feedbackWarning(inputId = 'dilrepselect',   show=(!rep_check && !tech_check),  text = "Please select the biological or technical replicates\' column.")
    # feedbackWarning(inputId = 'diltechselect',  show=(!rep_check && !tech_check),  text = "Please select the biological or technical replicates\' column.")
    feedbackWarning(inputId = 'dilgeneselect',  show=!gene_check, text = "Please select the genes\' column.")
    feedbackWarning(inputId = 'dilcpselect',    show=!cp_check,   text = "Please select the Cycles\' column.")
    feedbackWarning(inputId = 'dilcdnaselect',  show=!cdna_check, text = "Please select the Concentrations\' column.")

    # req(cond_check||time_check)
    # req(rep_check||tech_check)
    req(gene_check)
    req(cp_check)
    req(cdna_check)

    col_list = c(input$dilgeneselect)
    
    # all_checks = c(cond_check, time_check, gene_check, rep_check, tech_check)
    # all_vals   = c(input$dilcondselect, input$diltimeselect,
    #                input$dilgeneselect,
    #                input$dilrepselect, input$diltechselect)
    # col_list = c()
    # for (i in 1:length(all_checks))
    # {
    #   if (all_checks[i])
    #   {
    #     col_list = c(col_list, all_vals[i])
    #   }
    # }
    
    calc_slope = function(x, y){
      xx = unlist(log10(x))
      yy = unlist(y)
      slope = coefficients(lm(yy ~ xx))[2]
      return(10^(-1/slope))
    }
    dil_sum <<- my_dil_tab %>% group_by_at(col_list) %>%
      summarise(eff=calc_slope(cur_data()[, input$dilcdnaselect], cur_data()[, input$dilcpselect])) %>%
      as.data.frame()
    
    # print(dil_sum)
    
    leg = c()
    for (i in 1:nrow(dil_sum))
    {
      g  = dil_sum[i, 1]
      e  = dil_sum[i, 2]
      s  = -1/log10(e)
      ss = paste(g, ', slope=', round(s, digits = 2), ', eff=', round(e, digits = 2), sep='')
      leg = c(leg, ss)
    }
    
    p <- ggplot(my_dil_tab, aes(x=cDNA.Input, y=Cycle, col=Gene)) + 
      geom_point() + 
      scale_x_continuous(trans='log10') + 
      geom_smooth(method = "lm", se=F, formula = y ~ x) + 
      xlab('cDNA Input') + ylab('Cycles') +
      theme(legend.position = c(0.8, 0.8)) +
      scale_color_discrete(labels=leg)
    
    proc_dilplot <<- p
    output$dilplot = renderPlot(p, res = 96)
    # print(dil_sum)
    # dil_sum_wide <<- dil_sum %>% spread(input$dilgeneselect, 'eff')
    # print(dil_sum_wide)
    # print('end')
    
    enable("dilwidth")
    enable("dilheight")
    enable("dilpltfrmt")
    enable("download_dilplt")
    
  }
  
  observeEvent(ignoreInit = T, input$dilprocessb, 
               handlerExpr = {
                 processDil()
               })
  
  output$tabres <- renderTable(my_tab())
  
  observeEvent(eventExpr = input$condselect, handlerExpr = {
    condcol = input$condselect
    if (condcol != "")
    {
      cid = which(colnames(my_tab()) == condcol)
      updateSelectizeInput(inputId = 'condincselect', choices = unique(my_tab()[,cid]), selected = unique(my_tab()[,cid]))
      updateSelectInput(inputId = 'ctrlselect', choices = unique(my_tab()[,cid]))
    }
  })
  
  observeEvent(eventExpr = input$timeselect, handlerExpr = {
    timecol = input$timeselect
    if (timecol != "")
    {
      cid = which(colnames(my_tab()) == timecol)
      updateSelectizeInput(inputId = 'timeincselect', choices = unique(my_tab()[,cid]), selected = unique(my_tab()[,cid]))
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
        c_check  = input[[g]] != ""
        req(c_check)
      }
    }
    if (input$effradio == 2)
    {
      for (g in all_genes())
      {
        c_check = sum(dil_sum[, 1] == g) == 1
        if (!c_check)
        {
          showNotification(paste('Dilution method efficiency is not provided for ', g, sep=''), type='error')
        }
      }
      for (g in all_genes())
      {
        c_check = sum(dil_sum[, 1] == g) == 1
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
      for (g in all_genes())
      {
        eff_matrix[, g] = dil_sum[dil_sum[, 1]==g, 'eff']
      }
    }
    
    data = makeDeltaDelta(my_tab(), input$condselect, input$condincselect, input$timeselect, input$timeincselect,
                          input$repselect, input$techselect,
                          input$ctrlselect, input$timectrlselect,
                          input$houseselect, input$geneselect,
                          eff_matrix)
    
    
    # list("Genes" = 1, 
    #      "Conditions" = 2,
    #      "Times" = 3,
    #      "Conditions & Genes (color by Genes)" = 4,
    #      "Conditions & Genes (color by Conditions)" = 5,
    #      "Times & Genes (color by Genes)" = 6,
    #      "Times & Genes (color by Times)" = 7,
    #      "Conditions & Times (color by Times)" = 8,
    #      "Conditions & Times (color by Conditions)" = 9)
    plotgrp = list("Genes" = 1)
    if (input$condselect != "NA")
    {
      plotgrp = append(plotgrp, list("Conditions" = 2))
    }
    if (input$timeselect != "NA")
    {
      plotgrp = append(plotgrp, list("Times" = 3))
    }
    if (input$condselect != "NA")
    {
      plotgrp = append(plotgrp, list("Conditions & Genes (color by Genes)" = 4))
      plotgrp = append(plotgrp, list("Conditions & Genes (color by Conditions)" = 5))
    }
    if (input$timeselect != "NA")
    {
      plotgrp = append(plotgrp, list("Times & Genes (color by Genes)" = 6))
      plotgrp = append(plotgrp, list("Times & Genes (color by Times)" = 7))
    }
    if (input$condselect != "NA" && input$timeselect != "NA")
    {
      plotgrp = append(plotgrp, list("Conditions & Times (color by Times)" = 8))
      plotgrp = append(plotgrp, list("Conditions & Times (color by Conditions)" = 9))
    }
    updateSelectInput(inputId = 'plotgrp', choices = plotgrp)
    
    update_ori()
    
    proc_data <<- data
    p = makeOnePlot(data, input$condselect, input$timeselect, input$ctrlselect, input$timectrlselect, 
                    input$houseselect, input$geneselect,
                    input$plotctrl, input$plotlog, input$ploterr, input$plotgrp, input$plotori)
    
    
    proc_plot <<- p
    
    showTab(inputId = 'mainpagetab', target = 'Plot')
    updateNavbarPage(inputId = 'mainpagetab', selected = plot_tab_title)
    output$plot = renderPlot(p, res = 96)
  }
  
  observeEvent(ignoreInit = T, input$processb, 
    handlerExpr = {
      processAndPlot()
  })
  
  observeEvent(ignoreInit = T, c(input$plotctrl, input$plotlog, 
                                 input$ploterr, input$plotgrp,
                                 input$plotori), 
    handlerExpr = {
      p = makeOnePlot(proc_data, 
                      input$condselect, input$timeselect, 
                      input$ctrlselect, input$timectrlselect, 
                      input$houseselect, input$geneselect,
                      input$plotctrl, input$plotlog, input$ploterr, input$plotgrp, input$plotori)
      proc_plot <<- p
      output$plot = renderPlot(p, res = 96)
  })
  
  update_ori = function()
  {
    if (input$condselect != 'NA' && input$timeselect != 'NA' && input$plotgrp %in% c(1, 2, 3))
    {
      choices = switch(input$plotgrp, 
                       '1'=c('Conditions x Times'=1, 'Times x Conditions'=2),
                       '2'=c('Target x Times'=3, 'Times x Target'=4),
                       '3'=c('Conditions x Target'=5, 'Target x Conditions'=6)
      )
      updateRadioButtons(inputId = 'plotori', 
                         choices = choices)
      showElement(id = 'plotori')
    }
    else
    {
      updateRadioButtons(inputId = 'plotori', 
                         choices = list('NULL'=0))
      hideElement(id = 'plotori')
    }
  }
  
  observeEvent(ignoreInit = T, input$plotgrp, 
  handlerExpr = {
    update_ori()
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
  observeEvent(input$timeincselect, ignoreNULL = FALSE, ignoreInit = T, handlerExpr = {
    if(is.null(input$timeincselect))
    {
      updateSelectizeInput(inputId = 'timeincselect', selected = input$timectrlselect)
      showNotification("Selection could not be empty.", type = 'error')
    }
    updateSelectInput(inputId = 'timectrlselect', choices = input$timeincselect)
  })
  observeEvent(input$condselect, handlerExpr = {
    hideFeedback("condselect")
  })
  observeEvent(input$condincselect, ignoreNULL = FALSE, ignoreInit = T, handlerExpr = {
    if(is.null(input$condincselect))
    {
      updateSelectizeInput(inputId = 'condincselect', selected = input$ctrlselect)
      showNotification("Selection could not be empty.", type = 'error')
    }
    updateSelectInput(inputId = 'ctrlselect', choices = input$condincselect)
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
  
  observeEvent(input$dilinfile, handlerExpr = {
    hideFeedback("dilinfile")
  })
  observeEvent(input$dilgeneselect, handlerExpr = {
    hideFeedback("dilgeneselect")
  })
  observeEvent(input$dilcpselect, handlerExpr = {
    hideFeedback("dilcpselect")
  })
  observeEvent(input$dilcdnaselect, handlerExpr = {
    hideFeedback("dilcdnaselect")
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
  
  output$download_dilplt <- downloadHandler(
    filename = function(){paste('dilution_lines.', input$dilpltfrmt, sep='')},
    content = function(file)
    {
      if (input$dilpltfrmt == 'pdf')
      {
        fnc = pdf
      }
      else if (input$dilpltfrmt == 'png')
      {
        fnc = function(...){png(..., units='in', res=300)}
      }
      fnc(file = file, width = input$dilwidth, height = input$dilheight)
      plot(proc_dilplot)
      dev.off()
    }
  )
}
