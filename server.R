library(shiny)
library(shinyjs)
library(shinyFeedback)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(ComplexHeatmap)

source(file.path('global_vars.R'),  local = TRUE)

# isLoaded = F
# isProcessed = F

makeDeltaDelta = function(data, cond_col, uconditions, times_col, utimes, rep_col, tech_col,
                          ctrl, timecntrl, housekeeping, target,
                          eff_matrix)
{
  # DEBUG
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
  g_sd <- function(vec)
  {
    vec <- as.numeric(vec)
    
    if (length(vec) < 2)
    {
      return(NA_real_)
    }
    
    if (any(!is.finite(vec)))
    {
      stop("Cannot calculate geometric SD: values contain NA, NaN, or Inf.")
    }
    
    if (any(vec <= 0))
    {
      stop("Cannot calculate geometric SD: all values must be greater than zero.")
    }
    
    m <- g_mean(vec)
    
    exp(
      sqrt(
        sum(log(vec / m)^2) /
          (length(vec) - 1)
      )
    )
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
  
  get_agg <- function(vec, islog)
  {
    vec <- as.numeric(vec)
    
    if (length(vec) == 0)
    {
      stop("Cannot summarize an empty vector.")
    }
    
    if (any(!is.finite(vec)))
    {
      stop("Cannot summarize values containing NA, NaN, or Inf.")
    }
    
    if (any(vec <= 0))
    {
      stop("Fold-change values must be greater than zero.")
    }
    
    res_mean <- g_mean(vec)
    res_min  <- min(vec)
    res_max  <- max(vec)
    ss       <- g_sd(vec)
    
    if (islog)
    {
      res_mean <- log2(res_mean)
      res_min  <- log2(res_min)
      res_max  <- log2(res_max)
      
      if (is.na(ss))
      {
        res_sdn <- NA_real_
        res_sdp <- NA_real_
      }
      else
      {
        log_sd <- log2(ss)
        
        res_sdn <- res_mean - log_sd
        res_sdp <- res_mean + log_sd
      }
      
      agg <- c(
        res_mean,
        res_min,
        res_max,
        res_sdn,
        res_sdp
      )
      
      names(agg) <- c(
        "log.mean",
        "log.min",
        "log.max",
        "log.-sd",
        "log.+sd"
      )
    }
    else
    {
      if (is.na(ss))
      {
        res_sdn <- NA_real_
        res_sdp <- NA_real_
      }
      else
      {
        res_sdn <- res_mean / ss
        res_sdp <- res_mean * ss
      }
      
      agg <- c(
        res_mean,
        res_min,
        res_max,
        res_sdn,
        res_sdp
      )
      
      names(agg) <- c(
        "mean",
        "min",
        "max",
        "-sd",
        "+sd"
      )
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

prepare_display_data <- function(
    data,
    cond_select,
    time_select,
    ctrl,
    timectrl,
    hide_control)
{
  if (hide_control == 2)
  {
    if (
      cond_select != "NA" &&
      time_select != "NA"
    )
    {
      data <- data %>%
        filter(
          Conditions != ctrl |
            Times != timectrl
        )
    }
    else if (cond_select != "NA")
    {
      data <- data %>%
        filter(Conditions != ctrl)
    }
    else if (time_select != "NA")
    {
      data <- data %>%
        filter(Times != timectrl)
    }
  }
  
  if (nrow(data) == 0)
  {
    stop(
      paste0(
        "No data remain after hiding the control/baseline group. ",
        "Show the control or include additional conditions/time points."
      )
    )
  }
  
  return(data)
}

makeOneHeatmap = function(data,
                          cond_select,
                          time_select,
                          ctrl,
                          timectrl,
                          addctrl,
                          heatlog,
                          heatgrp,
                          heatori)
{
  data <- prepare_display_data(
    data = data,
    cond_select = cond_select,
    time_select = time_select,
    ctrl = ctrl,
    timectrl = timectrl,
    hide_control = addctrl
  )
  
  genes = unique(data$Target)
  if (heatlog == 2)
  {
    val_var = 'log.mean'
  }
  else
  {
    val_var = 'mean'
  }
  data_mat = spread(data[, c('Target', val_var, 'Times', 'Conditions')], 
                    key='Target', value=val_var)
  annot = rowAnnotation(Conditions=data_mat$Conditions, Times=data_mat$Times)
  h = Heatmap(as.matrix(data_mat[, genes]), right_annotation=annot)
  return(h)
}

makeOnePlot = function(
    data,
    cond_select,
    time_select,
    ctrl,
    timectrl,
    houses,
    genes,
    addctrl,
    addlog,
    addmin,
    addgrp,
    plotori)
{
  
  data <- prepare_display_data(
    data = data,
    cond_select = cond_select,
    time_select = time_select,
    ctrl = ctrl,
    timectrl = timectrl,
    hide_control = addctrl
  )
  
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

is_selected_col <- function(x)
{
  !is.null(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    x != "" &&
    x != "NA"
}

validate_design_inputs <- function(
    data,
    rep_col,
    tech_col,
    cond_col,
    time_col,
    housekeeping,
    targets,
    included_conditions,
    included_times,
    control_condition,
    control_time)
{
  errors <- character()
  
  add_error <- function(msg)
  {
    errors <<- c(errors, msg)
  }
  
  # --------------------------------------------------
  # Basic data check
  # --------------------------------------------------
  
  if (!is.data.frame(data))
  {
    add_error("Input data is not a valid data frame.")
  }
  
  if (nrow(data) == 0)
  {
    add_error("Input data contains no rows.")
  }
  
  if (ncol(data) == 0)
  {
    add_error("Input data contains no columns.")
  }
  
  
  # --------------------------------------------------
  # Required selections
  # --------------------------------------------------
  
  if (!is_selected_col(rep_col))
  {
    add_error("Please select the biological replicate column.")
  }
  
  cond_enabled <- is_selected_col(cond_col)
  time_enabled <- is_selected_col(time_col)
  tech_enabled <- is_selected_col(tech_col)
  
  if (!cond_enabled && !time_enabled)
  {
    add_error("Please select at least a condition column or a time column.")
  }
  
  if (is.null(housekeeping) || length(housekeeping) == 0)
  {
    add_error("Please select at least one reference gene.")
  }
  
  if (is.null(targets) || length(targets) == 0)
  {
    add_error("Please select at least one target gene.")
  }
  
  
  # --------------------------------------------------
  # Selected columns must exist
  # --------------------------------------------------
  
  selected_columns <- c(
    if (is_selected_col(rep_col)) rep_col,
    if (tech_enabled) tech_col,
    if (cond_enabled) cond_col,
    if (time_enabled) time_col,
    housekeeping,
    targets
  )
  
  missing_columns <- setdiff(
    selected_columns,
    colnames(data)
  )
  
  if (length(missing_columns) > 0)
  {
    add_error(
      paste0(
        "These selected columns do not exist in the input data: ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
  
  
  # --------------------------------------------------
  # Reference and target genes must not overlap
  # --------------------------------------------------
  
  overlap_genes <- intersect(
    housekeeping,
    targets
  )
  
  if (length(overlap_genes) > 0)
  {
    add_error(
      paste0(
        "The same gene cannot be both reference and target: ",
        paste(overlap_genes, collapse = ", ")
      )
    )
  }
  
  
  # --------------------------------------------------
  # Metadata columns must be different
  # --------------------------------------------------
  
  metadata_cols <- c(
    if (is_selected_col(rep_col)) rep_col,
    if (tech_enabled) tech_col,
    if (cond_enabled) cond_col,
    if (time_enabled) time_col
  )
  
  if (anyDuplicated(metadata_cols))
  {
    add_error(
      paste0(
        "Biological replicate, technical replicate, condition, ",
        "and time must use different columns."
      )
    )
  }
  
  
  # --------------------------------------------------
  # Gene columns must not also be metadata columns
  # --------------------------------------------------
  
  all_genes <- c(
    housekeeping,
    targets
  )
  
  metadata_gene_overlap <- intersect(
    metadata_cols,
    all_genes
  )
  
  if (length(metadata_gene_overlap) > 0)
  {
    add_error(
      paste0(
        "These columns were selected both as metadata and gene values: ",
        paste(metadata_gene_overlap, collapse = ", ")
      )
    )
  }
  
  
  # --------------------------------------------------
  # Condition checks
  # --------------------------------------------------
  
  if (cond_enabled)
  {
    if (is.null(included_conditions) ||
        length(included_conditions) == 0)
    {
      add_error("Please select at least one condition to include.")
    }
    
    if (is.null(control_condition) ||
        control_condition == "")
    {
      add_error("Please select a control condition.")
    }
    else
    {
      if (!(control_condition %in% included_conditions))
      {
        add_error(
          "The control condition must be included in the selected conditions."
        )
      }
    }
  }
  
  
  # --------------------------------------------------
  # Time checks
  # --------------------------------------------------
  
  if (time_enabled)
  {
    if (is.null(included_times) ||
        length(included_times) == 0)
    {
      add_error("Please select at least one time point to include.")
    }
    
    if (is.null(control_time) ||
        control_time == "")
    {
      add_error("Please select the baseline/T0 time point.")
    }
    else
    {
      if (!(control_time %in% included_times))
      {
        add_error(
          "The baseline/T0 time point must be included in the selected time points."
        )
      }
    }
  }
  
  
  # --------------------------------------------------
  # Return
  # --------------------------------------------------
  
  list(
    ok = length(errors) == 0,
    errors = unique(errors)
  )
}

get_analysis_subset <- function(
    data,
    cond_col,
    time_col,
    included_conditions,
    included_times)
{
  keep <- rep(TRUE, nrow(data))
  
  if (is_selected_col(cond_col))
  {
    keep <- keep &
      data[[cond_col]] %in% included_conditions
  }
  
  if (is_selected_col(time_col))
  {
    keep <- keep &
      data[[time_col]] %in% included_times
  }
  
  data[
    keep,
    ,
    drop = FALSE
  ]
}

validate_ct_values <- function(
    data,
    cond_col,
    time_col,
    included_conditions,
    included_times,
    housekeeping,
    targets,
    warning_low = 5,
    warning_high = 40)
{
  errors <- character()
  warnings <- character()
  
  add_error <- function(msg)
  {
    errors <<- c(errors, msg)
  }
  
  add_warning <- function(msg)
  {
    warnings <<- c(warnings, msg)
  }
  
  
  # --------------------------------------------------
  # Only validate rows that will actually be analysed
  # --------------------------------------------------
  
  analysis_data <- get_analysis_subset(
    data = data,
    cond_col = cond_col,
    time_col = time_col,
    included_conditions = included_conditions,
    included_times = included_times
  )
  
  
  if (nrow(analysis_data) == 0)
  {
    add_error(
      "No rows remain after applying the selected conditions/time points."
    )
    
    return(
      list(
        ok = FALSE,
        errors = errors,
        warnings = warnings
      )
    )
  }
  
  
  # --------------------------------------------------
  # Ct columns
  # --------------------------------------------------
  
  genes <- unique(
    c(
      housekeeping,
      targets
    )
  )
  
  
  for (gene in genes)
  {
    x <- analysis_data[[gene]]
    
    
    # -----------------------------
    # Must be numeric
    # -----------------------------
    
    if (!is.numeric(x))
    {
      x_text <- trimws(as.character(x))
      
      x_numeric <- suppressWarnings(
        as.numeric(x_text)
      )
      
      bad_text <- !is.na(x_text) &
        x_text != "" &
        is.na(x_numeric)
      
      if (any(bad_text))
      {
        bad_values <- unique(
          x_text[bad_text]
        )
        
        add_error(
          paste0(
            "Gene '",
            gene,
            "' contains non-numeric Ct/Cq value(s): ",
            paste(
              head(bad_values, 5),
              collapse = ", "
            ),
            "."
          )
        )
        
        next
      }
      
      x <- x_numeric
    }
    
    
    # -----------------------------
    # Missing / non-finite values
    # -----------------------------
    
    bad_missing <- is.na(x) |
      is.nan(x) |
      !is.finite(x)
    
    if (any(bad_missing))
    {
      add_error(
        paste0(
          "Gene '",
          gene,
          "' contains ",
          sum(bad_missing),
          " missing or non-finite Ct/Cq value(s)."
        )
      )
    }
    
    
    # -----------------------------
    # Zero or negative Ct
    # -----------------------------
    
    bad_nonpositive <- !bad_missing &
      x <= 0
    
    if (any(bad_nonpositive))
    {
      add_error(
        paste0(
          "Gene '",
          gene,
          "' contains ",
          sum(bad_nonpositive),
          " Ct/Cq value(s) <= 0."
        )
      )
    }
    
    
    # -----------------------------
    # Unusual but not necessarily invalid
    # -----------------------------
    
    usable <- x[
      is.finite(x) &
        !is.na(x)
    ]
    
    if (length(usable) > 0)
    {
      unusual <- usable < warning_low |
        usable > warning_high
      
      if (any(unusual))
      {
        unusual_values <- usable[unusual]
        
        add_warning(
          paste0(
            "Gene '",
            gene,
            "' contains ",
            length(unusual_values),
            " unusual Ct/Cq value(s) outside ",
            warning_low,
            "-",
            warning_high,
            ". Range observed: ",
            round(min(unusual_values), 2),
            " to ",
            round(max(unusual_values), 2),
            "."
          )
        )
      }
    }
  }
  
  
  list(
    ok = length(errors) == 0,
    errors = unique(errors),
    warnings = unique(warnings)
  )
}


# Define server logic ----
server <- function(input, output, session) {
  # disable("makeplot")
  state <- reactiveValues(
    dilution_data    = NULL,
    dilution_summary = NULL,
    processed_data   = NULL,
    main_plot        = NULL,
    heatmap           = NULL,
    dilution_plot    = NULL
  )
  output$diltabres <- renderTable({
    req(state$dilution_data)
    state$dilution_data
  })
  
  output$dilplot <- renderPlot({
    req(state$dilution_plot)
    print(state$dilution_plot)
  }, res = 96)
  
  output$plot <- renderPlot({
    req(state$main_plot)
    print(state$main_plot)
  }, res = 96)
  
  output$heatmap <- renderPlot({
    req(state$heatmap)
    ComplexHeatmap::draw(state$heatmap)
  }, res = 96)
  
  
  hideTab(inputId = 'mainpagetab', target = plot_tab_title)
  hideTab(inputId = 'mainpagetab', target = dilution_tab_title)
  hideTab(inputId = 'mainpagetab', target = heatmap_tab_title)
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
      # DEBUG
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
    
    # DEBUG
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
    
    state$dilution_data <- loadeddata
  })
  
  processDil <- function()
  {
    req(state$dilution_data)
    
    dil_data <- state$dilution_data
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
    state$dilution_summary <- dil_data %>%
      group_by_at(col_list) %>%
      summarise(
        eff = calc_slope(
          cur_data()[, input$dilcdnaselect],
          cur_data()[, input$dilcpselect]
        )
      ) %>%
      as.data.frame()
    dil_sum <- state$dilution_summary
    
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
    
    p <- ggplot(dil_data, aes(x=cDNA.Input, y=Cycle, col=Gene)) +
      geom_point() + 
      scale_x_continuous(trans='log10') + 
      geom_smooth(method = "lm", se=F, formula = y ~ x) + 
      xlab('cDNA Input') + ylab('Cycles') +
      theme(legend.position = c(0.8, 0.8)) +
      scale_color_discrete(labels=leg)
    
    state$dilution_plot <- p
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

    processAndPlot <- function()
    {
      data_input <- my_tab()
      req(data_input)
      
      validation <- validate_design_inputs(
        data = data_input,
        
        rep_col = input$repselect,
        tech_col = input$techselect,
        
        cond_col = input$condselect,
        time_col = input$timeselect,
        
        housekeeping = input$houseselect,
        targets = input$geneselect,
        
        included_conditions = input$condincselect,
        included_times = input$timeincselect,
        
        control_condition = input$ctrlselect,
        control_time = input$timectrlselect
      )
      
      if (!validation$ok)
      {
        for (msg in validation$errors)
        {
          showNotification(
            msg,
            type = "error",
            duration = NULL
          )
        }
        
        return(invisible(FALSE))
      }
      
      analysis_data <- get_analysis_subset(
        data = data_input,
        
        cond_col = input$condselect,
        time_col = input$timeselect,
        
        included_conditions = input$condincselect,
        included_times = input$timeincselect
      )
      
      ct_validation <- validate_ct_values(
        data = data_input,
        
        cond_col = input$condselect,
        time_col = input$timeselect,
        
        included_conditions = input$condincselect,
        included_times = input$timeincselect,
        
        housekeeping = input$houseselect,
        targets = input$geneselect
      )
      
      
      # Stop on hard errors
      if (!ct_validation$ok)
      {
        for (msg in ct_validation$errors)
        {
          showNotification(
            msg,
            type = "error",
            duration = NULL
          )
        }
        
        return(invisible(FALSE))
      }
      
      ct_genes <- unique(
        c(
          input$houseselect,
          input$geneselect
        )
      )
      
      for (gene in ct_genes)
      {
        analysis_data[[gene]] <-
          suppressWarnings(
            as.numeric(
              as.character(
                analysis_data[[gene]]
              )
            )
          )
      }
      
      # Show warnings, but continue
      if (length(ct_validation$warnings) > 0)
      {
        for (msg in ct_validation$warnings)
        {
          showNotification(
            msg,
            type = "warning",
            duration = 10
          )
        }
      }
      
      # --------------------------------------------------
      # Efficiency validation/calculation starts here
      # --------------------------------------------------

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
      req(state$dilution_summary)
      
      dil_sum <- state$dilution_summary
      
      for (g in all_genes())
      {
        c_check <- sum(dil_sum[, 1] == g) == 1
        
        if (!c_check)
        {
          showNotification(
            paste(
              "Dilution method efficiency is not provided for ",
              g,
              sep = ""
            ),
            type = "error"
          )
        }
      }
      
      for (g in all_genes())
      {
        c_check <- sum(dil_sum[, 1] == g) == 1
        req(c_check)
      }
    }
    
      eff <- 2
      
      eff_matrix <- matrix(
        eff,
        nrow(analysis_data),
        length(all_genes())
      )
      
      colnames(eff_matrix) <- all_genes()
    if (input$effradio == 1)
    {
      for (g in all_genes())
      {
        eff_matrix[, g] <-
          analysis_data[, input[[g]]]
      }
    }
    if (input$effradio == 2)
    {
      req(state$dilution_summary)
      
      dil_sum <- state$dilution_summary
      
      for (g in all_genes())
      {
        eff_matrix[, g] <-
          state$dilution_summary[
            state$dilution_summary[, 1] == g,
            "eff"
          ]
      }
    }
    
    data = makeDeltaDelta(analysis_data, input$condselect, input$condincselect, input$timeselect, input$timeincselect,
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
    
    state$processed_data <- data
    p = makeOnePlot(data, input$condselect, input$timeselect, input$ctrlselect, input$timectrlselect, 
                    input$houseselect, input$geneselect,
                    input$plotctrl, input$plotlog, input$ploterr, input$plotgrp, input$plotori)
    
    
    state$main_plot <- p
    
    h = makeOneHeatmap(data, 
                       input$condselect, input$timeselect, input$ctrlselect, input$timectrlselect,
                       input$heatctrl, input$heatlog, input$heatgrp, input$heatori)
    state$heatmap <- h
    
    
    
    showTab(inputId = 'mainpagetab', target = plot_tab_title)
    showTab(inputId = 'mainpagetab', target = heatmap_tab_title)
    updateNavbarPage(inputId = 'mainpagetab', selected = plot_tab_title)
    
  }
  
  observeEvent(ignoreInit = T, input$processb, 
    handlerExpr = {
      processAndPlot()
  })
  
  observeEvent(
    ignoreInit = TRUE,
    c(
      input$plotctrl,
      input$plotlog,
      input$ploterr,
      input$plotgrp,
      input$plotori
    ),
    handlerExpr = {
      
      req(state$processed_data)
      
      p <- makeOnePlot(
        state$processed_data,
        input$condselect,
        input$timeselect,
        input$ctrlselect,
        input$timectrlselect,
        input$houseselect,
        input$geneselect,
        input$plotctrl,
        input$plotlog,
        input$ploterr,
        input$plotgrp,
        input$plotori
      )
      
      state$main_plot <- p
    }
  )
  
  observeEvent(
    ignoreInit = TRUE,
    c(
      input$heatctrl,
      input$heatlog,
      input$heatgrp,
      input$heatori
    ),
    handlerExpr = {
      
      req(state$processed_data)
      
      h <- makeOneHeatmap(
        state$processed_data,
        input$condselect,
        input$timeselect,
        input$ctrlselect,
        input$timectrlselect,
        input$heatctrl,
        input$heatlog,
        input$heatgrp,
        input$heatori
      )
      
      state$heatmap <- h
    }
  )
  
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
  
  observeEvent(input$timeselect, {
    
    timecol <- input$timeselect
    
    if (is_selected_col(timecol))
    {
      cid <- which(
        colnames(my_tab()) == timecol
      )
      
      vals <- unique(
        my_tab()[, cid]
      )
      
      updateSelectizeInput(
        inputId = "timeincselect",
        choices = vals,
        selected = vals
      )
      
      updateSelectInput(
        inputId = "timectrlselect",
        choices = vals
      )
    }
    else
    {
      updateSelectizeInput(
        inputId = "timeincselect",
        choices = character(0),
        selected = character(0)
      )
      
      updateSelectInput(
        inputId = "timectrlselect",
        choices = character(0)
      )
    }
  })
  observeEvent(
    input$timeincselect,
    ignoreNULL = FALSE,
    ignoreInit = TRUE,
    handlerExpr = {
      
      if (is.null(input$timeincselect))
      {
        updateSelectizeInput(
          inputId = "timeincselect",
          selected = input$timectrlselect
        )
        
        showNotification(
          "Selection could not be empty.",
          type = "error"
        )
        
        return()
      }
      
      updateSelectInput(
        inputId = "timectrlselect",
        choices = input$timeincselect
      )
    }
  )

  observeEvent(input$condselect, {
    
    condcol <- input$condselect
    
    if (is_selected_col(condcol))
    {
      cid <- which(
        colnames(my_tab()) == condcol
      )
      
      vals <- unique(
        my_tab()[, cid]
      )
      
      updateSelectizeInput(
        inputId = "condincselect",
        choices = vals,
        selected = vals
      )
      
      updateSelectInput(
        inputId = "ctrlselect",
        choices = vals
      )
    }
    else
    {
      updateSelectizeInput(
        inputId = "condincselect",
        choices = character(0),
        selected = character(0)
      )
      
      updateSelectInput(
        inputId = "ctrlselect",
        choices = character(0)
      )
    }
  })
  observeEvent(
    input$condincselect,
    ignoreNULL = FALSE,
    ignoreInit = TRUE,
    handlerExpr = {
      
      if (is.null(input$condincselect))
      {
        updateSelectizeInput(
          inputId = "condincselect",
          selected = input$ctrlselect
        )
        
        showNotification(
          "Selection could not be empty.",
          type = "error"
        )
        
        return()
      }
      
      updateSelectInput(
        inputId = "ctrlselect",
        choices = input$condincselect
      )
    }
  )
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
      req(state$processed_data)
      
      write.csv(
        state$processed_data,
        file,
        row.names = FALSE
      )
    }
  )
  
  
  
  download_figure <- function(file, plt, frmt, width, height)
  {
    if (frmt == "pdf")
    {
      grDevices::pdf(
        file = file,
        width = width,
        height = height
      )
    }
    else if (frmt == "png")
    {
      grDevices::png(
        filename = file,
        width = width,
        height = height,
        units = "in",
        res = 300
      )
    }
    else
    {
      stop("Unsupported output format.")
    }
    
    on.exit(grDevices::dev.off(), add = TRUE)
    
    if (inherits(plt, c("Heatmap", "HeatmapList")))
    {
      ComplexHeatmap::draw(plt)
    }
    else
    {
      print(plt)
    }
  }
  
  output$download_plt <- downloadHandler(
    filename = function()
    {
      paste0(
        "delta_delta_bar.",
        input$pltfrmt
      )
    },
    
    content = function(file)
    {
      req(state$main_plot)
      
      download_figure(
        file,
        state$main_plot,
        input$pltfrmt,
        input$width,
        input$height
      )
    }
  )
  
  output$download_heat <- downloadHandler(
    filename = function(){paste('delta_delta_heat.', input$heatfrmt, sep='')},
    content = function(file)
    {
      req(state$heatmap)
      
      download_figure(
        file,
        state$heatmap,
        input$heatfrmt,
        input$heatwidth,
        input$heatheight
      )
    }
  )
  
  output$download_dilplt <- downloadHandler(
    filename = function(){paste('dilution_lines.', input$dilpltfrmt, sep='')},
    content = function(file)
    {
      req(state$dilution_plot)
      
      download_figure(
        file,
        state$dilution_plot,
        input$dilpltfrmt,
        input$dilwidth,
        input$dilheight
      )
    }
  )
}
