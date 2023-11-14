factoriser <- function(data, codelist = items, delabel = TRUE) {
  x <- names(data)
  y <- codelist %>%
    filter(id %in% x) %>%
    filter(categorical == 1) %>%
    select(id,value_labels)
  
  if(length(y$id) == 0) {
    return(data)
  }
  
  for (i in 1:length(y$id)){
    ct <- y %>%
      slice(i) %>%
      unnest(cols = c(value_labels)) %>% 
      unique()

   
    name1 <- y$id[[i]] 
    name2 <- paste0(y$id[[i]],"cd")
    
    
    labs <- ct[["codevalue"]]

    if (all(ct$datatype == "integer")) {
      # convert my_column to numeric
     labs <- as.numeric(labs)
    }
    names(labs) <- ct[["codetext"]]
    
    if(!all(is.na(data[[name2]]))){
    data <- data %>%
      #mutate_at(name2, as.character) %>%
      mutate_at(name2, haven::labelled, labels = labs) %>%
      mutate(!!name1 := as_factor(!!sym(name2), ordered = FALSE)) 
    
    if (delabel == TRUE && all(ct$datatype == "integer") ) data[[name2]] <- as.integer(data[[name2]])
    
    }
  }
  

 
  return(data)
}


labeliser <- function(data, codelist = items){
  x <- names(data)
  labels <- codelist %>%
    filter(id %in% x) %>%
    select(id,label) %>%
    spread(id, label) %>%
    as.list()
  
  labelled::var_label(data) <- labels
  
  return(data)
}

pick <- function(db, name) {
  db %>% dplyr::filter(id == name) %>% purrr::pluck("data",1)
}



###################
# Functions for tables and plots
##################
s_summary <- function(x, .N_col) {
  if (is.numeric(x)) {
    in_rows(
      "n" = rcell(sum(!is.na(x)), format = "xx"),
      "Mean (sd)" = rcell(c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)), format = "xx.xx (xx.xx)"),
      "IQR" = rcell(IQR(x, na.rm = TRUE), format = "xx.xx"),
      "min - max" = rcell(range(x, na.rm = TRUE), format = "xx.xx - xx.xx")
    )
  } else if (is.factor(x)) {
    in_rows(.list = lapply(as.list(table(x)), function(xi) rcell(xi * c(1, 1/.N_col), format = "xx (xx.xx%)")))
  } else {
    in_rows(.list = as.list(table(factor(x))))
  }
}


f_anplot1 <- function(data, var){
  
  var1 <- ensym(var)
  label_ <- labelled::var_label(data[[var]])
  
  df <- data %>% 
    mutate(studyday = as.numeric(studyday))
  # Plotting the time series
  plot <- ggplot(df, aes(x = studyday, y = {{var1}}, group = rantrt, color = rantrt)) +
    geom_point() +
    labs(x = "Study Day", y = label_) +
    theme_minimal() +
    facet_wrap(~ subjectid, ncol = 2) +
    scale_color_manual(name = "Treatment", values = c("Baseline" = "black", "Burst stimulation" = "red", "Sham" = "blue")) +
    theme(legend.position="bottom")
  
  return(plot)
}




f_antbl_period <- function(data, var) {
  label_ <- labelled::var_label(data[[var]])
  lyt <- rtables::basic_table(subtitles = c("Full analysis set", label_)) %>%
    split_cols_by("pair") %>% 
    split_cols_by("rantrt") %>% 
    split_rows_by("subjectid") %>% 
    analyze(var , afun = s_summary)
  
  prom_tbl_all <- build_table(lyt, data) %>% 
    tt_to_flextable() %>% 
    autofit()
  
  return(prom_tbl_all)
} 


f_antbl2_period <- function(data, var) {
  label_ <- labelled::var_label(data[[var]])
  lyt <- rtables::basic_table(subtitles = c("Full analysis set", label_)) %>%
    split_cols_by("pair") %>% 
    split_cols_by("rantrt") %>% 
    split_rows_by("subjectid") %>% 
    analyze(var , afun = mean, var_label = "")
  
  prom_tbl_all <- build_table(lyt, data) %>% 
    tt_to_flextable() %>% 
    autofit()
  
  return(prom_tbl_all)
} 



f_antbl_overall <- function(data, var) {
  var_ = ensym(var)
  label_ <- labelled::var_label(data[[var]])
  
  diff_tbl <- data %>% 
    filter(rantrt != "Baseline") %>% 
    group_by(subjectid, pair, rantrt) %>% 
    summarise(mean = mean({{var_}}, na.rm = TRUE), .groups = "drop_last") %>% 
    pivot_wider(names_from = rantrt, values_from = mean) %>% 
    mutate(diff = `Burst stimulation` - Sham) %>% 
    mutate(subjectid = factor(subjectid)) %>% 
    group_by(subjectid) 
  
  t.test <- t.test(diff_tbl$diff, alternative = "two.sided")
  
  diff_tbl2 <- diff_tbl %>% 
    pivot_wider(id_cols = subjectid, names_from = pair, values_from = diff, values_fn = ~round(.x, digits = 3)) %>% 
    rowwise() %>% 
    mutate(Overall = mean(c(`Pair 1`, `Pair 2`, `Pair 3`), na.rm = TRUE),
           Overall = round(Overall, digits = 3)) %>% 
    ungroup() %>% 
    knitr::kable(caption = label_, col.names = c("Subject ID", "Difference Pair 1", "Difference Pair 2", "Difference Pair 3", "Overall difference"))
  
  return(list(tbl = diff_tbl2, diff_tbl = diff_tbl, t.test = t.test))
} 



f_mixed <- function(data, var) {
  var_ = ensym(var)
  label_ <- labelled::var_label(data[[var]])
  
  
  data_ <- data %>% 
    filter(rantrt != "Baseline") %>% 
    mutate(rantrt = factor(rantrt),
           pair = factor(pair))
  

  fit7 <- rlang::inject(lme4::lmer(!!var_ ~ rantrt + pair + (1|subjectid) + (1|subjectid:pair) + (1|subjectid:rantrt), data = data_))



  by_subj <- marginaleffects::avg_comparisons(fit7, variables = list(rantrt = c("Sham", "Burst stimulation")), newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                                                                                                                  grid_type = "counterfactual"), by = "subjectid")  %>% 
    broom::tidy() %>% 
    select(-term, -contrast, -starts_with("predicted"), -statistic) 
    
  overall <- marginaleffects::avg_comparisons(fit7, 
                                              variables = list(rantrt = c("Sham", "Burst stimulation")), 
                                              newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                              grid_type = "counterfactual")) %>% 
    broom::tidy() %>% 
    select(-term, -contrast, -starts_with("predicted"), -statistic) 
  
  by_trt <- marginaleffects::avg_predictions(fit7, 
                                   variables = list(rantrt = c("Sham", "Burst stimulation")), 
                                   newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                                      grid_type = "counterfactual")) %>% 
    broom::tidy() %>% 
    select(-statistic) 

  return(list(by_subj = by_subj, overall = overall, fit = fit7, by_trt = by_trt))
}
f_mixed2 <- function(data, var) {
  var_ = ensym(var)
  label_ <- labelled::var_label(data[[var]])
  
  
  data_ <- data %>% 
    filter(rantrt != "Baseline") %>% 
    mutate(rantrt = factor(rantrt),
           pair = factor(pair))
  
  
  fit7 <- rlang::inject(lme4::lmer(!!var_ ~ rantrt + pair  + (1|subjectid:pair) + (1|subjectid:rantrt), data = data_))
  
  
  
  by_subj <- marginaleffects::avg_comparisons(fit7, variables = list(rantrt = c("Sham", "Burst stimulation")), newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                                                                                                                  grid_type = "counterfactual"), by = "subjectid")  %>% 
    broom::tidy() %>% 
    select(-term, -contrast, -starts_with("predicted"), -statistic) 
  
  overall <- marginaleffects::avg_comparisons(fit7, variables = list(rantrt = c("Sham", "Burst stimulation")), newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                                                                                                                  grid_type = "counterfactual")) %>% 
    broom::tidy() %>% 
    select(-term, -contrast, -starts_with("predicted"), -statistic) 
  
  by_trt <- marginaleffects::avg_predictions(fit7, 
                                             variables = list(rantrt = c("Sham", "Burst stimulation")), 
                                             newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                                                grid_type = "counterfactual")) %>% 
    broom::tidy() %>% 
    select(-statistic) 
  
  return(list(by_subj = by_subj, overall = overall, fit = fit7, by_trt = by_trt))
}

f_mixed3 <- function(data, var) {
  var_ = ensym(var)
  label_ <- labelled::var_label(data[[var]])
  
  
  data_ <- data %>% 
    filter(rantrt != "Baseline") %>% 
    mutate(rantrt = factor(rantrt),
           pair = factor(pair))
  
  
  fit7 <- rlang::inject(lme4::lmer(!!var_ ~ rantrt + pair  +  (1|subjectid:rantrt), data = data_))
  
  
  
  by_subj <- marginaleffects::avg_comparisons(fit7, variables = list(rantrt = c("Sham", "Burst stimulation")), newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                                                                                                                  grid_type = "counterfactual"), by = "subjectid")  %>% 
    broom::tidy() %>% 
    select(-term, -contrast, -starts_with("predicted"), -statistic) 
  
  overall <- marginaleffects::avg_comparisons(fit7, variables = list(rantrt = c("Sham", "Burst stimulation")), newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                                                                                                                  grid_type = "counterfactual")) %>% 
    broom::tidy() %>% 
    select(-term, -contrast, -starts_with("predicted"), -statistic) 
  
  by_trt <- marginaleffects::avg_predictions(fit7, 
                                             variables = list(rantrt = c("Sham", "Burst stimulation")), 
                                             newdata = datagrid(rantrt = c("Sham", "Burst stimulation"), 
                                                                grid_type = "counterfactual")) %>% 
    broom::tidy() %>% 
    select(-statistic) 
  
  return(list(by_subj = by_subj, overall = overall, fit = fit7, by_trt = by_trt))
}


stats_exec <- function(f, data, var, group, ...){
  rlang::exec(f, data, var, group, !!!(...))
}


