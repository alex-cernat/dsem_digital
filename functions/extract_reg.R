
extract_reg <- function(x) {
  nr_ids <- x$data_summary$overall$NClusters
  n <- x$summaries$Observations
  
  res <- x$parameters$unstandardized %>% 
    filter(str_detect(paramHeader, "ON")) %>%
    select(param, est, pval, sig, lower_2.5ci, upper_2.5ci)
  
  sel_warning <- x$warnings %>% 
    map(function(x) str_detect(x, "COEFFICIENTS GREATER THAN 1")) %>% 
    map(sum) %>% 
    unlist() %>% 
    as.logical()
  
  ess_warning <- x$output %>% 
    map_lgl(str_detect,
            "THE EFFECTIVE SAMPLE SIZE FOR THE POSTERIOR DISTRIBUTIO.+IS LESS") %>% 
    sum() 
  
  ess_warning <- ifelse(ess_warning > 0, TRUE, FALSE)
  
  ids_issues <- x$warnings[sel_warning] %>% 
    unlist() %>% 
    .[-c(1:3)] %>%
    str_c(collapse = " ") %>% 
    str_split(" ", simplify = T) %>% length() 
  
  # make data
  tibble(
    n = n,
    nr_ids = nr_ids,
    ids_issues = ids_issues,
    ess_warning = ess_warning
  ) %>% 
    cbind(res) %>% 
    select(param, everything()) %>% 
    mutate(param = str_to_lower(param))
  
}
