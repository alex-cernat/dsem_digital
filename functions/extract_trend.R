
extract_trend <- function(x) {
  df <- x$parameters$stdyx.standardized %>%
    filter(
      param == "DAY_BEFORE" & str_detect(paramHeader, "ON")
    ) %>%
    select(-posterior_sd, -sig, -BetweenWithin, -param)
  
  df$param <- x$parameters$unstandardized$param[1]
  
  df %>% 
    mutate_all(~str_to_lower(.))
}