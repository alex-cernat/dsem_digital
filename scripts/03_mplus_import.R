
# Code for applying dsem quasi-simplex to estimate reliability in digital trace 
# data


# Admin ------------


# install.packages("tidyverse")
# install.packages("MplusAutomation")

library(tidyverse)
library(MplusAutomation)

# import data

w1_30d2 <- read_rds("./data/w1_30d2.rds")
data_full <- read_rds("./data/data_full.rds")



# Import Mplus outputs -----------------


path <- list.files("./mplus/auto/", 
                      full.names = TRUE, 
                      pattern = "out") %>% 
  str_subset("m1") 

out_m1 <- path %>% 
  map(MplusAutomation::readModels) 



issues <- tibble(
  path = path,
  conv_issue = map_lgl(out_m1, function(x) x$tech8$psr%>% 
                         slice_tail(n = 1) %>% 
                         .[["psr"]] > 1.099
  )
)


count(issues, conv_issue)

issues %>% 
  filter(conv_issue) %>% 
  pull(path)

# issues %>% 
#   filter(conv_issue) %>% 
#   pull(path) %>% 
#   map(MplusAutomation::runModels, logFile = "./mplus/auto/log_retry.txt")


extract_r2 <- function(x) {
  nr_ids <- x$data_summary$overall$NClusters
  n <- x$summaries$Observations
  #res <- x$parameters$r2[1, c(1, 2, 5, 6)]

  res <- x$parameters$stdyx.standardized %>% 
    filter(str_detect(paramHeader, "BY")) %>%
    select(param, est, lower_2.5ci, upper_2.5ci)
  
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

res_rel <- out_m1[!(issues$conv_issue)] %>% 
  map_df(extract_r2) %>% 
  mutate(time = issues$path[issues$conv_issue == F] %>% 
           str_detect("time")
  )

res_rel



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




trend_effects <- out_m1[issues$conv_issue == FALSE &
                          str_detect(issues$path, "time")] %>% 
  map_dfr(extract_trend)

# any trends are significant?
trend_effects %>% 
  filter(pval < 0.05)



# extract reliabilities ----------------------------
# res_m1 <- out_m1[!issues$conv_issue] %>% 
#   map_dfr(extract_r2)

# res_m1 %>% 
#   filter(time_cntrl == T) %>% 
#   arrange(ids_used)


res_m1 <- res_rel %>% 
  separate(param, into = c("var", "method1", "method2"), sep = "_") %>% 
  mutate(method = case_when(method1 == "d" ~ "Log duration",
                             method1 == "c" & method2 == "l2" ~ "Log count",
                             TRUE ~ "Dummy"),
         ids_used = nr_ids - ids_issues) %>% 
  select(var, method, everything(), -method1, -method2)

  
res_m1  %>% 
  filter(method == "Dummy")

## get average reliabilities
res_m1 %>% 
  group_by(var, time) %>% 
  summarise(rel = mean(est))

res_m1 %>% 
  group_by(method) %>% 
  summarise(rel = mean(est))



# make graph with confidence interval
res_m1 %>% 
  ggplot(aes(x = var, y = est, color = time)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower_2.5ci, ymax = upper_2.5ci), 
                position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Variable", y = "Estimated reliability", color = "Time control") +
  facet_wrap(~method) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  ylim(0, 1)


res_m1 %>% 
  filter(time == T) %>% 
  ggplot(aes(est, var, color = method)) +
  geom_point(position = position_dodge(width = -0.5), size = 3) +
  geom_errorbar(aes(xmin = lower_2.5ci, xmax = upper_2.5ci), 
                position = position_dodge(width = -0.5), width = 0) +
  labs(x = "Variable", y = "Estimated reliability", color = "Method") +
  theme_bw() +
  theme(text = element_text(size = 16))



# Extract m2 results ----------------------------


path_m2 <- list.files("./mplus/auto/", 
                    full.names = TRUE, 
                    pattern = "out") %>% 
  str_subset("m2")


out_m2 <- path_m2 %>%
  map(MplusAutomation::readModels)

# get models with issues
issues_m2 <- tibble(
  path = path_m2,
  conv_issue = map_lgl(out_m2, function(x) x$tech8$psr%>% 
                         slice_tail(n = 1) %>% 
                         .[["psr"]] > 1.099
  )
)
issues_m2



extract_reg <- function(x) {
  nr_ids <- x$data_summary$overall$NClusters
  n <- x$summaries$Observations
  
  res <- x$parameters$unstandardized %>% 
    filter(str_detect(paramHeader, "ON")) %>%
    select(param, est, pval, lower_2.5ci, upper_2.5ci)
  
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

names(out_m2) <- path_m2

res_reg <- out_m2 %>% 
  map_df(extract_reg, .id = "model") %>% 
  mutate(model = str_remove_all(model, ".+m2_|_time.out"))


res_reg






res_m2 <- res_reg %>% 
  separate(model, into = c("var", "method1", "method2"), sep = "_") %>% 
  mutate(method = case_when(method1 == "d" ~ "Duration",
                            method1 == "c" & method2 == "l2" ~ "Count",
                            TRUE ~ "Dummy"),
         pred = case_when(param == "female" ~ "Female",
                          param == "age" ~ "Age",
                          param == "edu_med" ~ "Medium education",
                          param == "edu_high" ~ "High education",
                          TRUE ~ "Number of devices"),
         ids_used = nr_ids - ids_issues) %>% 
  select(var, method, everything(), -method1, -method2)





res_m2 %>% 
  mutate(sig = ifelse(pval < 0.05, "p < 0.05", "n.s."),
         pred = fct_rev(pred)) %>%
  ggplot(aes(est, pred, color = method, alpha = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = -0.5), size = 3) +
  geom_errorbar(aes(xmin = lower_2.5ci, xmax = upper_2.5ci), 
                position = position_dodge(width = -0.5), width = 0) +
  labs(x = "Regression coefficient", 
       y = "Predictor", 
       color = "Method",
       alpha = "Significant") +
  facet_wrap(~var, ncol = 2) +
  theme_bw() +
  theme(text = element_text(size = 16), legend.position = "bottom") 



# rerun problematic models --------------

paths_to_run <- issues$path[issues$conv_issue]
paths_to_run2 <- path[!issues$conv_issue] %>% .[res_rel$ess_warning]
to_run <- c(paths_to_run, paths_to_run2, path_m2)
