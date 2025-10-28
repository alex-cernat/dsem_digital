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
  res <- x$parameters$r2[1, c(1, 2, 5, 6)]

  sel_warning <- x$warnings %>% 
    map(function(x) str_detect(x, "COEFFICIENTS GREATER THAN 1")) %>% 
    map(sum) %>% 
    unlist() %>% 
    as.logical()
  
    ids_issues <- x$warnings[sel_warning] %>% 
      unlist() %>% 
      .[-c(1:3)] %>%
    str_c(collapse = " ") %>% 
    str_split(" ", simplify = T) %>% length() 
  
  # make data
  tibble(
    n = n,
    nr_ids = nr_ids,
    ids_issues = ids_issues
  ) %>% 
    cbind(res) %>% 
    select(param, everything()) %>% 
    mutate(param = str_to_lower(param))
  
}

x <- out_m1[str_detect(issues$path, "time")][[10]]


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
res_m1 <- out_m1[!issues$conv_issue] %>% 
  map_dfr(extract_r2)

# res_m1 %>% 
#   filter(time_cntrl == T) %>% 
#   arrange(ids_used)


res_m1 <- res_m1 %>% 
  separate(param, into = c("var", "method1", "method2"), sep = "_") %>% 
  mutate(method = case_when(method1 == "d" ~ "Log Duration",
                             method1 == "c" & method2 == "l2" ~ "Log count",
                             TRUE ~ "Dummy"),
         ids_used = nr_ids - ids_issues,
         time_cntrl = str_detect(issues$path[!issues$conv_issue], "time")) %>% 
  select(var, method, everything(), -method1, -method2)

  
res_m1  %>% 
  filter(method == "Dummy")

## get average reliabilities
res_m1 %>% 
  group_by(var, time_cntrl) %>% 
  summarise(rel = mean(est))

res_m1 %>% 
  group_by(method) %>% 
  summarise(rel = mean(est))



# make graph with confidence interval
res_m1 %>% 
  ggplot(aes(x = var, y = est, color = time_cntrl)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower_2.5ci, ymax = upper_2.5ci), 
                position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Variable", y = "Estimated reliability", color = "Time control") +
  facet_wrap(~method) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  ylim(0, 1)


res_m1 %>% 
  ggplot(aes(x = var, y =  ids_used, color = time_cntrl)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  labs(x = "Variable", y = "Estimated reliability", color = "Method") +
  facet_wrap(~method) +
  theme_bw() 



