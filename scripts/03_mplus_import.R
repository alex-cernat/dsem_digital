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

issues %>% 
  filter(conv_issue) %>% 
  pull(path) %>% 
  map(MplusAutomation::runModels, logFile = "./mplus/auto/log_retry.txt")



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




res_m1 <- map_dfr(outs_m1, extract_r2)

res_m1 <- res_m1 %>% 
  separate(param, into = c("var", "method1", "method2"), sep = "_") %>% 
  mutate(method = case_when(method1 == "d" ~ "Log Duration",
                             method1 == "c" & method2 == "l2" ~ "Log count",
                             TRUE ~ "Dummy"),
         ids_used = nr_ids - ids_issues) %>% 
  select(var, method, everything(), -method1, -method2)

  
res_m1  %>% 
  filter(method == "Dummy")

## get average reliabilities
res_m1 %>% 
  group_by(var) %>% 
  summarise(rel = mean(est))

res_m1 %>% 
  group_by(method) %>% 
  summarise(rel = mean(est))



# make graph with confidence interval
res_m1 %>% 
  ggplot(aes(x = var, y = est, color = method)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower_2.5ci, ymax = upper_2.5ci), 
                position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Variable", y = "Estimated reliability", color = "Method") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  ylim(0, 1)


