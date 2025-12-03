
# Code for applying dsem quasi-simplex to estimate reliability in digital trace 
# data


# Admin ------------


# install.packages("tidyverse")
# install.packages("MplusAutomation")

library(tidyverse)
library(MplusAutomation)

map(list.files("./functions/", full.names = T), source)

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



res_rel <- out_m1[!(issues$conv_issue)] %>% 
  map_df(extract_r2) %>% 
  mutate(time = issues$path[issues$conv_issue == F] %>% 
           str_detect("time")
  )

res_rel







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
to_run <- c(paths_to_run, paths_to_run2, path_m2) %>% 
  str_replace("out", "inp")
  

# make new folder for rerun
dir.create("./mplus/auto/retry/", showWarnings = F)
file.copy(to_run, 
          "./mplus/auto/retry/", 
          overwrite = T)
# copy data files
list.files("./mplus/auto/", 
           pattern = ".dat", 
           full.names = T) %>%
  file.copy("./mplus/auto/retry/", 
            overwrite = T)

list.files("./mplus/auto/retry",
           pattern = "inp",
           full.names = T) %>% 
  runModels(logFile = "./mplus/auto/log_retry.txt", 
            showOutput = T)


# compare long runs with original models ----------------------------

path_orig <- c(paths_to_run, paths_to_run2, path_m2)

out_orig <- path_orig %>% 
  map(MplusAutomation::readModels)



issues_orig <- tibble(
  path = path_orig,
  conv_issue = map_lgl(out_orig, function(x) x$tech8$psr%>% 
                         slice_tail(n = 1) %>% 
                         .[["psr"]] > 1.099
  )
)


path_retry <- list.files("./mplus/auto/retry/", 
                         full.names = TRUE, 
                         pattern = "out") 

out_retry <- path_retry %>% 
  map(MplusAutomation::readModels)





issues_retry <- tibble(
  path = path_retry,
  conv_issue = map_lgl(out_retry, function(x) x$tech8$psr%>%
                         slice_tail(n = 1) %>%
                         .[["psr"]] > 1.099
  )
)


count(issues_retry, conv_issue)

issues_retry %>% 
  filter(conv_issue) %>% 
  pull(path)




# issues_retry %>% 
#   filter(conv_issue) %>% 
#   pull(path) %>%
#   str_replace("out", "inp") %>%
#   map(MplusAutomation::runModels, logFile = "./mplus/auto/retry/log_retry.txt")



res_rel_retry <- out_retry[!(issues_retry$conv_issue)] %>% 
  map_df(extract_r2) %>% 
  mutate(time = issues_retry$path[issues_retry$conv_issue == F] %>% 
           str_detect("time")
  ) %>% 
  mutate(source = "retry")

res_rel_retry




res_rel_orig <- out_orig[!(issues_orig$conv_issue)] %>% 
  map_df(extract_r2) %>% 
  mutate(time = issues_orig$path[issues_orig$conv_issue == F] %>% 
           str_detect("time")
  ) %>% 
  mutate(source = "original")
res_rel_orig


res_rel_combined <- bind_rows(res_rel_orig, res_rel_retry) %>% 
  arrange(param, time)

# visualize estimate by source
res_rel_combined %>% 
  ggplot(aes(x = est, y = param, color = source)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = lower_2.5ci, xmax = upper_2.5ci), 
                position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Estimated reliability", y = "Parameter", color = "Source") +
  facet_wrap(~time) +
  theme_bw() 


# final results ----------------------------

# use long runs for models with issues otherwise keep original




path <- list.files("./mplus/auto/", 
                   full.names = TRUE, 
                   pattern = "out") %>% 
  str_subset("m1") 

# replace with retry paths where applicable

path2 <- ifelse(path %in% path_orig, 
                str_replace(path, "auto/", "auto/retry/"), 
                path
)



out_m1 <- path2 %>% 
  map(MplusAutomation::readModels) 



issues <- tibble(
  path = path2,
  conv_issue = map_lgl(out_m1, function(x) x$tech8$psr%>% 
                         slice_tail(n = 1) %>% 
                         .[["psr"]] > 1.099
  )
)


count(issues, conv_issue)

issues %>% 
  filter(conv_issue) %>% 
  pull(path)


res_rel <- out_m1[!(issues$conv_issue)] %>% 
  map_df(extract_r2) %>% 
  mutate(time = issues$path[issues$conv_issue == F] %>% 
           str_detect("time")
  )

res_rel




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
         variable = case_when(var == "msg" ~ "Messaging",
                              var == "call" ~ "Calling",
                              var == "phot" ~ "Take photos",
                              var == "web" ~ "Web browsing",
                              TRUE ~ "Social media"),
         variable = fct_rev(as.factor(variable)),
         ids_used = nr_ids - ids_issues) %>% 
  select(var, method, everything(), -method1, -method2)


res_m1  %>% 
  filter(method == "Dummy")

## get average reliabilities
res_m1 %>% 
  group_by(variable, time) %>% 
  summarise(rel = mean(est))

res_m1 %>% 
  group_by(method) %>% 
  summarise(rel = mean(est))



# make graph with confidence interval
res_m1 %>% 
  ggplot(aes(y = variable, x = est, color = time)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = lower_2.5ci, xmax = upper_2.5ci), 
                position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Variable", y = "Estimated reliability", color = "Time control") +
  facet_wrap(~method) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  xlim(0, 1)

ggsave("./out/m1_time_impact_reliability.png", width = 12, height = 4)

res_m1 %>% 
  filter(time == T) %>% 
  ggplot(aes(est, variable, color = method)) +
  geom_point(position = position_dodge(width = -0.5), size = 3) +
  geom_errorbar(aes(xmin = lower_2.5ci, xmax = upper_2.5ci), 
                position = position_dodge(width = -0.5), width = 0) +
  labs(y= "Variable", x = "Estimated reliability", color = "Method") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggsave("./out/m1_time_reliability.png", width = 8, height = 4)


# Extract m2 results ----------------------------


path_m2 <- list.files("./mplus/auto/", 
                      full.names = TRUE, 
                      pattern = "out") %>% 
  str_subset("m2")


path_m2b <- ifelse(path_m2 %in% path_orig, 
                str_replace(path_m2, "auto/", "auto/retry/"), 
                path_m2
)



out_m2 <- path_m2b %>%
  map(MplusAutomation::readModels)

# get models with issues
issues_m2 <- tibble(
  path = path_m2b,
  conv_issue = map_lgl(out_m2, function(x) x$tech8$psr%>% 
                         slice_tail(n = 1) %>% 
                         .[["psr"]] > 1.099
  )
)
issues_m2




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
         variable = case_when(var == "msg" ~ "Messaging",
                              var == "call" ~ "Calling",
                              var == "phot" ~ "Take photos",
                              var == "web" ~ "Web browsing",
                              TRUE ~ "Social media"),
         variable = fct_rev(as.factor(variable)),
         ids_used = nr_ids - ids_issues) %>% 
  select(var, method, everything(), -method1, -method2)





res_m2 %>% 
  mutate(sig = ifelse(sig == T, "p < 0.05", "n.s."),
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
  facet_wrap(~variable, ncol = 2) +
  theme_bw() +
  theme(text = element_text(size = 16), legend.position = "bottom") 

ggsave("./out/m2_regression_coefficients.png", width = 10, height = 8)

