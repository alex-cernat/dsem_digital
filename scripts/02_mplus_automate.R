# Code for applying dsem quasi-simplex to estimate reliability in digital trace 
# data

# automate the creation and running of Mplus models


# Admin ------------


# install.packages("tidyverse")
# install.packages("MplusAutomation")

library(tidyverse)
library(MplusAutomation)

# import data

w1_30d2 <- read_rds("./data/w1_30d2.rds")
data_full <- read_rds("./data/data_full.rds")










# export data -------------------------------------------------------------





# exclude cases with only 1 row of data

single_cases <- data_full %>% 
  group_by(new_id) %>%
  summarise(n = max(row_number())) %>% 
  filter(n < 2) %>% 
  .[["new_id"]]


data_full %>% 
  filter(!new_id %in% single_cases) %>%
  MplusAutomation::prepareMplusData(filename = "mplus/auto/data_full.dat",
                                    dropCols = c("w1_datetime",
                                                 "w1b_datetime"),
                                    inpfile="mplus/auto/m0.inp")



# find the number of cases for each individual

vars <- c(
  "call_d_l2",
  "msg_d_l2",
  "phot_d_l2",
  "web_d_l2",
  "sm_d_l2",
  "call_c_l2",
  "msg_c_l2",
  "phot_c_l2",
  "web_c_l2",
  "sm_c_l2"
)

miss_cases <- map(vars, function(x){
  data_full %>% 
    group_by(new_id) %>% 
    select(all_of(x)) %>% 
    summarise_all(~sum(!is.na(.))) %>%
    setNames(c("new_id", "n")) %>%
    filter(n<2) %>% 
    .[["new_id"]]
  
})


names(miss_cases) <- vars


# loop over vars to make datasets
map(vars, function(x){
  
  data_full %>% 
    filter(!new_id %in% miss_cases[[x]]) %>% 
    MplusAutomation::prepareMplusData(filename = 
                                        paste0("mplus/auto/data_full_", x, ".dat"),
                                      dropCols = c("w1_datetime",
                                                   "w1b_datetime"))
  
})



# create Mplus input files ------------------------------------------------
createModels("./mplus/auto/m1_template.txt")
createModels("./mplus/auto/m1_template_cont.txt")
createModels("./mplus/auto/m1_template_time.txt")
createModels("./mplus/auto/m1_template_cont_time.txt")
# make template for model 2

