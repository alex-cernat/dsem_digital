# Code for applying dsem quasi-simplex to estimate reliability in digital trace 
# data


# Admin ------------


# install.packages("tidyverse")
# install.packages("MplusAutomation")

library(tidyverse)
library(MplusAutomation)

# import data

app_raw <- read_rds("./data/app_tracking_combined.rds")
mobile_raw <- read_rds("./data/mobile_tracking_combined.rds")

app_select_info <- read_csv("./data/cat_select.csv")

vars_int_prefix <- c("call", "msg", "web", "phot", "sm")



survey_full <- read_rds("./data/survey_mtmm_clean.rds")




# Prepare data -----------------


# Descriptives ------------------------------------------------------------


glimpse(mobile_raw)
glimpse(app_raw)
count(app_raw, app_cat)


app_raw %>%
  filter(app_cat == "Email, Messaging & Telephone") %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()

app_raw %>%
  filter(app_cat == "Social & dating") %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()



app_raw %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()


# make categories ---------------------------------------------------------


# apps of interest

call_app_vct <- filter(app_select_info,
                       category == "calling" & fit == "yes") %>%
  .[["app_n"]]
msg_app_vct <- filter(app_select_info,
                      category == "messaging" & fit == "yes") %>%
  .[["app_n"]]
web_app_vct <- filter(app_select_info,
                      category == "browsing" & fit == "yes") %>%
  .[["app_n"]]
phot_app_vct <- filter(app_select_info,
                       category == "photo" & fit == "yes") %>%
  .[["app_n"]]
sm_app_vct <- filter(app_select_info,
                     category == "sm" & fit == "yes") %>%
  .[["app_n"]]


# code events -------------------------------------------------------------


app <- app_raw %>%
  mutate(
    call = ifelse(app_n %in% call_app_vct, 1, 0),
    msg = ifelse(app_n %in% msg_app_vct, 1, 0),
    web = ifelse(app_n %in% web_app_vct, 1, 0),
    phot = ifelse(app_n %in% phot_app_vct, 1, 0),
    sm = ifelse(app_n %in% sm_app_vct, 1, 0))

app %>%
  filter(sm == 1) %>%
  count(app_n) %>%
  arrange(desc(n))


# aggregate data ----------------------------------------------------------

glimpse(app)

count(app, survey)
qplot(app$duration %>% log())
summary(app$duration)
qplot(app$duration)
quantile(app$duration,  0.99)

app <- app %>%
  mutate(date = lubridate::as_date(used_at),
         call_dur = ifelse(call > 0, duration, 0),
         msg_dur = ifelse(msg > 0, duration, 0),
         web_dur = ifelse(web > 0, duration, 0),
         phot_dur = ifelse(phot > 0, duration, 0),
         sm_dur = ifelse(sm > 0, duration, 0))

agg_data <-  app %>%
  group_by(new_id, date) %>%
  summarise(call_count = sum(call),
            msg_count = sum(msg),
            web_count = sum(web),
            phot_count = sum(phot),
            sm_count = sum(sm),
            call_dur = sum(call_dur),
            msg_dur = sum(msg_dur),
            web_dur = sum(web_dur),
            phot_dur = sum(phot_dur),
            sm_dur = sum(sm_dur)
  ) %>%
  ungroup()


glimpse(agg_data)
View(agg_data)

map(agg_data, qplot)

# aggregate web browsing --------------------------------------------------

glimpse(mobile_raw)

# aggregate by id and date and make summary statistics for web and social media

agg_browse_data <- mobile_raw %>%
  mutate(date = lubridate::as_date(used_at)) %>%
  group_by(new_id, date) %>%
  summarise(web_main_count = max(row_number()),
            web_main_dur = sum(duration, na.rm = T),
            sm_web_count = sum(
              str_detect(url, "facebook|twitter|instagram"), na.rm = T),
            sm_web_dur = sum(
              ifelse(str_detect(url, "facebook|twitter|instagram"),
                     duration,
                     0), na.rm = T)) %>%
  ungroup() %>%
  arrange(new_id, date)

glimpse(agg_browse_data)
View(agg_browse_data)
summary(agg_browse_data)

# top_sites <- count(mobile_raw, url) %>% arrange(desc(n))
#
# top_sites %>%
#   mutate() %>%
#   View()

# put together aggregate data ---------------------------------------------


agg_all_data <- full_join(agg_data, agg_browse_data,
                          by = c("new_id", "date")) %>%
  mutate_all(~ifelse(is.na(.), 0, .))

agg_all_data2 <- agg_all_data %>%
  mutate(web_count2 = web_count + web_main_count,
         web_dur2 = web_dur + web_main_dur,
         sm_count2 = sm_count + sm_web_count,
         sm_dur2 = sm_dur + sm_web_dur) %>%
  select(-web_count, -web_dur, -web_main_dur, -web_main_count,
         -sm_web_count, -sm_web_dur, -sm_count, -sm_dur) %>%
  rename_all(~str_remove(., "2"))

View(agg_all_data2)

mean(agg_all_data$web_count, use = "compete.obs")
mean(agg_all_data2$web_count, use = "compete.obs")

mean(agg_all_data$sm_count, use = "compete.obs")
mean(agg_all_data2$sm_count, use = "compete.obs")



# save aggregate date by date and individual

write_rds(agg_all_data2, "./data/agg_phone_id_day.rds")


# aggregate 7/30 day period -----------------------------------------------


# check duplicates
duplicates <- count(survey_full, new_id) %>% filter(n > 1) %>% .[["new_id"]]

filter(survey_full, new_id %in% duplicates) 

# get rid of duplicates
survey <- survey_full %>% 
  group_by(new_id) %>%
  filter(row_number() == 1) %>%
  ungroup() 



# make date thresholds
survey_dates <- survey %>%
  select(new_id, w1_datetime, w1b_datetime) %>%
  mutate(w1_date = as_date(mdy_hm(w1_datetime)),
         w1b_date = as_date(mdy_hm(w1b_datetime)),
         w1_date_7 = w1_date - ddays(7),
         w1_date_30 = w1_date - ddays(30),
         w1b_date_7 = w1b_date - ddays(7),
         w1b_date_30 = w1b_date - ddays(30),
         new_id = as.integer(new_id))

count(survey_dates, w1_date)



# merge thresholds with aggregate date

agg_all_data3 <- left_join(agg_all_data2, survey_dates, by = "new_id") %>%
  mutate(date = as_date(date))


agg_all_data3 %>%
  mutate_at(vars(matches("count"), matches("dur")),
            list("7d_w1" = ~ifelse(date > w1_date_7 & date < w1_date,
                                   ., 0))) %>%
  View()

trace_data_diff <- agg_all_data3  %>%
  group_by(new_id) %>%
  mutate(day_before_w1 = difftime(min(date), w1_date, units = "days") %>%
           round(),
         day_before_w1b = difftime(min(date), w1b_date, units = "days") %>%
           round(),
         more_7_days_w1 = ifelse(day_before_w1 < -6, "Yes", "No"),
         more_30_days_w1 = ifelse(day_before_w1 < -29, "Yes", "No"),
         more_7_days_w1b = ifelse(day_before_w1b < -7, "Yes", "No"),
         more_30_days_w1b = ifelse(day_before_w1b < -30, "Yes", "No")
  ) %>%
  filter(row_number() == 1) %>%
  ungroup()


count(trace_data_diff, more_30_days_w1)
count(trace_data_diff, more_30_days_w1b)
count(trace_data_diff, more_7_days_w1)


qplot(trace_data_diff$day_before_w1)
qplot(trace_data_diff$day_before_w1b)

count(trace_data_diff, more_7_days_w1, more_7_days_w1b)





# make 7d and 30d aggregates ----------------------------------------------

agg_all_data3 <- mutate(agg_all_data3,
                        day_before_w1 = difftime(date,
                                                 w1_date,
                                                 units = "days") %>%
                          round() %>% as.numeric(),
                        day_before_w1b = difftime(date,
                                                  w1b_date,
                                                  units = "days") %>%
                          round() %>% as.numeric(),
                        w1_7d_window = ifelse(day_before_w1 %in% -6:0, 1, 0),
                        w1b_7d_window = ifelse(day_before_w1b %in% -6:0, 1, 0),
                        w1_30d_window = ifelse(day_before_w1 %in% -29:0, 1, 0),
                        w1b_30d_window = ifelse(day_before_w1b %in% -29:0, 1, 0))
# 
# w1_7d <- agg_all_data3 %>%
#   filter(w1_7d_window == 1) %>%
#   group_by(new_id) %>%
#   mutate(days_count = max(row_number())) %>%
#   ungroup

w1_30d <- agg_all_data3 %>%
  filter(w1_30d_window == 1) %>%
  group_by(new_id) %>%
  mutate(days_count = max(row_number())) %>%
  ungroup()


# clean duration ----------------------------------------------------------


# make duration minutes

w1_30d2 <- w1_30d %>%
  mutate_at(vars(matches("_dur")),
            ~./60) %>%
  mutate_at(vars(matches("_dur"), matches("_count")),
            list("log" = ~log(. + 1)))


# make version that codes 0s as missing
w1_30d2 <- w1_30d2 %>%
  mutate_at(vars(matches("_dur$"), matches("_count$"), -days_count),
            list("log2" = ~log(.))) %>%
  mutate_all(~ifelse(is.infinite(.), NA, .))

# make dummy version
w1_30d2 <- w1_30d2 %>%
  mutate_at(vars(matches("_count$"), -days_count),
            list("b" = ~ifelse(. > 0, 1, 0))) 

count(w1_30d2, call_count, call_count_b) %>% print(n = 100)
count(w1_30d2, msg_count, msg_count_b) %>% print(n = 100)
count(w1_30d2, phot_count, phot_count_b) %>% print(n = 100)
count(w1_30d2, web_count, web_count_b) %>% print(n = 100)

# make time variable
w1_30d2 <- w1_30d2 %>%
  mutate(time = 30 + day_before_w1) %>%
  ungroup()

summary(w1_30d2)


# clean names
w1_30d2 <- w1_30d2 %>%
  rename_all(~str_replace_all(., "_count", "_c")) %>%
  rename_all(~str_replace_all(., "_dur", "_d")) %>% 
  rename_all(~str_replace_all(., "_log", "_l")) 

# arrange data
w1_30d2 <- w1_30d2 %>% 
  arrange(new_id, time)


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
  w1_30d2 %>% 
    group_by(new_id) %>% 
    select(all_of(x)) %>% 
    summarise_all(~sum(!is.na(.))) %>%
    setNames(c("new_id", "n")) %>%
    filter(n<2) %>% 
    .[["new_id"]]
  
})


names(miss_cases) <- vars


glimpse(w1_30d2)


# check distributions -----------------------------------------------------
qplot(w1_30d2$call_c)
qplot(w1_30d2$call_c_l)
qplot(w1_30d2$call_c_l2)
count(w1_30d2, is.na(call_c_l2), call_c_b)


qplot(w1_30d2$msg_c)
qplot(w1_30d2$msg_c_l)
qplot(w1_30d2$msg_c_l2)
count(w1_30d2, is.na(msg_c_l2))



qplot(w1_30d2$phot_c)
qplot(w1_30d2$phot_c_l)
qplot(w1_30d2$phot_c_l2)
count(w1_30d2, is.na(phot_c_l2))



qplot(w1_30d2$web_c)
qplot(w1_30d2$web_c_l)
qplot(w1_30d2$web_c_l2)
count(w1_30d2, is.na(web_c_l2))


qplot(w1_30d2$call_d)
qplot(w1_30d2$call_d_l)
qplot(w1_30d2$call_d_l2)
count(w1_30d2, is.na(call_d_l2))


qplot(w1_30d2$msg_d)
qplot(w1_30d2$msg_d_l)
qplot(w1_30d2$msg_d_l2)
count(w1_30d2, is.na(msg_d_l2))

qplot(w1_30d2$phot_d)
qplot(w1_30d2$phot_d_l)
qplot(w1_30d2$phot_d_l2)
count(w1_30d2, is.na(phot_d_l2))  

qplot(w1_30d2$web_d)
qplot(w1_30d2$web_d_l)
qplot(w1_30d2$web_d_l2)
count(w1_30d2, is.na(web_d_l2))




w1_30d2 %>% 
  select(matches("count"), matches("dur")) %>% 
  summary()


w1_30d2 %>% 
  group_by(new_id) %>%
  summarise(n = max(row_number()),
            non_miss = sum(!is.na(call_c_l2))) %>% 
  filter(n < 2) 


w1_30d2 %>% 
  filter(new_id == 70) %>% 
  select(time, call_c, call_c_b) %>% print(n = 100)


# clean survey data ----------------------------------------------------


count(survey, w1_trust_media1)
count(survey, w1_trust_media2)
count(survey, w1_trust_media3)
count(survey, w1_trust_media4)


count(survey, w1_sph)
count(survey, w1_pc)
count(survey, w1_tabl)
# make sum score



qplot(as.numeric(survey$w1_inet_hr))
qplot(as.numeric(survey$w1_inet_min))


qplot(as.numeric(survey$w1_sph_use_hr))
qplot(as.numeric(survey$w1_sph_use_min))

# add up in hours


count(survey, gender)
qplot(as.numeric(survey$birth_year))


count(survey, w1_edu)
count(survey, w1_edu3)
count(survey, w1_edu4)
count(survey, w1_edu5)
count(survey, w1_edu6)
count(survey, w1_edu7)
count(survey, w1_edu8)
count(survey, w1_edu9)
count(survey, w1_edu10)


survey <- survey %>%
  mutate(
    w1_pc_nr = ifelse(w1_pc == "quoted", 1, 0),
    w1_sph_nr = ifelse(w1_sph == "quoted", 1, 0),
    w1_tabl_nr = ifelse(w1_tabl == "quoted", 1, 0),
    nr_dev = w1_pc_nr + w1_sph_nr + w1_tabl_nr,
    w1_inet = as.numeric(w1_inet_hr) + (as.numeric(w1_inet_min) / 60),
    w1_sp = as.numeric(w1_sph_use_hr) + (as.numeric(w1_sph_use_min) / 60),
    female = case_when(gender == "Mannlich" ~ 0,
                       gender == "Weiblich" ~ 1),
    age = 2021 - as.numeric(birth_year)
  )


count(survey, w1_pc, w1_tabl_nr, w1_sph_nr, nr_dev)
count(survey, gender, female)
count(survey, birth_year, age) %>% print(n = 100)
survey %>% 
  summarise(miss_or1 = mean(is.na(w1_inet_hr)),
            miss_or2 = mean(is.na(w1_inet_min)),
            miss_new = mean(is.na(w1_inet)))


survey <- survey %>%
  mutate(
    edu_cat = ifelse(
      w1_edu == "(Noch) kein Schulabschluss" |
        w1_edu == "Abschluss einer Förderschule (Sonderschule, Hilfsschule)" |
        w1_edu == "Volks- oder Hauptschulabschluss bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 8. oder 9. Klasse" |
        w1_edu == "Anderer Schulabschluss, und zwar",
      "low",
      ifelse(
        w1_edu == "Mittlere Reife, Realschulabschluss, Fachoberschulreife oder mittlerer Schulabschluss bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 10. Klasse",
        "medium",
        ifelse(
          w1_edu == "Allgemeine oder fachgebundene Hochschulreife, Abitur",
          "high",
          NA
        )
      )
    )) %>%
  mutate(edu_cat = factor(edu_cat, levels = c("low", "medium", "high")),
         edu = as.numeric(edu_cat),
         edu_low = ifelse(edu_cat == "low", 1, 0),
         edu_med = ifelse(edu_cat == "medium", 1, 0),
         edu_high = ifelse(edu_cat == "high", 1, 0)
  )

count(survey, w1_edu, edu_cat, edu, edu_low, edu_med, edu_high)


# join survey and DTD data ------------------------------------------------


data_full <- right_join(
  mutate(survey, new_id = as.numeric(new_id)) %>% 
    select(new_id, age, female, edu, edu_low, edu_med, edu_high,
           nr_dev, w1_inet, w1_sp, w1_pc_nr, w1_sph_nr, w1_tabl_nr),
  w1_30d2,
  by = "new_id")


# export data -------------------------------------------------------------

write_rds(w1_30d2, "./data/w1_30d2.rds")
write_rds(data_full, "./data/data_full.rds")


# exclude cases with only 1 row of data

single_cases <- w1_30d2 %>% 
  group_by(new_id) %>%
  summarise(n = max(row_number())) %>% 
  filter(n < 2) %>% 
  .[["new_id"]]


w1_30d2 %>% 
  filter(!new_id %in% single_cases) %>%
  MplusAutomation::prepareMplusData(filename = "mplus/w1_30d2.dat",
                                  dropCols = c("w1_datetime",
                                               "w1b_datetime"),
                                  inpfile="mplus/m0.inp")



# loop over vars to make datasets
map(vars, function(x){
  
  w1_30d2 %>% 
    filter(!new_id %in% miss_cases[[x]]) %>% 
    MplusAutomation::prepareMplusData(filename = paste0("mplus/w1_30d2_", x, ".dat"),
                                      dropCols = c("w1_datetime",
                                                   "w1b_datetime"))
  
})
