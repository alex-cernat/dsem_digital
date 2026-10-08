# PINCET example: does daily social media use go together with
# daily news exposure on the smartphone?

# mirrors Scharkow, Mangold, Stier & Breuer (2020, PNAS) with our data
# compares the naive model with models corrected for measurement error:
# a) correcting social media use only, using our MEAR reliability
# b) correcting social media use and the control (other web activity)

# steps:
# 1. make daily news and social media website visits from the mobile web tracking
# 2. merge with the coded daily app data (30 days before wave 1)
# 3. split the predictors in within and between parts
# 4. latent variable model (lavaan), naive: ignoring measurement error
# 5. correct social media use for measurement error
# 6. correct both social media use and other web activity
# 7. summarise and plot

# whole script takes about 3 minutes (mostly reading the tracking data)


# Admin ------------


# install.packages("tidyverse")
# install.packages("lavaan") # not yet in the renv library of the project

library(tidyverse)
library(lavaan)

# folder for results
dir.create("./out", showWarnings = FALSE)


# Import data -----------------

data_full <- read_rds("./data/data_full.rds")
mobile_raw <- read_rds("./data/mobile_tracking_combined.rds")


# 1. Daily news and social media website visits -----------------

# news: content category news, without weather sites (the outcome)
# social media websites: as in scripts/01.data_prep.R, where they are
# already part of the social media measure
web_day <- mobile_raw %>%
  mutate(date = lubridate::as_date(used_at),
         is_news = coalesce(cat.1 == "News / Weather / Information" &
                              !str_detect(domain, "wetter|weather"), FALSE),
         is_sm = coalesce(str_detect(url, "facebook|twitter|instagram"), FALSE)) %>%
  group_by(new_id, date) %>%
  summarise(news = sum(is_news),
            sm_web = sum(is_sm & !is_news),
            .groups = "drop")

# check the most visited news sites
mobile_raw %>%
  filter(cat.1 == "News / Weather / Information",
         !str_detect(domain, "wetter|weather")) %>%
  count(domain) %>%
  arrange(desc(n)) %>%
  head(20)


# 2. Merge -----------------

# only people with mobile web tracking, days without visits = 0
dat <- data_full %>%
  mutate(date = lubridate::as_date(date)) %>%
  filter(new_id %in% mobile_raw$new_id) %>%
  left_join(web_day, by = c("new_id", "date")) %>%
  mutate(news = replace_na(news, 0),
         sm_web = replace_na(sm_web, 0))

# keep days with social media use, as our MEAR estimates for log counts
# (sm_c_l2) are for these days
# control: other web activity, log(1 + count), like "other visits" in
# Scharkow et al.: web browser app sessions and mobile website visits (web_c)
# without the news visits (the outcome) and the social media website visits
# (part of sm)
dat <- dat %>%
  filter(!is.na(sm_c_l2), !is.na(age), !is.na(female)) %>%
  mutate(sm = sm_c_l2,
         oth = log1p(web_c - news - sm_web),
         news_l = log1p(news),
         age_c = age - mean(age))

# check: other web activity is never negative
summary(dat$web_c - dat$news - dat$sm_web)


# 3. Within and between parts -----------------

# within part (_w) = daily value minus the person mean
# between part (_b) = person mean, centred on the grand mean
dat <- dat %>%
  group_by(new_id) %>%
  mutate(across(c(sm, oth),
                list(w = ~ .x - mean(.x),
                     b = ~ mean(.x))),
         n_days = n()) %>%
  ungroup() %>%
  mutate(across(c(sm_b, oth_b), ~ .x - mean(.x)))

# number of people, days and days per person
n_distinct(dat$new_id)
nrow(dat)
count(dat, new_id) %>% summary()

# share of days with news visits
mean(dat$news > 0)

# within-person correlation of social media use and other web activity
cor(dat$sm_w, dat$oth_w)


# 4. Latent variable model, naive -----------------

# latent variable model: two-level SEM in lavaan (as in sim_a_scharkow.R)
# lavaan splits the variables in within and between parts itself
# true social media use is a latent variable measured by daily social media
# use, with the error variance at the within level fixed to the value
# implied by the reliability (reliability 1 = no error, the naive model)
# other web activity is treated the same way; by default it has no error
# (rel_oth = 1), see step 6
fit_latent <- function(data, rel, rel_oth = 1) {

  # error variance of a single day
  # (centring on the person mean shrinks the variance by 1 - 1 / n_days)
  err_sm <- (1 - rel) * var(data$sm_w) / mean(1 - 1 / data$n_days)
  err_oth <- (1 - rel_oth) * var(data$oth_w) / mean(1 - 1 / data$n_days)

  model <- paste0('
    level: 1
      sm_true =~ 1*sm
      sm ~~ ', err_sm, '*sm
      oth_true =~ 1*oth
      oth ~~ ', err_oth, '*oth
      news_l ~ sm_true + oth_true
      # true social media use can correlate with other web activity
      sm_true ~~ oth_true
    level: 2
      news_l ~ sm + oth + age_c + female
  ')

  sem(model, data = data, cluster = "new_id")
}

# helper: within and between effects of social media use from the latent model
# and the correlation between true social media use and other web activity
get_sm <- function(fit, label, method) {
  est <- parameterEstimates(fit) %>%
    filter(lhs == "news_l", op == "~", rhs %in% c("sm_true", "sm"))
  tibble(model = label,
         method = method,
         within = est$est[est$rhs == "sm_true"],
         se_within = est$se[est$rhs == "sm_true"],
         between = est$est[est$rhs == "sm"],
         se_between = est$se[est$rhs == "sm"],
         cor_true = lavInspect(fit, "cor.lv")[[1]]["sm_true", "oth_true"],
         converged = lavInspect(fit, "converged"))
}

# naive model: ignores measurement error (reliability 1)
# (lavaan warns that some people have no variation over days, e.g. no news
# visits on any day; this is fine)
res_naive <- get_sm(fit_latent(dat, 1), "Naive", "Naive")

res_naive


# 5. Correct social media use for measurement error -----------------

# within-person reliability of social media log counts from our MEAR model
# is .30 (average of the person-specific R-squares); we also try .23 (ratio of
# the average true to the average total within variance in the same model)
# and .40 (the highest reliability we found for log counts, taking photos)
res_sm <- bind_rows(
  get_sm(fit_latent(dat, 0.40), "Social media (0.40)", "Corrected"),
  get_sm(fit_latent(dat, 0.30), "Social media (0.30)", "Corrected"),
  get_sm(fit_latent(dat, 0.23), "Social media (0.23)", "Corrected")
)

res_sm

# alternative: regression calibration, gives the same results
# (see sim_a_scharkow.R, step 4, and ai/archive/sim_a_corrections.R)


# 6. Correct both social media use and other web activity -----------------

# other web activity is also DTD, so it has random error too
# we have no MEAR estimate for this coding (log(1 + count), including days
# without use), so we try a few values, keeping social media at .30
res_both <- bind_rows(
  get_sm(fit_latent(dat, 0.30, rel_oth = 0.90), "Both (0.30 and 0.90)", "Corrected (both)"),
  get_sm(fit_latent(dat, 0.30, rel_oth = 0.66), "Both (0.30 and 0.66)", "Corrected (both)"),
  get_sm(fit_latent(dat, 0.30, rel_oth = 0.40), "Both (0.30 and 0.40)", "Corrected (both)")
)

res_both


# 7. Results -----------------

# naive and corrected effects of social media use on news visits
# (standard errors of the corrected effects treat the reliability as known)
res_all <- bind_rows(res_naive, res_sm, res_both)

res_all

write_csv(res_all, "./out/pincet_news_results.csv")

# graph: estimates and 95% confidence intervals
res_all %>%
  pivot_longer(c(within, between), names_to = "part", values_to = "est") %>%
  mutate(se = ifelse(part == "within", se_within, se_between),
         part = fct_relevel(str_to_title(part), "Within"),
         model = fct_rev(fct_inorder(model))) %>%
  ggplot(aes(est, model, color = method)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se)) +
  facet_wrap(~part) +
  scale_color_manual(values = c("Naive" = "#00BFC4",
                                "Corrected" = "#F8766D",
                                "Corrected (both)" = "#7CAE00")) +
  labs(x = "Effect of social media use on news visits",
       y = NULL,
       color = "Method") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggsave("./out/pincet_news.png", width = 10, height = 4)
