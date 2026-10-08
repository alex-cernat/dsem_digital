# Simulation B: impact of random error in a digital trace OUTCOME on
# between-person effects

# replicates Harari, Müller, Stachl, Wang, Wang, Bühner, Rentfrow, Campbell &
# Gosling (2020, Journal of Personality and Social Psychology)
# "Sensing sociability: Individual differences in young adults' conversation,
# calling, texting, and app use behaviors in daily life",
# using the authors' open data: https://osf.io/p9rz3/
# we use one result from the German sample (Sample 3, 137 people, 30 days):
# extraversion -> number of times messaging apps are used per day

# same approach as sim_a_scharkow.R:
# 1. replicate the published result
# 2. fit the naive and the latent variable model on the original data (benchmark)
# 3. add random error to daily messaging
# 4. correct for the error with the latent variable model (lavaan)
# 5. run the simulation, summarise and plot
# we do this using all 30 days and only the first 7 days

# whole script takes about 6 minutes


# Admin ------------


# install.packages("tidyverse")
# install.packages("lavaan") # not yet in the renv library of the project

library(tidyverse)
library(lavaan)

set.seed(1234)

# folder for results
dir.create("./out", showWarnings = FALSE)


# Import data -----------------

# daily number of times messaging apps were used (days without use not included)
tmp <- tempfile(fileext = ".csv")
download.file("https://osf.io/download/9q7hg/", tmp, mode = "wb")
msg_raw <- read_csv(tmp)

# person-level data, including extraversion (BFSI_E)
tmp2 <- tempfile(fileext = ".csv")
download.file("https://osf.io/download/8ztd6/", tmp2, mode = "wb")
pers_raw <- read_csv(tmp2)


# Prepare data -----------------

# one row per person and day (days 2 to 31), days without a record = 0
# (as in the authors' person means for almost all participants)
# outcome is log(1 + number of times), as for the counts in our data
dat <- expand_grid(userid = pers_raw$userid, day = 2:31) %>%
  left_join(msg_raw %>%
              transmute(userid,
                        day = as.numeric(str_remove(date, "day")),
                        msg_n = sums_24hours),
            by = c("userid", "day")) %>%
  mutate(msg_n = replace_na(msg_n, 0),
         msg = log1p(msg_n)) %>%
  left_join(select(pers_raw, userid, extra = BFSI_E), by = "userid")

# number of people and days
n_distinct(dat$userid)
nrow(dat)


# 1. Replicate published results -----------------

# published (Table 6): Spearman correlation between extraversion and the
# average daily number of times messaging apps were used, r = .24
pers_mean <- dat %>%
  group_by(userid, extra) %>%
  summarise(msg_n = mean(msg_n), .groups = "drop")

cor(pers_mean$msg_n, pers_mean$extra, method = "spearman")


# 2. Benchmark on the original data -----------------

# a) naive: regression of the observed person means of log(1 + messaging) on
# extraversion, the usual approach (standardised effect = correlation)
# fitted in lavaan, like the corrected model (same results as lm())

get_naive <- function(data) {

  pm <- data %>%
    group_by(userid, extra) %>%
    summarise(msg = mean(msg), .groups = "drop")

  fit <- sem('msg ~ extra', data = pm)

  est <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "~")

  c(b = est$est, beta = est$std.all, t = est$z)
}

# b) latent variable model: two-level SEM in lavaan, where extraversion
# predicts the latent person mean of messaging
# the day to day error ends up in the within-person variance (level 1),
# so here we don't need to know the reliability to correct for it

model_lat <- '
  level: 1
    msg ~~ msg
  level: 2
    msg ~ extra
'

get_latent <- function(data) {

  fit <- sem(model_lat, data = data, cluster = "userid")

  est <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "~")

  c(b = est$est, beta = est$std.all, t = est$z)
}

# we use the first 7 days or all 30 days
days_levels <- c(7, 30)

# the "true" effects of extraversion
# each model has its own benchmark (as in sim_a_scharkow.R): with few days
# the naive model is lower even without added error, as the person means
# also include true day to day variation
true_b <- map_df(days_levels, function(d) {
  rbind(Naive = get_naive(filter(dat, day <= d + 1)),
        Corrected = get_latent(filter(dat, day <= d + 1))) %>%
    as_tibble(rownames = "method") %>%
    mutate(n_days = d)
}) %>%
  select(method, n_days, true_b = b, true_beta = beta)

true_b


# 3. Add random error -----------------

# we treat the observed log(1 + messaging) as the true score
# and add normal random error each day
# within-person reliability = true within variance / (true within variance + error variance)
# the within variance is computed on the days analysed (first 7 or all 30),
# as the first week has less day to day variation
# (centring on the person mean shrinks the variance by 1 - 1 / n_days)
var_msg_w <- map_dbl(days_levels, function(d) {
  dat %>%
    filter(day <= d + 1) %>%
    group_by(userid) %>%
    mutate(msg_w = msg - mean(msg)) %>%
    ungroup() %>%
    summarise(v = var(msg_w) / (1 - 1 / d)) %>%
    .[["v"]]
})

var_msg_w

get_err_var <- function(rel, d) {
  var_msg_w[days_levels == d] * (1 - rel) / rel
}

# reliability levels from our MEAR results, as in sim_a_scharkow.R
rel_levels <- c(0.66, 0.40, 0.30, 0.19)

# function that adds error to daily messaging and keeps the first d days
# z are standard normal draws, the same for 7 and 30 days so that the two
# are comparable
add_error <- function(rel, d, z) {
  dat %>%
    mutate(msg = msg + z * sqrt(get_err_var(rel, d))) %>%
    filter(day <= d + 1)
}


# 4. Correct for the error -----------------

# the latent variable model from step 2, see run_one() below

# alternative: correct the person-mean correlation with the reliability of
# the person mean (Spearman-Brown), gives the same results (see ai/archive/sim_b_ringwald.R)


# 5. Run simulation -----------------

# one repetition: add error, fit the naive and the corrected model
# with the first 7 days and with all 30 days
run_one <- function(rel) {

  z <- rnorm(nrow(dat))

  map_df(days_levels, function(d) {
    dat_err <- add_error(rel, d, z)
    rbind(Naive = get_naive(dat_err),
          Corrected = get_latent(dat_err)) %>%
      as_tibble(rownames = "method") %>%
      mutate(n_days = d)
  }) %>%
    mutate(rel = rel)
}

# number of repetitions per reliability level
n_reps <- 50

res_sim <- map_df(rep(rel_levels, each = n_reps), run_one)

write_rds(res_sim, "./out/sim_b_harari_results.rds")
write_rds(true_b, "./out/sim_b_harari_true.rds")


# res_sim <- read_rds("./out/sim_b_harari_results.rds")
# true_b <- read_rds("./out/sim_b_harari_true.rds")

# Results -----------------

# estimates as share of the true value of each model
res_long <- res_sim %>%
  left_join(true_b, by = c("method", "n_days")) %>%
  mutate(ratio_b = b / true_b,
         ratio_beta = beta / true_beta)

# average estimate / true value, average t value and share significant
res_long %>%
  group_by(n_days, method, rel) %>%
  summarise(ratio_b = mean(ratio_b),
            ratio_beta = mean(ratio_beta),
            mean_t = mean(t),
            sig = mean(abs(t) > 1.96),
            .groups = "drop") %>%
  print(n = Inf)

# graph: standardised effect / true value by reliability (1 = no bias)
res_long %>%
  mutate(n_days = fct_relevel(paste(n_days, "days"), "7 days")) %>%
  ggplot(aes(as.factor(rel), ratio_beta, color = method)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~n_days) +
  labs(x = "Within-person reliability of daily messaging",
       y = "Estimate / true value",
       color = "Method") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggsave("./out/sim_b_harari.png", width = 9, height = 4)
