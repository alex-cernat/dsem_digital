# Simulation A: impact of random error in digital trace data (DTD) on
# within- and between-person effects

# replicates Scharkow, Mangold, Stier & Breuer (2020, PNAS)
# "How social network sites and other online intermediaries increase
# exposure to news", using the authors' open data: https://osf.io/pqd9f/

# steps:
# 1. replicate the published model
# 2. fit the latent variable model (lavaan) on the original data (benchmark)
# 3. add random error to daily Facebook use
# 4. fit the same model ignoring the error (naive) and correcting for it
# 5. run the simulation, summarise and plot

# whole script takes about 40 minutes (step 1 about 4, simulation about 35)


# Admin ------------


# install.packages("tidyverse")
# install.packages("lme4")   # not yet in the renv library of the project
# install.packages("lavaan") # not yet in the renv library of the project

library(tidyverse)
library(lme4)
library(lavaan)

set.seed(1234)

# folder for results
dir.create("./out", showWarnings = FALSE)


# Import data -----------------

# person-day data for the 2018 panel (desktop and mobile browsing)
# only days with some browsing are included
tmp <- tempfile(fileext = ".csv.gz")
download.file("https://osf.io/download/h2cp4/", tmp, mode = "wb")

dat_raw <- read_csv(tmp)

glimpse(dat_raw)


# Prepare data -----------------

# as in the authors' code (rewb_models.R):
# visits to other sites = total minus the rest
# predictors are log(1 + visits)
dat <- dat_raw %>%
  mutate(other_visits = total_visits - news_visits - google_visits -
           facebook_visits - twitter_visits - portals_visits,
         fb = log1p(facebook_visits),
         tw = log1p(twitter_visits),
         goo = log1p(google_visits),
         por = log1p(portals_visits),
         oth = log1p(other_visits),
         news_l = log1p(news_visits),
         age_c = age - mean(age),
         day = as.factor(day),
         obs = row_number())

# function to split the predictors in:
# within part (_w) = daily value minus the person mean
# between part (_b) = person mean, centred on the grand mean
split_wb <- function(data) {
  data %>%
    group_by(person_id) %>%
    mutate(across(c(fb, tw, goo, por, oth),
                  list(w = ~ .x - mean(.x),
                       b = ~ mean(.x)))) %>%
    ungroup() %>%
    mutate(across(c(fb_b, tw_b, goo_b, por_b, oth_b), ~ .x - mean(.x)))
}

dat_wb <- split_wb(dat)

# number of people, days and days per person
n_distinct(dat_wb$person_id)
nrow(dat_wb)
count(dat_wb, person_id) %>% summary()


# 1. Replicate published results -----------------

# published Poisson model (news visits 2018)
# from Table 3 "Published Poisson REWB model (10 second cutoff)", page 4 of the
# supplementary material on OSF (appendix.pdf at https://osf.io/pqd9f/),
# not in the PNAS article itself
# (1 | obs) deals with overdispersion, as in the authors' model
# the authors also have random slopes for all within effects,
# we drop them so the model runs in a few minutes rather than hours
# (takes about 3-4 minutes)
fit_pois <- glmer(news_visits ~ fb_w + tw_w + goo_w + por_w + oth_w +
                    fb_b + tw_b + goo_b + por_b + oth_b + age_c + female +
                    (1 | person_id) + (1 | day) + (1 | obs),
                  data = dat_wb,
                  family = poisson,
                  control = glmerControl(optimizer = "nloptwrap",
                                         calc.derivs = FALSE))

# published estimates (Table 3 of the OSF supplementary material, column Visits 2018)
published <- tibble(
  term = c("(Intercept)", "fb_w", "tw_w", "goo_w", "por_w", "oth_w",
           "fb_b", "tw_b", "goo_b", "por_b", "oth_b", "age_c", "female"),
  published = c(-1.99, 0.25, 0.20, 0.42, 0.14, 0.35,
                0.28, 0.36, 0.54, 0.15, 0.55, 0.01, -0.40)
)

# compare
tibble(term = names(fixef(fit_pois)),
       replication = round(fixef(fit_pois), 2)) %>%
  left_join(published, by = "term")


# 2. Benchmark on the original data -----------------

# for the simulation we keep people with at least 10 days
# (with very few days the latent variable model weights people differently
# once error is added, which shifts its between effect)
dat <- dat %>%
  group_by(person_id) %>%
  filter(n() >= 10) %>%
  ungroup()

dat_wb <- split_wb(dat)

n_distinct(dat_wb$person_id)
nrow(dat_wb)

# latent variable model: two-level SEM in lavaan, a linear version of the
# published model on log(1 + news visits)
# lavaan splits the variables in within and between parts itself (latent
# person means); it has no random effect for day
# true Facebook use is a latent variable measured by daily Facebook use,
# with the error variance at the within level fixed to:
# - 0 in the naive model (assumes Facebook use has no error)
# - the value implied by the reliability in the corrected model
#   (in practice this comes from our MEAR reliability estimates)
# the coefficients are on a different scale than the Poisson model,
# but here we only compare the model with itself (with and without error)
# (lavaan warns when some people have no variation over days in a variable,
# e.g. never on Twitter; this is fine)
fit_latent <- function(data, err_var) {

  model <- paste0('
    level: 1
      fb_true =~ 1*fb
      fb ~~ ', err_var, '*fb
      news_l ~ fb_true + tw + goo + por + oth
      # true Facebook use can correlate with the other predictors
      fb_true ~~ tw + goo + por + oth
      tw ~~ goo + por + oth
      goo ~~ por + oth
      por ~~ oth
    level: 2
      news_l ~ fb + tw + goo + por + oth + age_c + female
  ')

  sem(model, data = data, cluster = "person_id")
}

# helper: Facebook effects and t value of the within effect
get_fb <- function(fit) {
  est <- parameterEstimates(fit) %>%
    filter(lhs == "news_l", op == "~", rhs %in% c("fb_true", "fb"))
  c(Within = est$est[est$rhs == "fb_true"],
    Between = est$est[est$rhs == "fb"],
    t_within = est$z[est$rhs == "fb_true"])
}

fit_true <- fit_latent(dat, err_var = 0)

summary(fit_true)

# the "true" effects of Facebook use
true_fb <- get_fb(fit_true)

true_fb


# 3. Add random error -----------------

# we treat the observed log(1 + Facebook visits) as the true score
# and add normal random error each day
# within-person reliability = true within variance / (true within variance + error variance)
# so the error variance for a given reliability is:
# (centring on the person mean shrinks the variance by 1 - 1 / n_days)
var_fb_w <- dat_wb %>%
  add_count(person_id, name = "n_days") %>%
  summarise(v = var(fb_w) / mean(1 - 1 / n_days)) %>%
  .[["v"]]

get_err_var <- function(rel) {
  var_fb_w * (1 - rel) / rel
}

# reliability levels from our MEAR results (log counts .19-.40, binary up to .66)
rel_levels <- c(0.66, 0.40, 0.30, 0.19)

# function that adds error to Facebook use
add_error <- function(rel) {
  dat %>%
    mutate(fb = fb + rnorm(n(), mean = 0, sd = sqrt(get_err_var(rel))))
}


# 4. Naive and corrected model -----------------

# the latent variable model from step 2 with the error variance fixed at
# 0 (naive) or at get_err_var(rel) (corrected), see run_one() below

# alternative: regression calibration in a multilevel model (lme4), gives the
# same results for the within effect (see ai/archive/sim_a_corrections.R)
# replaces the noisy Facebook variables with their expected true values given
# the other predictors and the known error variance
# the within part has error variance err_var * (1 - 1 / n_days)
# the between part (person mean) has error variance err_var / n_days
# form_lin <- news_l ~ fb_w + tw_w + goo_w + por_w + oth_w +
#   fb_b + tw_b + goo_b + por_b + oth_b + age_c + female +
#   (1 | person_id) + (1 | day)
#
# correct_error <- function(data, rel) {
#
#   err_var <- get_err_var(rel)
#
#   data <- data %>%
#     add_count(person_id, name = "n_days")
#
#   # within part
#   res_w <- resid(lm(fb_w ~ tw_w + goo_w + por_w + oth_w, data = data))
#   lambda_w <- (var(res_w) - err_var * mean(1 - 1 / data$n_days)) / var(res_w)
#
#   data <- data %>%
#     mutate(fb_w = fb_w - (1 - lambda_w) * res_w)
#
#   # between part, one row per person
#   pers <- data %>%
#     distinct(person_id, fb_b, tw_b, goo_b, por_b, oth_b, age_c, female, n_days)
#
#   res_b <- resid(lm(fb_b ~ tw_b + goo_b + por_b + oth_b + age_c + female,
#                     data = pers))
#   lambda_b <- (var(res_b) - err_var * mean(1 / pers$n_days)) / var(res_b)
#
#   pers <- pers %>%
#     mutate(fb_b_cor = fb_b - (1 - lambda_b) * res_b) %>%
#     select(person_id, fb_b_cor)
#
#   data %>%
#     left_join(pers, by = "person_id") %>%
#     mutate(fb_b = fb_b_cor)
# }
#
# fit_rc <- lmer(form_lin, data = correct_error(split_wb(dat_err), rel))


# 5. Run simulation -----------------

# one repetition: add error, fit the naive and the corrected model
run_one <- function(rel) {

  dat_err <- add_error(rel)

  fit_naive <- fit_latent(dat_err, err_var = 0)
  fit_cor <- fit_latent(dat_err, err_var = get_err_var(rel))

  rbind(Naive = get_fb(fit_naive),
        Corrected = get_fb(fit_cor)) %>%
    as_tibble(rownames = "method") %>%
    mutate(rel = rel)
}

# number of repetitions per reliability level
n_reps <- 50

res_sim <- map_df(rep(rel_levels, each = n_reps), run_one)

write_rds(res_sim, "./out/sim_a_results.rds")
write_rds(true_fb, "./out/sim_a_true.rds")


# Results -----------------

# estimates as share of the true value
res_long <- res_sim %>%
  pivot_longer(c(Within, Between), names_to = "part", values_to = "est") %>%
  mutate(true = true_fb[part],
         ratio = est / true)

# average estimate / true value by reliability
res_long %>%
  group_by(part, method, rel) %>%
  summarise(ratio = mean(ratio), .groups = "drop") %>%
  pivot_wider(names_from = rel, values_from = ratio, names_prefix = "rel_")

# precision of the within effect: average t value and share significant
# (the correction removes the bias but does not give back the lost precision)
res_sim %>%
  group_by(method, rel) %>%
  summarise(mean_t = mean(t_within),
            sig_within = mean(abs(t_within) > 1.96),
            .groups = "drop")

# graph: estimate / true value by reliability (1 = no bias)
res_long %>%
  mutate(part = fct_relevel(part, "Within")) %>%
  ggplot(aes(as.factor(rel), ratio, color = method)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~part) +
  labs(x = "Within-person reliability of Facebook use",
       y = "Estimate / true value",
       color = "Method") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggsave("./out/sim_a_scharkow.png", width = 9, height = 4)
