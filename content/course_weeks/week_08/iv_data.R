library(tidyverse)
set.seed(11)

# Randomized Encouragement Trial
# Causal Effect: new feature on time spent in app
# You don't want to run an A/B test and offer it to all customers directly.
# Only comparing users of feature to non-users results in biased estimate as
# some users might be more benefiting from it.
# Therefore, a random selection of users is encouraged to use new feature with
# a popup message in app.
# Instrument: randomized encouragement
# Treatment: new feature
# Outcome: time spent in app
# Confounder: need for feature

# [0] Data ----
n <- 1e+4

# simulate who was encouraged to use feature
popup <- rbinom(n, size = 1, prob = .5)

# simulate age. younger users more likely to adapt.
age <- rbeta(n, 2, 5) %>% scales::rescale(to = c(12, 65))

# gender
gender <- rbinom(n, size = 1, prob = 0.5)

# premium member
premium <- rbinom(n, size = 1, prob = 0.2)

# daily logins
daily_logins <- rbeta(n, 2, 5) %>% scales::rescale(to = c(0, 8)) |> round() 

# simulate who used feature
use_enc  <- .25 + (8/age) + .05 * premium + 0.025 * daily_logins # (encouraged group)
use_enc[use_enc>1] <- 1
use_nenc <- .0 + (8/age) + .05 * premium + 0.025 * daily_logins # (not encouraged group)
used_ftr <- rbinom(n, size = 1, if_else(popup == 1, use_enc, use_nenc))

# simulate outcome. users who use feature spend more time on app.
time_spent <- 8*used_ftr + .8*(100-age) + 4*premium + 0.5*daily_logins^2 + rnorm(n, 25, 20)

df <- tibble(popup   = popup,
             used_ftr   = used_ftr,
             age = age,
             gender, 
             premium,
             daily_logins,
             time_spent = time_spent)
cor(df) %>% round(2)

saveRDS(df, "content/course_weeks/week_08/rand_enc8.rds")

summary(lm(time_spent ~ popup, data = df)) # smaller because
summary(lm(time_spent ~ used_ftr, data = df)) # larger because
summary(lm(time_spent ~ used_ftr + age, data = df))

# [1] Estimation ----
library(estimatr)
model_iv <- iv_robust(time_spent ~ used_ftr | popup, data = df)
broom::tidy(model_iv)

model_iv <- iv_robust(time_spent ~ used_ftr + age + gender + premium + daily_logins | popup + age + gender + premium + daily_logins, data = df)
broom::tidy(model_iv)
