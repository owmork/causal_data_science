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

# simulate who used feature
use_enc  <- .35 + (8/age) # (encouraged group)
use_enc[use_enc>1] <- 1
use_nenc <- .0 + (8/age)# (not encouraged group)
used_ftr <- rbinom(n, size = 1, if_else(popup == 1, use_enc, use_nenc))

# simulate outcome. users who use feature spend more time on app.
time_spent <- 10*used_ftr + .8*(100-age) + rnorm(n, 5, 5)

df <- tibble(popup   = popup,
             used_ftr   = used_ftr,
             unobserved = age,
             time_spent = time_spent)
cor(df) %>% round(2)

saveRDS(df %>% select(-unobserved), "content/course_weeks/week_07/rand_enc.rds")

summary(lm(time_spent ~ popup, data = df)) # smaller because
summary(lm(time_spent ~ used_ftr, data = df)) # larger because
summary(lm(time_spent ~ used_ftr + age, data = df))

# [1] Estimation ----
library(estimatr)
model_iv <- iv_robust(time_spent ~ used_ftr | popup, data = df)
broom::tidy(model_iv)

# [2] DAG ----
library(dagitty)
library(ggdag)

iv <- dagify(
  D ~ Y,
  U ~ Y,
  U ~ D,
  D ~ Z,
  exposure = "D",
  latent = "U",
  outcome = "Y",
  coords = list(x = c(U = 1, D = 0, Y = 2, Z = -1),
                y = c(U = 1, D = 0, Y = 0, Z = 0)),
  labels = c("D" = "New Feature", 
             "Y" = "Time Spent", 
             "U" = "Unobserved Variable",
             "Z" = "Random Encouragement")
)

ggdag_status(iv, use_labels = "label", text = T) +
  guides(color = "none") +
  theme_dag() + 
  ggtitle("Instrumental Variable Estimation")





