library(tidyverse)
set.seed(11)

# [0] Data ----
# previous purchasing behavior, age, income?
n <- 1e+3
age <- rbeta(n, 2, 5) %>%
  scales::rescale(c(16, 90)) %>%
  round(1)

sex <- rbinom(n, 1, 0.5)

pre_avg_purch <- scales::rescale(runif(n, 0, 60) + rnorm(n, age, 15), c(0, 70))

card_prob <- (0.55*pre_avg_purch + 0.15*age) %>% scales::rescale(c(0.1, 0.8))
card <- rbinom(n, 1, card_prob) 

avg_purch <- rnorm(n, pre_avg_purch, 15) + rnorm(n, 15, 3)*card

df <- tibble(
  age = age, 
  sex = sex,
  pre_avg_purch = pre_avg_purch,
  card = card,
  avg_purch = avg_purch
)

saveRDS(df, "content/course_weeks/week_04/membership.rds")

