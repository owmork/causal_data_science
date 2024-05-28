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

avg_purch <- rnorm(n, pre_avg_purch, 15) + rnorm(n, 15, 3)*card + (30-age)*card + sex*card*5

# no influence
# Define the categories and their respective lambda values
categs <- list(
  "vehicle" = 0.1,
  "food" = 1,
  "beverage" = 1,
  "art" = 1,
  "baby" = 1,
  "personal_care" = 1,
  "toys" = 1,
  "clothing" = 2,
  "decor" = 1,
  "cell_phones" = 3,
  "construction" = 1,
  "home_appliances" = 1,
  "electronics" = 2,
  "sports" = 1,
  "tools" = 1,
  "games" = 2,
  "industry" = 1,
  "pc" = 2,
  "jewel" = 1,
  "books" = 1,
  "music_books_movies" = 1,
  "health" = 2
)

# Generate random numbers from Poisson distribution for each category
categ_purchase <- lapply(categs, function(l) rpois(n, l))

# Create data
df <- tibble(
  age = age, 
  sex = sex,
  pre_avg_purch = pre_avg_purch,
  card = card,
  avg_purch = avg_purch,
  bind_cols(categ_purchase)
)

summary(lm(avg_purch ~ . + card:age + card:sex, data = df))

saveRDS(df, "content/course_weeks/week_06/membership.rds")
