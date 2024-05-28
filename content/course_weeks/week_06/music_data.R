library(tidyverse)


# Number of users
n <- 1000

# Simulate covariates
prior_minutes <- rnorm(n, mean = 220, sd = 50)  # Prior minutes spent on the app
age <- rnorm(n, mean = 30, sd = 10)  # Age of users
age <- if_else(age<0, 0, age)
days_since_registration <- rnorm(n, mean = 365, sd = 100)  # Days since registration
app_usage_frequency <- rpois(n, lambda = 3)  # Number of times the user opens the app per day

# Simulate treatment assignment (1 = new recommendation algorithm, 0 = old recommendation algorithm)
treatment <- rbinom(n, 1, 0.5)

# Simulate heterogeneous treatment effects based on prior usage and app usage frequency (non-linear effect)
heterogeneous_effect <- ifelse(prior_minutes > 220, 
                               -0.2 * (prior_minutes - 220) + 0.1 * (app_usage_frequency^2), 
                               0.2 * (220 - prior_minutes) + 0.1 * (app_usage_frequency^2))

# Simulate the outcome (minutes spent after introducing the new algorithm)
baseline_minutes <- 220
treatment_effect <- heterogeneous_effect * treatment
minutes_spent <- baseline_minutes + treatment_effect + rnorm(n, mean = 0, sd = 20)

# Simulate 10 categorical music genres
genres <- c("Pop", "Rock", "Hip_Hop", "Jazz", "Classical", "Country", "Electronic", "RnB", "Reggae", "Metal")
music_genres <- matrix(rbinom(n * 10, 1, 0.3), n, 10)
colnames(music_genres) <- genres

# Create data frame
data <- data.frame(
  minutes_spent = minutes_spent,
  prior_minutes = prior_minutes,
  age = age,
  days_since_registration = days_since_registration,
  app_usage_frequency = app_usage_frequency,
  treatment = treatment
)
data <- cbind(data, music_genres)



summary(lm(minutes_spent ~ treatment, data = data))
summary(lm(minutes_spent ~ treatment:prior_minutes + treatment, data = data))
summary(lm(minutes_spent ~ treatment:I(app_usage_frequency) + treatment:prior_minutes + treatment, data = data))
summary(lm(minutes_spent ~ treatment:I(app_usage_frequency^2) + treatment:prior_minutes + treatment, data = data))

music <- data |> as_tibble() |> rename(new_algo = treatment)

saveRDS(music, "content/course_weeks/week_06/music.rds")

