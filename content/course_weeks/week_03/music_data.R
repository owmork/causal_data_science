# Number of observations
n <- 10000

# Generate data
data <- data.frame(
  previous_minutes = sample(0:900, n, replace = TRUE),
  new_algo = sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5)),
  log_in_day = sample(1:3, n, replace = TRUE, prob = c(0.3, 0.4, 0.3))
)

# Add treatment effect based on new_algo and first_day_used
data$treatment_effect <- 15 * data$new_algo * data$log_in_day

# Generate outcome variable minutes
data$minutes <- data$previous_minutes + data$treatment_effect

# Add some noise to minutes
data$minutes <- data$minutes# + rnorm(n, mean = 0, sd = 1)

# Ensure minutes is within range [0, 900]
data$minutes <- scales::rescale(data$minutes, to = c(0, 900))



# Print summary statistics
summary(data)


summary(lm(minutes ~ new_algo, data = data))
summary(lm(minutes ~ new_algo, data = data |> filter(log_in_day == 1)))
summary(lm(minutes ~ new_algo, data = data |> filter(log_in_day == 2)))
summary(lm(minutes ~ new_algo, data = data |> filter(log_in_day == 3)))

summary(lm(minutes ~ new_algo, data = data |> filter(log_in_day <= 1)))
summary(lm(minutes ~ new_algo, data = data |> filter(log_in_day <= 2)))
summary(lm(minutes ~ new_algo, data = data |> filter(log_in_day <= 3)))


summary(lm(minutes ~ new_algo:log_in_day, data = data))
summary(lm(minutes ~ new_algo + new_algo:log_in_day, data = data))


# cancel subscription
data$cancel <- rbinom(n, 1, round(scales::rescale(data$minutes, to = c(.55, 0))))
data$cancel <- rbinom(n, 1, scales::rescale(data$minutes, to = c(.18, 0)))


glm_fit <- glm(cancel ~ new_algo, family = binomial(link = "logit"), data = data)
glm_fit <- glm(cancel ~ new_algo:log_in_day, family = binomial(link = "logit"), data = data)
summary(glm_fit)
exp(coefficients(glm_fit)[2])
data |> group_by(new_algo) |> summarise(mean(cancel))
data |> group_by(new_algo, log_in_day) |> summarise(mean(cancel))


#saveRDS(data, "content/course_weeks/week_03/music.rds")
