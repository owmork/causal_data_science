# Part 1
library(tidyverse)
music <-readRDS("music.rds")

# As a pre-check, test whether your random assignment worked well by comparing
# the daily number of minutes users listened to music per group.
music |> 
  group_by(new_algo) |> 
  summarise(mean_previous_minutes = mean(previous_minutes)) |> 
  ungroup()
  
# Compute the average treatment effect using a linear regression.
lr <- lm(minutes ~ new_algo, data = music)
summary(lr)

# Take a look at the summary statistics of the regression and answer the following questions:
#   What is the estimated treatment effect?

#   What is the null hypothesis and do you reject or do you confirm it? At what significance level?

#   What doest $R^2$ describe?

#   What is the intercept and what does it describe?

# Calculate 90%, 95% and 99% confidence intervals for the treatment effect and
# compare them. Which intervals are larger? And what question do they answer?
confint(lr, level = .9)
confint(lr, level = .99)

# Part 2
# Fit the model and return the summary statistics.
glr <- glm(cancel ~ new_algo, family=binomial(link='logit'), data = music)
summary(glr)

# Interpret the coefficients.
exp(glr$coefficients["new_algo"]) - 1

