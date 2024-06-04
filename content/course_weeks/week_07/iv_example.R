library(tidyverse)

# [1] Directed Acyclic Graph ----
library(ggdag)
library(dagitty)

# -> see .Rmd

# [2] Application description ----
# Outcome:    work efficiency (KPI)
# Treatment:  training
# Instrument: distance from workplace to training location

# Measuring the direct effect of training on work efficiency is difficult as
# factors like motivation (that are unobserved) could bias the result. Thus,
# we use distance from workplace as an instrument.

# Let's see if it ticks all the boxes:
# 1) Stable unit treatment value assumption:
# Although all workers are eligible for treatment/training, it could happen
# that decisions of your direct coworker could also influence your decision.
# (actually not a violation?)
# 2) Independence assumption: 
# Instrument distance to training location should be independent of potential 
# outcomes and potential treatment assignment. In other words, no confounding
# between Z and D or Z and Y should exist. A potential violation would occur
# when more motivated workers would work closer to the training location
# (e.g. if training location == headquarter)
# 3) Exclusion restriction:
# Instrument does not directly affect outcome. Here, there is no reason to
# believe that the instrument affects outcome directly.
# 4) Correlation of Z and D.
# Being closer to training location increases probability to participate in
# training program.
# 5) Monotonicity assumption.
# Potential violation: Some workers are looking forward to a "long" business
# trip.

# DAG: -> see .Rmd

# [3] Generate synthetic data ----
set.seed(11)
# Number of observations
n <- 6e+3

# U -> Unobserved factor: e.g. motivation
motivation <- rnorm(n, mean = 10, sd = 2.5) %>% scales::rescale()

# Z -> Instrument: Distance to closest training location
distance <- rbeta(n, shape1 = 7, shape2 = 4) %>% scales::rescale()

# D -> Treatment: Participation in training.
# First compute unscaled score based on distance and motivation (and noise)
prop_score <- motivation - distance + rnorm(n) %>% scales::rescale()

# Generate a 0/1 variable using the propensity score
program <- ifelse(prop_score >= median(prop_score), 1, 0)

# Y -> Outcome: Work efficiency (KPI)
kpi_score <- program + motivation + rnorm(n)

# Add variables to tibble
df <- tibble(
  distance   = distance,
  program    = program,
  motivation = motivation,
  kpi        = kpi_score
)

df %>% head()

# [4] Variable relationships ----
# Correlation matrix
cor_df <- round(cor(df), 2)
cor_df

# # Scatter plots
# my_cols <- c("#00AFBB", "#FC4E07")  
# pairs(df, pch = 19, cex = .25, 
#       col = my_cols[as.factor(df$program)],
#       lower.panel = NULL)
# 
# # Relationship between omitted variable and outcome: Omitted variable has an
# # impact on the outcome. However, the variable is unobserved and therefore, to
# # eliminate omitted variable bias, we need to use an instrument. Here, 
# # unobserved motivation increases the expected outcome.
# ggplot(df, aes(x = motivation, y = kpi_score, color = as.factor(program))) +
#   geom_point(alpha = .2) +
#   geom_smooth(method = "lm")
# 
# # Relationship between instrument and treatment. One requirement of a valid 
# # instrument is its impact on the treatment variable. In this case, workers
# # living/working closer to the training location are more likely to participate
# # in the training program.
# ggplot(df, aes(x = distance, fill = as.factor(program))) +
#   geom_density(alpha = .5)
# 
# # Relationship between treatment and outcome. Of course, there also needs to
# # be a correlation between outcome and treatment.
# ggplot(df, aes(x = as.factor(program), y = kpi, color = as.factor(program))) +
#   stat_summary(geom = "pointrange", fun.data = "mean_se")

# [5] Modeling ----
# First of all, let's look at the coefficients of the "full" (but unobservable)
# model. It is unobservable, as it includes motivation, which in reality is 
# a variable that is very hard to collect or measure.
# Coefficients are expected to be close to what he have defined in the data
# generation section.
model_full <- lm(kpi ~ program + motivation, data = df)
broom::tidy(model_full)

# Modeling the data without the unobservable variable, i.e. only including 
# program participation in this case, returns a biased coefficient as the 
# relationship between program and the outcome is biased by a collider.
model_biased <- lm(kpi ~ program, data = df)
broom::tidy(model_biased)

# 2SLS
# First stage
first_stage <- lm(program ~ distance, data = df)
summary(first_stage)

# Second stage
second_stage <- lm(df$kpi ~ first_stage$fitted.values)
summary(second_stage)

# Using our instrument (distance to training location), we try to eliminate the
# bias induced by the omitted variable. If all assumptions regarding the 
# validity of our instrument are met, the resulting coefficient should be
# close to what we have defined above.
library(estimatr)
model_iv <- iv_robust(kpi ~ program | distance, data = df)
broom::tidy(model_iv)
