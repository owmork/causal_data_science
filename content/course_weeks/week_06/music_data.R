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

# Solution
music <- readRDS("content/course_weeks/week_06/music.rds")


# 1. Take a look at the formula rearrangements for the R - learner. Based on
# both rearrangements, construct what is needed to run the final regression.
# Prove the equivalence of both rearrangements by calculating the individual
# treatment effects and comparing them.

# Load packages
library(mlr3)
library(mlr3learners)
library(tidyverse)

## Approach 1:
## Specify
# Prediction model for the treatment/exposure
task_e <- as_task_classif(music |> select(-minutes_spent), target = "new_algo")
lrnr_e <- lrn("classif.svm", predict_type = "prob")

# Prediction model for the outcome
task_m <- as_task_regr(music |> select(-new_algo), target = "minutes_spent")
lrnr_m <- lrn("regr.ranger")

## Train
lrnr_m$train(task_m)
lrnr_e$train(task_e)

## Predict
m_pred_tbl <- lrnr_m$predict(task_m)
e_pred_tbl <- lrnr_e$predict(task_e)

## Residualize
# True values
D <- music$new_algo
Y <- music$minutes_spent

# Residuals
Y_res <- Y - m_pred_tbl$response
D_res <- D - e_pred_tbl$prob[, 2]

# Get matrix of unmodified covariates
X <- music |> select(-minutes_spent, -new_algo) |> mutate(intercept = 1)
X <- as.matrix(X)

# Modify by multiplying with residuals
X_tilde <- X * D_res

# Partially linear R-Learner
rl_pl <- lm(Y_res ~ 0 + X_tilde)
summary(rl_pl)

# Multiply covariates with coefficient vector
rl_pl_CATE <- X %*% rl_pl$coefficients
hist(rl_pl_CATE, breaks = 30)

## Approach 2:

pseudo_rl <- Y_res / D_res
weights_rl <- D_res ^ 2

r_learner_mod2 <- lm(pseudo_rl ~ 0 + X, weights = weights_rl)
summary(r_learner_mod2)
all.equal(as.numeric(rl_pl$coefficients),
          as.numeric(r_learner_mod2$coefficients))
unname(unlist(predict(r_learner_mod2)))


all(rl_pl_CATE == unname(unlist(predict(r_learner_mod2))))


## Task 2
# library(GenericML)
# # Specify set of learners
# lrnr <- c("mlr3::lrn('svm')", 'mlr3::lrn("ranger", num.trees = 100)')
# X1 <- setup_X1(funs_Z = c("S", "B", "p"))
# 
# gen_ML <- GenericML(
#   # data
#   Z = X, 
#   D = D,
#   Y = Y,
#   # learners
#   learners_GenericML = lrnr,
#   learner_propensity_score = "lasso",
#   # algorithm
#   num_splits = 10,
#   quantile_cutoffs = c(0.2, 0.4, 0.6, 0.8),
#   # regression setup
#   X1_BLP = X1,
#   X1_GATES = X1,
#   # computational issues
#   parallel = TRUE, 
#   num_cores = 6, 
#   seed = 11
# )
# 
# get_BLP(gen_ML, plot = TRUE)
# get_GATES(gen_ML, plot = TRUE)
# get_CLAN(gen_ML, variable = "app_usage_frequency", plot = TRUE)
# get_CLAN(gen_ML, variable = "prior_minutes", plot = TRUE)


library(DoubleML)

# Specify data object
email_dml_data <- DoubleMLData$new(
  data = as.data.frame(music),
  y_col = "minutes_spent", 
  d_cols = "new_algo"
)

dml_aipw_obj = DoubleMLIRM$new(
  data = email_dml_data,
  ml_g = lrnr_m,
  ml_m = lrnr_e,
  score = "ATE",
  trimming_threshold = 0.01, # to prevent too extreme weights
  apply_cross_fitting = TRUE,
  n_folds = 10)

# Fit and return summary
dml_aipw_obj$fit()
dml_aipw_obj$summary()

summary(lm(dml_aipw_obj$psi_b ~ music$prior_minutes))
summary(lm(dml_aipw_obj$psi_b ~ music$app_usage_frequency))

library(np)
np_model = npreg(dml_aipw_obj$psi_b ~ music$app_usage_frequency)  # kernel regression
plot(np_model)  # plot the kernel regression


