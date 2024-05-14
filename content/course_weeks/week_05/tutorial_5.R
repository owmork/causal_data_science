# Load tidyverse package
library(tidyverse)

email <- read_csv("email_obs.csv")

# Data overview
glimpse(email)

# Frisch--Waugh--Lovell Theorem ----

### Q1:
# Frisch–Waugh–Lovell Theorem: 3-step procedure

# (1) Debiasing:

# (2) Denoising:

# (3) Residual regression

# Double machine learning with partially linear regression ----

# Load packages
library(mlr3)
library(mlr3learners)

# Prediction model for the treatment/exposure
task_e <- as_task_classif(email |> select(-next_mnth_pv), target = "mkt_email")
lrnr_e <- lrn("classif.log_reg", predict_type = "prob")

# Prediction model for the outcome
task_m <- as_task_regr(email |> select(-mkt_email), target = "next_mnth_pv")
lrnr_m <- lrn("regr.lm")

# Initialize nuisance vectors
n <- nrow(email)
m_hat <- rep(NA, n)
e_hat <- rep(NA, n)

# Split sample
ids_s1 <- sample(1:n, n/2) # indices of sample 1
ids_s2 <- which(!1:n %in% ids_s1) # indices of sample 2

# Iteration 1:
# Train: S1 - Predict: S2
# Y ~ X
lrnr_m$train(task_m, row_ids = ids_s1)
m_hat[ids_s2] <- lrnr_m$predict(task_m, row_ids = ids_s2)$response
# D ~ X
lrnr_e$train(task_e, row_ids = ids_s1)
e_hat[ids_s2] <- lrnr_e$predict(task_e, row_ids = ids_s2)$prob[, 2] # col 2 for value D = 1

### Q2:
# ...

### Q3a:
# ...

### Q3b:
# ...

# Double machine learning with augmented inverse probability weighting ----

# Initialize nuisance vectors
e_hat <- rep(NA,n)
m0_hat <- rep(NA,n) # untreated
m1_hat <- rep(NA,n) # treated

# Treatment indices
ids_d0 <- which(email$mkt_email==0)
ids_d1 <- which(email$mkt_email==1)

# Iteration 1:
# Train in S1, predict in S2
# D ~ X
lrnr_e$train(task_e, row_ids = ids_s1)
e_hat[ids_s2] <- lrnr_e$predict(task_e, row_ids = ids_s2)$prob[, 2]
# Y0 ~ X
lrnr_m$train(task_m, row_ids = intersect(ids_s1, ids_d0))
m0_hat[ids_s2] <- lrnr_m$predict(task_m, row_ids = ids_s2)$response
# Y1 ~ X
lrnr_m$train(task_m, row_ids = intersect(ids_s1, ids_d1))
m1_hat[ids_s2] <- lrnr_m$predict(task_m, row_ids = ids_s2)$response

### Q4:
# ...

### Q5:
# ...

# Obtain statistical inference (same as t-test)
mod_aipw <- lm(Y_ate ~ 1)
summary(mod_aipw)
