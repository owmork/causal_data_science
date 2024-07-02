# TODO: remove state and population
# TODO: check pre-treatment fit
# TODO: change data and use one unit
# TODO: package

library(tidyverse)

mkt <- read_csv("/Users/oli/my_drive/02_TUHH/03_github/causal_ds_ss24/content/course_weeks/week_11/online_mkt_cov.csv")

# () exploration
# How many units?
N1 <- n_distinct(mkt[mkt$treated == 1, ]$city)
N0 <- n_distinct(mkt[mkt$treated == 0, ]$city)

# How many periods?
TT <- n_distinct(unique(mkt$date))

# What is first treatment period?
min(mkt[mkt$treated == 1, ]$date)

# () plot
mkt |> 
  ggplot(aes(
    x = date,
    y = app_download_pct,
    group = city,
    color = factor(treated),
    linewidth = factor(treated)
  ))+
  geom_line(aes(alpha = factor(treated))) + 
  geom_vline(xintercept = as.Date("2022-05-01"), linetype = "dashed") + 
  scale_alpha_manual(values = c(.25, 1)) +
  scale_linewidth_manual(values = c(.5, .8)) + 
  labs(color = "Treatment group", alpha = "Treatment group", linewidth = "Treatment group")

# () make 4 blocks
y0_pre <- mkt |>
  filter(post == 0, treated == 0) |> 
  select(app_download_pct, city, date) |> 
  pivot_wider(id_cols = "date", names_from = "city", values_from = "app_download_pct") |> 
  select(-date)

y1_pre <- mkt |>
  filter(post == 0, treated == 1) |> 
  select(app_download_pct, city, date) |> 
  pivot_wider(id_cols = "date", names_from = "city", values_from = "app_download_pct") |> 
  select(-date)

y0_post <- mkt |>
  filter(post == 1, treated == 0) |> 
  select(app_download_pct, city, date) |> 
  pivot_wider(id_cols = "date", names_from = "city", values_from = "app_download_pct") |> 
  select(-date)

y1_post <- mkt |>
  filter(post == 1, treated == 1) |> 
  select(app_download_pct, city, date) |> 
  pivot_wider(id_cols = "date", names_from = "city", values_from = "app_download_pct") |> 
  select(-date)

# () get weights with OLS only on pre-outcomes
y1_pre_mean <- rowMeans(y1_pre)

ols <- lm(y1_pre_mean ~ 0 + as.matrix(y0_pre))
coef(ols)
sum(coef(ols))

w_ols <- coef(ols)

# counterfactual
y1_post_mean_cf <- as.matrix(y0_post) %*% w_ols
# observed
y1_post_mean <- rowMeans(y1_post)
# ATT
mean(y1_post_mean - y1_post_mean_cf)

# () introduce V: more attention on periods prior to treatment

# uniform weights
a_uni <- rep(1, N)
y0_pre_uni <- a_uni * y0_pre

ols_uni <- lm(y1_pre_mean ~ 0 + as.matrix(y0_pre_uni))

coef(ols_uni)
sum(coef(ols_uni))
all(coef(ols) == coef(ols_uni))
round(coef(ols) - coef(ols_uni))

# custom weights
a_adj <- seq(1, N, 1)
a_adj <- N*a_adj / sum(a_adj)
y0_pre_adj <- a_adj * y0_pre

ols_adj <- lm(y1_pre_mean ~ 0 + as.matrix(y0_pre_adj))

coef(ols_adj)
sum(coef(ols_adj))
all(coef(ols) == coef(ols_adj))
sum(round(coef(ols) - coef(ols_adj))>0)

# # () lasso regression
# library(glmnet)
# 
# x = Y_pre_v |> select(-treat_mean)
# y = Y_pre_v$treat_mean
# 
# lambdas <- 10^seq(2, -3, by = -.1)
# lasso_cv <- cv.glmnet(as.matrix(x), y, alpha = 0, lambda = lambdas)
# lasso_mod <- glmnet(as.matrix(x), y, alpha = 1, lambda = lasso_cv$lambda.min)
# summary(lasso_mod)
# round(lasso_mod$beta, 2) |> sort()

# () constraints: nonnegative and sum to 1
library(CVXR)

w <- Variable(ncol(as.matrix(y0_pre)))
objective <- Minimize(sum_squares(as.matrix(y0_pre) %*% w - y1_pre_mean))
constraints <- list(sum(w) == 1, w >= 0)
problem <- Problem(objective, constraints)
loss <- solve(problem)$value
w_star <- as.vector(solve(problem)$getValue(w))

sum(w_star)
sum(w_star >= 0)
sum(w_star == 0)
sum(bazar::almost.zero(w_star, tolerance = 1e-3))

# () covariates
x0_pre <- mkt |>
  filter(post == 0, treated == 0) |> 
  select(comp_download_pct, city, date) |> 
  pivot_wider(id_cols = "date", names_from = "city", values_from = "comp_download_pct") |> 
  select(-date)


V <- c(1, 0)

X_v <- V[1] * as.matrix(y0_pre) + V[2] * as.matrix(x0_pre)

w <- Variable(ncol(as.matrix(X_v)))
objective <- Minimize(sum_squares(as.matrix(X_v) %*% w - y1_pre_mean))
constraints <- list(sum(w) == 1, w >= 0)
problem <- Problem(objective, constraints)
loss <- solve(problem)$value
w_star <- as.vector(solve(problem)$getValue(w))

round(w_star, 2)

# custom weights
V <- c(1.88, 0.02)

X_v <- V[1] * as.matrix(y0_pre) + V[2] * as.matrix(x0_pre)

w <- Variable(ncol(as.matrix(X_v)))
objective <- Minimize(sum_squares(as.matrix(X_v) %*% w - y1_pre_mean))
constraints <- list(sum(w) == 1, w >= 0)
problem <- Problem(objective, constraints)
loss <- solve(problem)$value
w_star <- as.vector(solve(problem)$getValue(w))

round(w_star, 2)
round(rowMeans(w_star * y0_post), 3)

# () plot
# treatment observed
# synthetic control

y_synth_pre <- as.matrix(y0_pre) %*% w_star
y_synth_post <- as.matrix(y0_post) %*% w_star

SC_plot_df <- tibble(
  date = unique(mkt$date),
  y_synth = c(y_synth_pre, y_synth_pre),
  y_treat = c(y1_pre_mean, y1_post_mean)
  ) |> 
  pivot_longer(cols = c("y_synth", "y_treat"))

SC_plot_df |> ggplot(aes(x = date, y = value, group = name, color = name)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2022-05-01"), linetype = "dashed")

# () package
# library(Synth)
# library(SCtools)
# data(basque)
# 
# mkt$city_id <- as.numeric(factor(mkt$city))
# 
# dataprep.out <- dataprep(
#   foo = as.data.frame(mkt),
#   predictors = c("comp_download_pct"),
#   time.predictors.prior = 
#   dependent = "app_download_pct",
#   unit.variable = "city_id", 
#   unit.names.variable = "city",
#   time.variabl = "date",
#   treatment.identifier = 45,
#   controls.identifier = (1:47)[(1:47) != 45]
# )
# 
