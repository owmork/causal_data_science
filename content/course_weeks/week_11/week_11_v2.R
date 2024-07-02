# Generate plot of synthetic unit and observed treated unit
plot_synth <- function(SC_pre, SC_post) {
  # Data to plot
  sc_vs_trt <- tibble(
    period = unique(ride$period), # periods (will be replicated)
    y_synth = c(SC_pre, SC_post), # synthetic outcomes
    y_treat = c(y1_pre, y1_post) # observed outcomes of treated unit
  ) |> 
    # long format for ggplot
    pivot_longer(cols = c("y_synth", "y_treat"))
  
  ggplot(sc_vs_trt,
         aes(
           x = period, 
           y = value, 
           group = name, 
           color = name)
         ) +
    geom_line() +
    geom_vline(xintercept = T0, linetype = "dashed")
}

# Minimization subject to constraints
get_weights <- function(X, y) {
  
  # Initialize weights
  w <- Variable(ncol(X))
  # Objective: minimize sum of squares (reproduce y)
  objective <- Minimize(sum_squares(X %*% w - y))
  # Subject to constraints
  constraints <- list(sum(w) == 1, w >= 0)
  # Define problem
  problem <- Problem(objective, constraints)
  # Solve and get weights
  w_star <- as.vector(solve(problem)$getValue(w))
  
  return(w_star)
}

library(tidyverse)

# ride
ride <- readRDS("content/course_weeks/week_11/ride.rds")
all_cities <- unique(ride$city)
ctrl_sample <- sample(all_cities[all_cities!="Chicago"], 6)

ride <- ride |> filter(city %in% c(ctrl_sample, "Chicago"))

# () exploration
# How many units?
N1 <- n_distinct(ride[ride$treated == 1, ]$city)
N0 <- n_distinct(ride[ride$treated == 0, ]$city)

# How many periods?
TT <- n_distinct(unique(ride$period))

# What is first treatment period?
T0 <- min(ride[ride$post == 1, ]$period)

# () plot
ride |> 
  ggplot(aes(
    x = period,
    y = revenue,
    group = city,
    color = factor(treated),
    linewidth = factor(treated)
  ))+
  geom_line(aes(alpha = factor(treated))) + 
  geom_vline(xintercept = T0, linetype = "dashed") + 
  scale_alpha_manual(values = c(.25, 1)) +
  scale_linewidth_manual(values = c(.5, .8)) + 
  labs(color = "Treatment group", alpha = "Treatment group", linewidth = "Treatment group")

# () table 1a
ride |> 
  group_by(treated) |> 
  summarise(
    mean(density),
    mean(employment),
    mean(gdp), 
    mean(population),
    mean(revenue)
  )

# () make 4 blocks
y0_pre <- ride |>
  filter(post == 0, treated == 0) |> 
  select(revenue, city, period) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "revenue") |> 
  select(-period) |> 
  as.matrix()

y1_pre <- ride |>
  filter(post == 0, treated == 1) |> 
  select(revenue, city, period) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "revenue") |> 
  select(-period) |> 
  as.matrix()

y0_post <- ride |>
  filter(post == 1, treated == 0) |> 
  select(revenue, city, period) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "revenue") |> 
  select(-period) |> 
  as.matrix()

y1_post <- ride |>
  filter(post == 1, treated == 1) |> 
  select(revenue, city, period) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "revenue") |> 
  select(-period) |> 
  as.matrix()

# () get weights with OLS only on pre-outcomes
ols <- lm(y1_pre ~ 0 + y0_pre)
coef(ols)
sum(coef(ols))

w_ols <- coef(ols)

# counterfactual
y1_post_mean_cf <- y0_post %*% w_ols

# ATT
mean(y1_post - y1_post_mean_cf)

# () plot
y_synth_pre <- y0_pre %*% w_ols
y_synth_post <- y0_post %*% w_ols

sc_vs_trt <- tibble(
  period = unique(ride$period),
  y_synth = c(y_synth_pre, y_synth_post),
  y_treat = c(y1_pre, y1_post)
) |> 
  pivot_longer(cols = c("y_synth", "y_treat"))

ggplot(sc_vs_trt, aes(x = period, y = value, group = name, color = name)) +
  geom_line() +
  geom_vline(xintercept = T0, linetype = "dashed")


# () introduce V: more attention on periods prior to treatment

# uniform weights
a_uni <- rep(1, T_pre)
y0_pre_uni <- sweep(y0_pre, 1, a_uni, "*")
y1_pre_uni <- sweep(y1_pre, 1, a_uni, "*")

ols_uni <- lm(y1_pre_uni ~ 0 + y0_pre_uni)

coef(ols_uni)
sum(coef(ols_uni))
all(coef(ols) == coef(ols_uni))

# custom weights
a_adj <- seq(1, T_pre, 1)
a_adj <- T_pre*a_adj / sum(a_adj)
y0_pre_adj <- sweep(y0_pre, 1, a_adj, "*")
y1_pre_adj <- sweep(y1_pre, 1, a_adj, "*")

ols_adj <- lm(y1_pre ~ 0 + as.matrix(y0_pre_adj))

coef(ols_adj)
sum(coef(ols_adj))
all(coef(ols) == coef(ols_adj))

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

w <- Variable(ncol(y0_pre))
objective <- Minimize(sum_squares(y0_pre %*% w - y1_pre))
constraints <- list(sum(w) == 1, w >= 0)
problem <- Problem(objective, constraints)
loss <- solve(problem)$value
w_star <- as.vector(solve(problem)$getValue(w))

sum(w_star)
sum(w_star >= 0)
round(w_star, 2)

# () plot
y_synth_pre <- y0_pre %*% w_star
y_synth_post <- y0_post %*% w_star

plot_synth(w_star)

# () covariates
x0_pre <- ride |>
  filter(post == 0, treated == 0) |> 
  select(employment, city, period) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "employment") |> 
  select(-period)

V <- c(1, 0)
X_v <- V[1] * as.matrix(y0_pre) + V[2] * as.matrix(x0_pre)

w_star <- get_weights(X_v, y1_pre)
round(w_star, 2)

# custom weights
V <- c(1, 1)

X_v <- V[1] * y0_pre + V[2] * x0_pre

w_star <- get_weights(X_v, y1_pre)

round(w_star, 2)
y0_post %*% w_star

plot_synth(w_star)

# () package
# library(Synth)
# library(SCtools)
# data(basque)
# 
# ride$city_id <- as.numeric(factor(ride$city))
# 
# dataprep.out <- dataprep(
#   foo = as.data.frame(ride),
#   predictors = c("comp_download_pct"),
#   time.predictors.prior = 
#   dependent = "revenue",
#   unit.variable = "city_id", 
#   unit.names.variable = "city",
#   time.variabl = "period",
#   treatment.identifier = 45,
#   controls.identifier = (1:47)[(1:47) != 45]
# )
# 




#### Covariates

#Additionally to the pre-treatment outcomes, we can also include covariates when there is good reason to do so. The inclusion of covariates requires an additional weight matrix $V$ which indicates the weight of each covariate. The weight depends on the relative importance for the discrepancy between the treated and control units and on the predictive power for the counterfactual outcome. Typically, you would run cross-validation to find the optimal $V^*$. But here we first implement the weight matrix such that it yields the same estimate as before.


# With same logic as above, extract covariates
x0_pre <- ride |>
  filter(post == 0, treated == 0) |> 
  select(employment, city, period) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "employment") |> 
  select(-period)

# Define importance of "covariates" (actually of pre-treatment outcome vs covariates)
V <- c(1, 0)
X_v <- V[1] * as.matrix(y0_pre) + V[2] * as.matrix(x0_pre)

# Run optimization
w_v <- get_weights(X_v, y1_pre)
round(w_v, 2)
