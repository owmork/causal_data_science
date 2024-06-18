# Load necessary libraries
library(MASS)  # for mvrnorm
library(dplyr)
library(fixest)

# Sample size
n <- 200
# Number of pre-treatment periods
n_pre_periods <- 8
# Number of post-treatment periods
n_post_periods <- 1
# Total periods
total_periods <- n_pre_periods + n_post_periods
# pscore index (strength of common support)
Xsi.ps <- .75
# Proportion in each period (post period)
lambda <- 1 / total_periods

# Mean and Std deviation of Z's without truncation
mean.z1 <- exp(0.25 / 2)
sd.z1 <- sqrt((exp(0.25) - 1) * exp(0.25))
mean.z2 <- 10
sd.z2 <- 0.54164
mean.z3 <- 0.21887
sd.z3 <- 0.04453
mean.z4 <- 402
sd.z4 <- 56.63891

set.seed(1234)

# Generate covariates for all individuals (time invariant)
x1 <- stats::rnorm(n, mean = 0, sd = 1)
x2 <- stats::rnorm(n, mean = 0, sd = 1)
x3 <- stats::rnorm(n, mean = 0, sd = 1)
x4 <- stats::rnorm(n, mean = 0, sd = 1)

z1 <- exp(x1 / 2)
z2 <- x2 / (1 + exp(x1)) + 10
z3 <- (x1 * x3 / 25 + 0.6)^3
z4 <- (x1 + x4 + 20)^2

z1 <- (z1 - mean.z1) / sd.z1
z2 <- (z2 - mean.z2) / sd.z2
z3 <- (z3 - mean.z3) / sd.z3
z4 <- (z4 - mean.z4) / sd.z4

# # replace x1
# x1 <- stats::rnorm(n, mean = 20, sd = 4)
# z1 <- exp(x1 / 2)
# mean.z1 <- mean(z1)
# sd.z1 <- sd(z1)
# z1 <- (z1 - mean.z1) / sd.z1

x <- cbind(x1, x2, x3, x4)
z <- cbind(z1, z2, z3, z4)

# Generate treatment groups
# Propensity score
pi <- stats::plogis(Xsi.ps * (-z1 + 0.5 * z2 - 0.25 * z3 - 0.1 * z4))
d <- as.numeric(runif(n) <= pi)

# Generate aux indexes for the potential outcomes
index.lin <- 210 + 27.4 * z1 + 13.7 * (z2 + z3 + z4)
index.unobs.het <- d * (index.lin)
index.att <- 0

# This is the key for consistency of outcome regression
index.trend <- 210 + 27.4 * z1 + 13.7 * (z2 + z3 + z4)

# v is the unobserved heterogeneity
v <- stats::rnorm(n, mean = index.unobs.het, sd = 1)

# Generate realized outcomes for all periods
y <- matrix(0, n, total_periods)
for (t in 1:n_pre_periods) {
  y[, t] <- index.lin + v + stats::rnorm(n) + (t - 1) * index.trend / n_pre_periods + (1-d)*100
}

# Generate outcomes for the post-treatment period
y10 <- index.lin + v + stats::rnorm(n, mean = 0, sd = 1) + index.trend + (1-d)*100
y11 <- index.lin + v + stats::rnorm(n, mean = 0, sd = 1) + index.trend + index.att + 20 # Adding true treatment effect of 1
y[, total_periods] <- d * y11 + (1 - d) * y10

# Create panel data structure
id <- rep(1:n, each = total_periods)
period <- rep(1:total_periods, times = n)
post <- ifelse(period == total_periods, 1, 0)
y <- as.vector(t(y)) |> round(0) + stats::rnorm(n, mean = 0, sd = 15)
d <- rep(d, each = total_periods)
x1 <- rep(z1 + 45, each = total_periods) # age
x2 <- rep(scales::rescale(z2, to = c(0, .3)), each = total_periods) # user share
x3 <- rep(ntile(z3, 15), each = total_periods) # population size group
x4 <- rep(scales::rescale(z4, to = c(0.99, 4.99)), each = total_periods) # competitor price

# Combine into a data frame
sim_panel <- data.frame(id = id, period = period, post = post, y = y, d = d,
                        x1 = x1, x2 = x2, x3 = x3, x4 = x4)

# Order by id and period
sim_panel <- sim_panel %>% arrange(id, period)

head(sim_panel)

sim_panel$time_to_treat <- 0
sim_panel[sim_panel$d==1, ]$time_to_treat <- sim_panel[sim_panel$d==1, ]$period - 9

sim_panel_22 <- sim_panel |> filter(period %in% 8:9)

summary(feols(y ~ post:d | id + post, data = sim_panel_22))
summary(feols(y ~ post:d + post:x1 + post:x2 + post:x3 + post:x4 | id + post, data = sim_panel_22))

evt_stdy <- feols(y ~ i(time_to_treat, d, ref = -1) | id + period, data = sim_panel)
summary(evt_stdy)
iplot(evt_stdy)

evt_stdy <- feols(y ~ i(time_to_treat, d, ref = -1) + period:x1 + period:x2 + period:x3 + period:x4 | 
                    id + period, data = sim_panel)
summary(evt_stdy)
iplot(evt_stdy)

DRDID::drdid(
  yname = "y",
  tname = "post",
  idname = "id",
  dname = "d",
  xformla = ~x1+x2+x3+x4,
  data = sim_panel_22
)

df <- sim_panel |> 
  rename(
    city = id,
    month = period,
    downloads = y,
    ever_treated = d,
    time_to_treat = time_to_treat,
    age = x1,
    user_share = x2,
    population_size = x3,
    competitor_price = x4
  ) |> 
  select(-time_to_treat)

saveRDS(df, "content/course_weeks/week_09/mkt_panel.rds")
