library(tidyverse)

set.seed(11)

# add: marketing campaign, competitor pricing, rating = rating, time_of_day = time of day

# Price elasticity
price_elast <- function(price, weekday, rating, temperature) {
  return(-8 + 0.2 * price + 2 * as.numeric(weekday %in% c(1, 7)) + 0.25 * (rating) - 1.8*(temperature/20 - 14/20)^2)
}

price_elast <- function(price, weekday, rating, temperature) {
  ela <- -8 + 2 * as.numeric(weekday %in% c(1, 7)) + 0.25 * (rating) - 4*(temperature/20 - 14/20)^2
  ela <- ela / 2.5
  return(ela)
}

map(10:20, ~price_elast(.x, 1, 4, 15))
map(1:2, ~price_elast(25, .x, 4, 15))
map(3:5, ~price_elast(25, 1, .x, 15))
map(c(5, 15, 25), ~price_elast(25, 1, 4, .x))

# Sales
sales <- function(price, weekday, rating, temperature) {
  elast <- -abs(price_elast(price, weekday, rating, temperature))
  output <-
    rnorm(
      length(price),
      mean = 130 + 10 * as.numeric(weekday %in% c(1, 7)) + 1 * elast * price - 5*(temperature/20 - 14/20)^2 ,
      sd = 10
    )
  return(as.integer(output))
}

n <- 10000

weekday <- sample(1:7, n, replace = TRUE)
rating <- sample(seq(3.5, 4.9, .1), n, replace = TRUE)
temperature <- round(rnorm(n, mean = 18, sd = 5), 1)
price <- round(rnorm(n, mean = 25 + rating + as.numeric(weekday %in% c(1, 7)), sd = 1), 1)

prices <- tibble(
  weekday = weekday,
  rating = rating,
  temperature = temperature,
  price = price,
  sales = abs(sales(price, weekday, rating, temperature))
  )

summary(prices)

saveRDS(prices |> select(-temperature), "content/course_weeks/week_04/prices.rds")
saveRDS(prices, "content/course_weeks/week_04/prices_new.rds")


# Naive estimate
mod_naive <- lm(sales ~ price, data = prices)
summary(mod_naive)

# OLS
mod_ols <- lm(sales ~ price + as.factor(weekday) + rating, data = prices)
mod_ols <- lm(sales ~ price + as.factor(weekday) + rating + temperature, data = prices)
mod_ols <- lm(sales ~ price + as.factor(weekday) + rating + temperature + I(temperature^2), data = prices)
summary(mod_ols)

# (1) Debiasing:
mod_D <- lm(price ~ as.factor(weekday) + rating + temperature, prices)
D_hat <- mod_D$residuals

# (2) Denoising:
mod_Y <- lm(sales ~ as.factor(weekday) + rating + temperature, prices)
Y_hat <- mod_Y$residuals

# (3) Residual regression
mod_fwl <- lm(Y_hat ~ 0 + D_hat)
summary(mod_fwl)


ggplot(prices, aes(y = sales, x = price)) +
  geom_point(alpha = .2) +
  geom_smooth(method='lm') +
  labs(x = "Price (X)", y = "Sales (Y)")


# Add residuals to data frame
prices <- prices |> mutate(sales_hat = Y_hat, price_hat = D_hat)

# Plot
ggplot(prices, aes(y = sales_hat, x = price_hat)) +
  geom_point(alpha = .2) +
  geom_smooth(method='lm') +
  labs(x = "Price residuals (X_hat)", y = "Sales residuals (Y_hat)")


