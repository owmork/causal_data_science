library(tidyverse)

generate_data <- function(city='Chicago', year=2010, seed=1) {
  set.seed(seed)
  
  # Load Data
  df <- read.csv('content/course_weeks/week_11/us_cities_20022019_clean.csv')
  df <- df[df$year > 2008 & df$year < 2014, ]
  
  # Expand to monthly data
  df <- df[rep(seq_len(nrow(df)), each=12), ]
  df$month <- rep(1:12, times=nrow(df)/12)
  
  # Select only big cities
  df$mean_pop <- ave(df$population, df$city, FUN=mean)
  df <- df[df$mean_pop > 1, ]
  df$mean_pop <- NULL
  
  # Treatment
  df$treated <- df$city == city
  df$post <- df$year >= year
  df$cohort <- Inf
  df[df$city == city, ]$cohort <- year
  
  # Generate revenue with increased revenue during summer months
  df$summer <- df$month %in% 6:8
  df$pre_summer <- df$month %in% 4:5
  df$post_summer <- df$month %in% 9:10
  df$revenue <- df$gdp + sqrt(df$population) + 
    20 * sqrt(df$employment) - df$density / 100 + 
    (df$year - 1990) / 5 + rnorm(nrow(df), mean=0, sd=1) + 
    df$treated * df$post * log(pmax(2, df$year - year)) +
    ifelse(df$summer, 5, 0) + ifelse(df$pre_summer, 2, 0) + ifelse(df$post_summer, 2, 0) # Increase revenue by 5 during summer months
  
  # Create date column
  df$period <- as.Date(paste(df$year, df$month, "01", sep="-"), format="%Y-%m-%d")
  
  return(tibble(df))
}


# Example usage
df <- generate_data(city='Chicago', year=2010, seed=1)
# head(df)

ride <- tibble(df) |> 
  select(-year, -month, -summer, -pre_summer, -post_summer)

all_cities <- unique(ride$city)
ctrl_sample <- sample(all_cities[all_cities!="Chicago"], 12)

ride <- ride |> filter(city %in% c(ctrl_sample, "Chicago")) |>
  mutate(treat_post = treated*post)

# Augmented Synthetic Controls

library(augsynth)

# outcome ~ treatment | auxiliary covariates
results <- augsynth(revenue ~ treat_post | gdp + population + log(gdp), 
                    unit = city, 
                    time = period, 
                    data = ride,
                    progfunc = "Ridge",#  function to use to impute control outcomes
                    scm = TRUE) # whether to use the SCM
summary(results) # summarize the results
plot(results) # plot the results

## Staggered adoption
df1 <- generate_data(city='Chicago', year=2010, seed=1)
df2 <- generate_data(city='Phoenix', year=2010, seed=1)

df3 <- generate_data(city='Las Vegas', year=2011, seed=1)
df4 <- generate_data(city='Seattle', year=2011, seed=1)

df5 <- generate_data(city='Houston', year=2012, seed=1)
df6 <- generate_data(city='Washington', year=2012, seed=1)

ride_12 <- bind_rows(df1, df2, df3, df4, df5, df6) |> 
  arrange(-treated, city, period) |> 
  distinct(city, period, .keep_all = TRUE) |> 
  mutate(post = if_else(treated, year >= cohort, 0))

results <- multisynth(revenue ~ post | gdp + population + log(gdp), 
                    unit = city, 
                    time = period, 
                    data = ride_12)#,
                    #progfunc = "Ridge",#  function to use to impute control outcomes
                    #scm = T) # whether to use the SCM
summary(results) # summarize the results
plot(results) # plot the results
plot(results, levels = "Average") # plot the results


### Panel view
library("panelView")

panelview(revenue ~ post, 
          data = ride_12,
          index = c("city","period"), 
          xlab = "Period", 
          ylab = "City",
          by.timing = TRUE,
          pre.post = TRUE
          )

panelview(
  revenue ~ post,
  data = ride_12,
  index = c("city","period"), 
  type = "outcome",
  main = "Revenue",
  xlab = "Period",
  ylab = "Revenue"
)

panelview(
  revenue ~ post,
  data = ride_12,
  index = c("city","period"), 
  type = "outcome",
  main = "Revenue",
  xlab = "Period",
  ylab = "Revenue", 
  id = c("Chicago", "Phoenix", "Las Vegas", "Seattle", "Houston", "Washington"),
  by.group = TRUE
)


### Synthetic DID

# Split and extract 4 matrices
y0_pre <- ride |>
  filter(post == 0, treated == 0) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "revenue") |> 
  select(-period) |> 
  as.matrix()

y1_pre <- ride |>
  filter(post == 0, treated == 1) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "revenue") |> 
  select(-period) |> 
  as.matrix()

y0_post <- ride |>
  filter(post == 1, treated == 0) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "revenue") |> 
  select(-period) |> 
  as.matrix()

y1_post <- ride |>
  filter(post == 1, treated == 1) |> 
  pivot_wider(id_cols = "period", names_from = "city", values_from = "revenue") |> 
  select(-period) |> 
  as.matrix()

library(CVXR)

# Optimization procedure
w <- Variable(ncol(y0_pre))
objective <- Minimize(sum_squares(y0_pre %*% w - y1_pre))
constraints <- list(sum(w) == 1, w >= 0)
problem <- Problem(objective, constraints)
w_star <- as.vector(solve(problem)$getValue(w))

unit_w <- tibble(city = colnames(y0_pre), unit_weight = w_star)

ride_w <- ride |> 
  left_join(unit_w, by = "city") |> 
  mutate(unit_weight = replace_na(unit_weight, mean(ride$treated)))

mod_w <- lm(revenue ~ treat_post + factor(period), 
          data = ride_w |> filter(unit_weight > 1e-10), 
          weights = unit_weight)
mod_w$coefficients[2]
mean(y1_post - (y0_post %*% w_star))

# Time weights
b <- Variable(nrow(y0_pre)+1)
t_y0_pre <- t(y0_pre)
icept <- matrix(rep(1, nrow(t_y0_pre)), ncol = 1)
t_y0_pre <- cbind(icept, t_y0_pre)
objective <- Minimize(sum_squares(t_y0_pre %*% b - colMeans(y0_post)))
constraints <- list(sum(b[2:ncol(t_y0_pre)]) == 1, b[2:ncol(t_y0_pre)] >= 0)
problem <- Problem(objective, constraints)
b_star <- as.vector(solve(problem)$getValue(b))
b_star <- b_star[2:ncol(t_y0_pre)]

pre_periods <- ride |> filter(post == 0, treated == 0) |> pull(period) |> unique()
time_w <- tibble(period = pre_periods, time_weight = b_star)

plot(time_w)

ride_ww <- ride_w |> 
  left_join(time_w, by = "period") |> 
  mutate(time_weight = replace_na(time_weight, mean(ride$post))) |> 
  mutate(weight = unit_weight * time_weight)

mod_ww <- lm(revenue ~ treat_post, 
   data = ride_ww |> filter(weight > 1e-10), 
   weights = weight)
summary(mod_ww)
mod_ww$coefficients[2]


library(synthdid)

setup = panel.matrices(
  panel = as.data.frame(ride),
  unit = "city",
  time = "period",
  outcome = "revenue",
  treatment = "treat_post",
  treated.last = TRUE
)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


saveRDS(ride, "content/course_weeks/week_12/ride_12_single.rds")
saveRDS(ride_12, "content/course_weeks/week_12/ride_12_multi.rds")

