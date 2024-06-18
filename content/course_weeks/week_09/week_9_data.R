library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)

library(fixest)

set.seed(123)

diff_month <- function(end_date, start_date) {
  end_date <- as.Date(end_date)
  start_date <- as.Date(start_date)
  round(12 * (zoo::as.yearmon(end_date) - zoo::as.yearmon(start_date)))
}


# Generate the date range
date <- seq(ymd("2021-01-01"), ymd("2022-12-01"), by = "month")

# Define the cohorts
cohorts <- as.Date(c("2021-12-01", "2022-02-01", "2022-04-01"))
cohorts <- as.Date(c("2021-12-01", "2021-12-01", "2021-12-01"))

# Define possible regions and their respective properties
poss_regions <- c("S", "N", "W", "E")
reg_ps <- c(S = 0.3, N = 0.6, W = 0.7, E = 0.8)
reg_fe <- c(S = 20, N = 16, W = 8, E = 2)
reg_trend <- c(S = 0, N = 0.2, W = 0.4, E = 0.6)

# Possible areas (city size) and properties for area fixed effects
poss_areas <- c("small", "mid", "large")
are_p <- c(small = .5, mid = .3, large = .25)
are_fe <- c(small = 1, mid = 2, large = 3)
are_mean_age <- c(small = 59, mid = 50, large = 42)
are_mean_use <- c(small = .05, mid = .1, large = .15)


# Define units
units <- 1:200

# Assign regions and treatments
unit_reg <- sample(poss_regions, length(units), replace = TRUE)
unit_are <- sample(poss_areas, length(units), replace = TRUE, prob = are_p)
exp_trend <- rexp(length(units), rate = 0.01)
treated_unit <- rbinom(length(units), 1, reg_ps[unit_reg])

# Create the time fixed effects vector
time_fe <- rnorm(length(date))

# Create the data frame
df <- expand.grid(date = date, city = units) %>%
  mutate(region = unit_reg[city],
         area = unit_are[city],
         treated_unit = treated_unit[city],
         cohort = sample(cohorts, length(units), replace = TRUE)[city],
         eff_heter = rexp(length(units), rate = 1)[city],
         unit_fe = rnorm(length(units), mean = 0, sd = 2)[city],
         time_fe = rep(time_fe, each = length(units)),
         m_seas = (abs(6 - month(date)) %% 12),
         # ADDED
         age_mean = rnorm(length(units), mean = are_mean_age[unit_are], sd = 5)[city],
         use_share = rnorm(length(units), mean = are_mean_use[unit_are], sd =.025)[city],
         competitor_mkt = rbinom(nrow(.), size = 1, p = .2) + rnorm(nrow(.), 1, .2)*treated_unit
         ) %>%
  mutate(reg_fe = reg_fe[region],
         are_fe = are_fe[area],
         reg_trend = reg_trend[region],
         reg_ps = reg_ps[region],
         trend = as.numeric(date - min(date)),
         day = as.numeric(date - min(date)),
         cohort = if_else(treated_unit == 1, as.Date(cohort), as.Date("2100-01-01")),
         treated = as.integer(date >= cohort & treated_unit == 1),
         time_to_treat = ifelse(treated_unit == 1, diff_month(date, "2021-12-01"), 0)
         ) %>%
  mutate(y0 = round(10 +
                      treated_unit +
                      #reg_trend * trend / 60 +
                      #reg_trend * trend * (cohort!=as.Date("2100-01-01"))  / 60 +
                      #0.2*abs(12-time_to_treat) * (cohort!=as.Date("2100-01-01")) * competitor_mkt +
                      0.2*(12-time_to_treat) * (cohort!=as.Date("2100-01-01")) * competitor_mkt +
                      #trend * age_mean +
                      unit_fe +
                      0.4 * time_fe +
                      1 * reg_fe +
                      are_fe +
                      m_seas / 5 -
                      2 * competitor_mkt -
                      0.2 * age_mean -
                      10 * use_share, 
                    0)) %>%
  mutate(
    #y1 = y0 + 1 + pmin(0.2 * 1 + pmax(0, time_to_treat), 1) * eff_heter * 2,
    y1 = y0 + 1 + 0.2 * 1 * eff_heter * 2,
    #y1 = y0 + 1,
    tau = y1 - y0,
    #downloads = round(ifelse(treated == 1, y1, y0) + rnorm(n(), mean = 0, sd = 0.7), 0)
    downloads = round(ifelse(treated == 1, y1, y0) + rnorm(n(), mean = 15, sd = 1), 0)
    )

# Filter the data
reg_filter <- c("N", "S", "E", "W")
mkt_data_all <- df %>%
  filter(region %in% reg_filter, date <= as.Date("2022-12-01")) %>%
  select(-reg_fe, -time_fe, -cohort, -m_seas, -reg_trend, -trend, -day, -unit_fe, -y0, -y1, -eff_heter, -reg_ps, -treated_unit) %>%
  mutate(post = as.integer(date >= as.Date("2021-12-01"))) %>%
  group_by(city) %>%
  mutate(
    treated = max(treated),
    time_to_treat = ifelse(treated == 1, diff_month(date, "2021-12-01"), 0)
    ) %>%
  ungroup()

# add competitor something
# cor(mkt_data_all$downloads, mkt_data_all$competitor_mkt)
# cor(mkt_data_all$downloads, mkt_data_all$age_mean)
# cor(mkt_data_all$downloads, mkt_data_all$use_share)

# referral program
# clustered standard error

es <- feols(downloads ~ i(time_to_treat, treated, ref = -1)  | city + date, data = mkt_data_all)
iplot(es)

# Data 2 x 2:
df_22 <- mkt_data_all |> filter(date %in% c("2021-11-01", "2021-12-01"))

# Data T_pre+1 x 2:  
df_T2 <- mkt_data_all |> filter(date <= "2021-12-01")

es <- feols(downloads ~ i(time_to_treat, treated, ref = -1) | city + date, data = df_T2)
summary(es)

# 6
iplot(es)


# task 1: count
# add: plot implicit assumption of PO
# task 2: plot pre and post
# add: table of cannonical DID
# task 3: compute pre and post agg (cannonical)
# task 4: compute with lm and twfe
# add: parallel trends?
# task 5: compute event study
# task 6: plot event study
# add: conditional parallel trends?
# task 7: covariates into twfe did
# task 8: covariates into twfe event study
# task 9: doubly robust "2x2"
# task 10: pre-trend placebo test

# 1
df_22 |> 
  group_by(treated) |> 
  summarise(
    cities = n_distinct(city),
    dates = n_distinct(date)
    )

# 2


# 3




# 4

# 5

# 7
feols(downloads ~ treated:post + competitor_mkt | city + date, data = df_22)
   

# 9

DRDID::drdid(
  yname = "downloads",
  tname = "post",
  idname = "city",
  dname = "treated",
  xformla = ~region + use_share + age_mean,
  data = df_22
)



generate_data(scenario = "B", P = 10)
df <- map(1:300, ~generate_data(scenario = "B")) %>%
  bind_rows()
df$city <- rep(1:600, each = 10)
df$time_to_treat <- 0
df[df$treat==1, ]$time_to_treat <- df[df$treat==1, ]$period - 6

df <- df |> group_by(city) |> mutate(x1 = mean(x1)) |> ungroup()

summary(feols(sales ~ treat:after | city + period, data = df))
summary(feols(sales ~ treat:after + period:x1 | city + period, data = df))

evt_stdy <- feols(sales ~ i(time_to_treat, treat, ref = -1) | city + period, data = df)
summary(evt_stdy)
iplot(evt_stdy)

evt_stdy <- feols(sales ~ i(time_to_treat, treat, ref = -1) + x1 + period:x1 + treat:x1 | city + period, data = df)
summary(evt_stdy)
iplot(evt_stdy)

DRDID::drdid(
  yname = "sales",
  tname = "after",
  idname = "city",
  dname = "treat",
  xformla = ~x1,
  data = df |> filter(period %in% 5:6)
)


generate_data <- function(
    S = 2, # number of groups
    P = 10, # number of periods
    D_size = 1, # effect of treatment
    D_time = NULL, # time of treatment
    y_0 = 50, # base value for y
    sd = 1, # standard deviations for randomly generated sequences
    scenario = c("A", "B")
){
  # create group period dyads
  s <- rep(0:1, each = P)
  p <- rep(1:P, S)
  
  # timing and size of treatment (effect)
  delta <- D_size
  if (missing(D_time)) D_time <- P/2
  after <- as.numeric(ifelse(p > D_time, 1, 0))
  
  # create relation between independent variables and treatment (actually
  # other way round, but easier to simulate this way)
  x1 <- rnorm(S*P, s, sd)
  
  # create dependent variable ...
  # ... for scenario (A)
  y_a <- y_0 + delta*s*after + 1/5*p + x1 + rnorm(S*P, 0, sd)
  
  # ... for scenario (B)
  y_b <- y_0 + delta*s*after + 1/5*p + x1 + s*x1 + 1/3*p*x1 + rnorm(S*P, 0, sd)
  
  # add variables to table
  df <- tibble(
    treat   = s,
    period  = p,
    after   = after,
    x1      = x1,
    sales   = if (scenario == "A") {y_a} else {y_b}
  )
  
  # return table
  return(df)
}
