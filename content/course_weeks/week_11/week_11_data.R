generate_data <- function(city='Chicago', year=2010, seed=1) {
  set.seed(seed)
  
  # Load Data
  df <- read.csv('content/course_weeks/week_11/us_cities_20022019_clean.csv')
  df <- df[df$year > 2002, ]
  
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
  
  return(df)
}


# Example usage
df <- generate_data(city='Chicago', year=2010, seed=1)
# head(df)

ride <- tibble(df) |> 
  select(-year, -month, -summer, -pre_summer, -post_summer)

all_cities <- unique(ride$city)
ctrl_sample <- sample(all_cities[all_cities!="Chicago"], 12)

ride <- ride |> filter(city %in% c(ctrl_sample, "Chicago"))

saveRDS(ride, "content/course_weeks/week_11/ride.rds")

