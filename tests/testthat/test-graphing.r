library(dplyr)
library(purrr)
library(stringr)
library(xpt)
library(lubridate)
library(ggplot2)
library(scales)

test_rs_quintile <- function() {
  # Set seed for reproducibility
  set.seed(123)

  # Define user types and quintiles
  user_types <- c("Superforecasters", "General X-risk Experts", "Other Experts")
  quintiles <- paste0("Q", 1:5)

  # Generate data for each user type
  df_list <- map(user_types, function(user_type) {
    data.frame(
      userType = user_type,
      forecast = round(rnorm(20, mean = 5, sd = 2), 1),
      quintile = sample(quintiles, 20, replace = TRUE)
    )
  })

  # Combine data frames into one
  df <- bind_rows(df_list)

  # Shuffle rows
  df <- df[sample(nrow(df)), ]

  # Add row numbers as an ID column
  df$id <- 1:nrow(df)

  df <- df %>%
      mutate(
          userType = factor(userType, levels = user_types),
          quintile = factor(quintile, levels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
      ) %>%
      select(userType, forecast, quintile)

  rs_quintile_plot(df, title = "fake data", subtitle = "RS accuracy")
}

test_boot <- function() {
  dd <- read.csv("/home/molly/fri/sample_date_data.csv")
  aa <- boot_results(dd)
  return(aa)
}

test_percentogram <- function() {
  data <- read.csv("/home/molly/fri/xpt-basic-analysis/sources/FORECASTS.csv") %>%
    mutate(questionName = tolower(questionName)) %>%
    filter(grepl("AI Catastrophic Risk", setName),
           questionName == paste0(2100, " (your beliefs)")) %>%
    mutate(timestamp = ymd_hms(timestamp)) %>%
    group_by(userId) %>%
    slice_max(timestamp) %>%
    ungroup() %>%
    select(userId, forecast, timestamp, setName) %>%
    mutate(forecast = as.numeric(forecast))

  ggplot(data, aes(forecast)) +
    geom_histogram(stat = StatQuantileBin, aes(fill = after_stat(quantile)))
}

#test_boot()
test_rs_quintile()
