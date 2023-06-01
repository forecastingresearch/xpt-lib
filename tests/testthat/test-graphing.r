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

test_bind_with_grp <- function() {
  # Make a dataframe with three columns A, B, C, random numbers
  d1 <- data.frame(A = rnorm(3), B = rnorm(3), C = rnorm(3))
  d1 <- d1 %>%
    summarize(n_ids = n(),
              mean = mean(A),
              median = median(A))
  # Make a dataframe that's same columns as d1 but different values
  d2 <- data.frame(A = rnorm(2), B = rnorm(2), C = rnorm(2))
  d2 <- d2 %>%
    summarize(n_ids = n(),
              mean = mean(A),
              median = median(A))
  dempty <- data.frame(
    n_ids = numeric(0),
    mean = numeric(0),
    median = numeric(0)
  )
  # Bind the two dataframes together
  d3 <- gdata::combine(dempty, d1, d2)
}

test_mutate_csv <- function() {
  csv <- read.csv("/home/molly/fri/xpt-basic-analysis/Summary Data/11. Year of Extinction/Figure Data/5th %/11. Year of Extinction - 5th %.csv")
  plotTable <- mutate_figure_data_median(csv)
  plotTable <- plotTable %>%
    rename(confint_lower = contains("confint_lower"), confint_upper = contains("confint_upper"))

  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, color = group, fill = group)) +
    geom_line() +
    ylab("Median") +
    xlab("Date") +
    labs(title = "holla", subtitle = "atcha girl") +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = unlist(group_colors)) +
    scale_fill_manual(values = unlist(group_colors)) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") #+
    #xlim(phaseTwoMedian, NA)
  plot$labels$color <- ""
}

#test_boot()
test_rs_quintile()
