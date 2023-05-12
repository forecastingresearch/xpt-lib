ibrary(dplyr)
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
  user_types <- c("Supers", "General X-risk Experts", "Other Experts")
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

test_plot_ci_ribbon <- function() {
  plotTable <- read.csv("/home/molly/fri/plotTable_sample.csv")
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, color = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2, color = "transparent") + ylab("Median") +
    xlab("Date") +
    labs(title = paste("test", "by", 2345)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = cb_pal) +
    scale_fill_manual(values = cb_pal)
  plot$labels$color <- ""
  plot
}

test_plot_ci_ribbon()
#test_boot()
#test_rs_quintile()
