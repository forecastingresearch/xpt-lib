library(dplyr)
library(docstring)

trim <- function(x, p = 0.1) {
  #' Trimmed mean
  #'
  #' Trim the top and bottom (p*100)% of forecasts
  #'
  #' @param questionData The processed question data table (needs to have a forecast column)
  #' @param p The proportion of forecasts to trim from each end (between 0 and 1)
  #' @return The trimmed mean

  x <- sort(x)
  trimN <- round(p * length(x))
  lastRow <- length(x) - trimN
  trimVec <- x[(trimN + 1):lastRow]
  trimmedMean <- mean(trimVec)
  return(trimmedMean)
}

hd_trim <- function(x, p = 0.5) {
  #' High-Density Trimming/Winsorizing
  #'
  #' @description This code comes from an email from Ben Powell.
  #'
  #' You find the shortest interval containing (1-p) * 100% of the data and take the
  #' mean of the forecasts within that interval.
  #'
  #' @note As p gets bigger this acts like a mode in a similar way to
  #' the symmetrically-trimmed mean acting like a median.
  #'
  #' @param x A vector of forecasts
  #' @param p The proportion of forecasts to trim (between 0 and 1)

  x <- sort(x)
  n_out <- floor(length(x) * p)
  n_in <- length(x) - n_out
  d <- c()
  for (i in 1:(n_out + 1)) {
    d[i] <- x[i + n_in - 1] - x[i]
  }
  i <- which.min(d)
  mean(x[i:(i + n_in - 1)])
}

neymanAggCalc <- function(x) {
  #' Neyman Aggregation (Extremized)
  #'
  #' @description Origin: Neyman and Roughgarden 2021
  #' See also Jaime Savilla's post on the EA Forum about it.
  #'
  #' Extremizes the aggregated forecast by a factor of:
  #' (n*(sqrt((3*n^2) - (3*n) + 1) - 2))/(n^2 - n - 1)
  #' where n is the number of forecasts.
  #'
  #' @param x A vector of forecasts
  #' @references Neyman, E. and Roughgarden, T. (2021). Are you
  #' smarter than a random expert? The robust aggregation of substitutable
  #' signals. `https://arxiv.org/abs/2111.03153`

  x <- (x / 100)
  n <- length(x)
  d <- (n * (sqrt((3 * n^2) - (3 * n) + 1) - 2)) / (n^2 - n - 1)
  t <- (x^d) / ((x^d) + (1 - x)^d)
  return(mean(t) * 100)
}

geoMeanCalc <- function(x) {
  #' Geometric Mean
  #'
  #' Calculate the geometric mean of a vector of forecasts.
  #' @param x A vector of forecasts

  x[x == 0] <- as.numeric(quantile(x[x != 0], 0.05))
  geoMean <- exp(mean(log(x)))
  return(geoMean)
}
