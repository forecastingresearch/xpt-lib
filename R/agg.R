library(dplyr)
library(docstring)

# Create an aggregation class that these all inherit from
preprocess <- function(x, q = 0, drop_zeroes = FALSE) {
  #' Preprocessing function for agg methods
  #'
  #' @description This is sort of a base class or a template for the individual
  #' aggregation functions. It does the things that they all have in common.
  #'
  #' @param specific_function The specific aggregation function to be generated
  #' @param x A vector of forecasts
  #' @param q The quantile to use for replacing 0s and 1s (between 0 and 1)
  #' @param drop_zeroes Whether to drop zeroes from the vector
  #'
  #' @note ASSUMES FORECASTS ARE IN THE RANGE [0, 100]!

  x <- sort(x)

  if (q != 0) {
    x[x == 100] <- as.numeric(quantile(x[x != 100], 1 - q))
    x[x == 0] <- as.numeric(quantile(x[x != 0], q))
  }

  if (drop_zeroes) {
    x <- x[x != 0]
  }

  return(x)
}

trim <- function(x, p = 0.1, drop_zeroes = FALSE) {
  #' Trimmed mean
  #'
  #' Trim the top and bottom (p*100)% of forecasts
  #'
  #' @param questionData The processed question data table (needs to have a
  #' forecast column)
  #' @param p The proportion of forecasts to trim from each end (between 0 and
  #' 1)
  #' @param drop_zeroes Whether to drop zeroes from the vector
  #' @return The trimmed mean
  #'
  #' @export

  x <- preprocess(x, q = 0, drop_zeroes)
  trimN <- round(p * length(x))
  lastRow <- length(x) - trimN
  trimVec <- x[(trimN + 1):lastRow]
  trimmedMean <- mean(trimVec)
  return(trimmedMean)
}

hd_trim <- function(x, p = 0.1, drop_zeroes = FALSE) {
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
  #' @param drop_zeroes Whether to drop zeroes from the vector
  #'
  #' @export

  x <- preprocess(x, q = 0, drop_zeroes)
  n_out <- floor(length(x) * p)
  n_in <- length(x) - n_out
  d <- c()
  for (i in 1:(n_out + 1)) {
    d[i] <- x[i + n_in - 1] - x[i]
  }
  i <- which.min(d)
  mean(x[i:(i + n_in - 1)])
}

soften_mean <- function(x, p = 0.1, drop_zeroes = FALSE) {
  #' Soften the mean.
  #'
  #' If the mean is > .5, trim the top trim%; if < .5, the bottom trim%. Return
  #' the new mean (i.e. soften the mean).
  #'
  #' @param x A vector of forecasts
  #' @param trim The proportion of forecasts to trim from each end (between 0
  #' and 1)
  #' @param drop_zeroes Whether to drop zeroes from the vector
  #'
  #' @note This goes against usual wisdom of extremizing the mean, but performs
  #' well when the crowd has some overconfident forecasters in it.
  #'
  #' @export

  x <- preprocess(x, q = 0, drop_zeroes)
  mymean <- mean(x)
  if (mymean > .5) {
    return(mean(x[1:floor(length(x) * (1 - p))]))
  } else {
    return(mean(x[ceiling(length(x) * p):length(x)]))
  }
}

neymanAggCalc <- function(x, drop_zeroes = FALSE) {
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
  #' @param drop_zeroes Whether to drop zeroes from the vector
  #' @references Neyman, E. and Roughgarden, T. (2021). Are you
  #' smarter than a random expert? The robust aggregation of substitutable
  #' signals. `https://arxiv.org/abs/2111.03153`
  #' @note Expects forecasts to be in the range [0, 100]!
  #'
  #' @export

  x <- preprocess(x, q = 0, drop_zeroes)
  x <- (x / 100)
  n <- length(x)
  d <- (n * (sqrt((3 * n^2) - (3 * n) + 1) - 2)) / (n^2 - n - 1)
  t <- (x^d) / ((x^d) + (1 - x)^d)
  return(mean(t) * 100)
}

geoMeanCalc <- function(x, q = 0.05, drop_zeroes = FALSE) {
  #' Geometric Mean
  #'
  #' Calculate the geometric mean of a vector of forecasts. We handle 0s by
  #' replacing them with the qth quantile of the non-zero forecasts.
  #'
  #' @param x A vector of forecasts
  #' @param q The quantile to use for replacing 0s (between 0 and 1)
  #' @param drop_zeroes Whether to drop zeroes from the vector
  #' @note agg(a) + agg(not a) does not sum to 1 for this aggregation method.
  #'
  #' @export

  x <- preprocess(x, q, drop_zeroes)
  geoMean <- exp(mean(log(x)))
  return(geoMean)
}

geoMeanOfOddsCalc <- function(x, q = 0.05, drop_zeroes = FALSE) {
  #' Geometric Mean of Odds
  #'
  #' Convert probabilities to odds, and calculate the geometric mean of the
  #' odds. We handle 0s by replacing them with the qth quantile of the non-zero
  #' forecasts, before converting.
  #'
  #' @param x A vector of forecasts (probabilities!)
  #' @param q The quantile to use for replacing 0s (between 0 and 1)
  #' @param drop_zeroes Whether to drop zeroes from the vector
  #' @note agg(a) + agg(not a) does not sum to 1 for this aggregation method.
  #'
  #' @export

  x <- preprocess(x, q, drop_zeroes)
  x <- x / 100
  odds <- x / (1 - x)
  geoMeanOfOdds <- exp(mean(log(odds)))

  # Convert back to probability
  geoMeanOfOdds <- geoMeanOfOdds / (geoMeanOfOdds + 1)
  return(geoMeanOfOdds * 100)
}
