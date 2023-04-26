library(dplyr)
library(docstring)

newAddInit <- function() {
  #' Initialize empty dataframe to hold the summary stats.
  #'
  #' @return Dataframe (wide) with one row per (question, stage) pair, gives aggregates
  #' with columns like:
  #' specialty - nuclear, biorisk, etc
  #' pct* - everyone
  #' g1* - group 1 (highest-paid experts & ALL of the supers)
  #' g2* - group 2 (lower tier, experts only)
  #' domainExperts - ONLY the g1 experts from the domain of the question
  #' nonDomainExperts - experts from not the domain of the question, and supers
  #' *_exc - filter out extreme outliers (based on standard deviation; different from trimmed mean)

  return(data.frame(
    setName = character(0),
    questionName = character(0),
    answerText = character(0),
    stage = numeric(0),
    specialty = character(0),
    mean = numeric(0),
    median = numeric(0),
    sd = numeric(0),
    n = numeric(0),
    trimmedMean = numeric(0),
    pct5th = numeric(0),
    pct25th = numeric(0),
    pct75th = numeric(0),
    pct95th = numeric(0),
    geoMean = numeric(0),
    hdTrim = numeric(0),
    neymanAgg = numeric(0),
    g1Mean = numeric(0),
    g1Median = numeric(0),
    g1Sd = numeric(0),
    g1N = numeric(0),
    g1TrimmedMean = numeric(0),
    g1Pct5th = numeric(0),
    g1Pct25th = numeric(0),
    g1Pct75th = numeric(0),
    g1Pct95th = numeric(0),
    g1GeoMean = numeric(0),
    g1HdTrim = numeric(0),
    g1NeymanAgg = numeric(0),
    supersMean = numeric(0),
    supersMedian = numeric(0),
    supersSd = numeric(0),
    supersN = numeric(0),
    supersTrimmedMean = numeric(0),
    supersPct5th = numeric(0),
    supersPct25th = numeric(0),
    supersPct75th = numeric(0),
    supersPct95th = numeric(0),
    supersGeoMean = numeric(0),
    supersHdTrim = numeric(0),
    supersNeymanAgg = numeric(0),
    expertsG1Mean = numeric(0),
    expertsG1Median = numeric(0),
    expertsG1Sd = numeric(0),
    expertsG1N = numeric(0),
    expertsG1TrimmedMean = numeric(0),
    expertsG1Pct5th = numeric(0),
    expertsG1Pct25th = numeric(0),
    expertsG1Pct75th = numeric(0),
    expertsG1Pct95th = numeric(0),
    expertsG1GeoMean = numeric(0),
    expertsG1HdTrim = numeric(0),
    expertsG1NeymanAgg = numeric(0),
    expertsG2Mean = numeric(0),
    expertsG2Median = numeric(0),
    expertsG2Sd = numeric(0),
    expertsG2N = numeric(0),
    expertsG2TrimmedMean = numeric(0),
    expertsG2Pct5th = numeric(0),
    expertsG2Pct25th = numeric(0),
    expertsG2Pct75th = numeric(0),
    expertsG2Pct95th = numeric(0),
    expertsG2GeoMean = numeric(0),
    expertsG2HdTrim = numeric(0),
    expertsG2NeymanAgg = numeric(0),
    domainExpertsMean = numeric(0),
    domainExpertsMedian = numeric(0),
    domainExpertsSd = numeric(0),
    domainExpertsN = numeric(0),
    domainExpertsTrimmedMean = numeric(0),
    domainExpertsPct5th = numeric(0),
    domainExpertsPct25th = numeric(0),
    domainExpertsPct75th = numeric(0),
    domainExpertsPct95th = numeric(0),
    domainExpertsGeoMean = numeric(0),
    domainExpertsHdTrim = numeric(0),
    domainExpertsNeymanAgg = numeric(0),
    nonDomainExpertsMean = numeric(0),
    nonDomainExpertsMedian = numeric(0),
    nonDomainExpertsSd = numeric(0),
    nonDomainExpertsN = numeric(0),
    nonDomainExpertsTrimmedMean = numeric(0),
    nonDomainExpertsPct5th = numeric(0),
    nonDomainExpertsPct25th = numeric(0),
    nonDomainExpertsPct75th = numeric(0),
    nonDomainExpertsPct95th = numeric(0),
    nonDomainExpertsGeoMean = numeric(0),
    nonDomainExpertsHdTrim = numeric(0),
    nonDomainExpertsNeymanAgg = numeric(0),
    mean_exc = numeric(0),
    median_exc = numeric(0),
    sd_exc = numeric(0),
    n_exc = numeric(0),
    trimmedMean_exc = numeric(0),
    pct5th_exc = numeric(0),
    pct25th_exc = numeric(0),
    pct75th_exc = numeric(0),
    pct95th_exc = numeric(0),
    geoMean_exc = numeric(0),
    hdTrim_exc = numeric(0),
    neymanAgg_exc = numeric(0),
    g1Mean_exc = numeric(0),
    g1Median_exc = numeric(0),
    g1Sd_exc = numeric(0),
    g1N_exc = numeric(0),
    g1TrimmedMean_exc = numeric(0),
    g1Pct5th_exc = numeric(0),
    g1Pct25th_exc = numeric(0),
    g1Pct75th_exc = numeric(0),
    g1Pct95th_exc = numeric(0),
    g1GeoMean_exc = numeric(0),
    g1HdTrim_exc = numeric(0),
    g1NeymanAgg_exc = numeric(0),
    supersMean_exc = numeric(0),
    supersMedian_exc = numeric(0),
    supersSd_exc = numeric(0),
    supersN_exc = numeric(0),
    supersTrimmedMean_exc = numeric(0),
    supersPct5th_exc = numeric(0),
    supersPct25th_exc = numeric(0),
    supersPct75th_exc = numeric(0),
    supersPct95th_exc = numeric(0),
    supersGeoMean_exc = numeric(0),
    supersHdTrim_exc = numeric(0),
    supersNeymanAgg_exc = numeric(0),
    expertsG1Mean_exc = numeric(0),
    expertsG1Median_exc = numeric(0),
    expertsG1Sd_exc = numeric(0),
    expertsG1N_exc = numeric(0),
    expertsG1TrimmedMean_exc = numeric(0),
    expertsG1Pct5th_exc = numeric(0),
    expertsG1Pct25th_exc = numeric(0),
    expertsG1Pct75th_exc = numeric(0),
    expertsG1Pct95th_exc = numeric(0),
    expertsG1GeoMean_exc = numeric(0),
    expertsG1HdTrim_exc = numeric(0),
    expertsG1NeymanAgg_exc = numeric(0),
    expertsG2Mean_exc = numeric(0),
    expertsG2Median_exc = numeric(0),
    expertsG2Sd_exc = numeric(0),
    expertsG2N_exc = numeric(0),
    expertsG2TrimmedMean_exc = numeric(0),
    expertsG2Pct5th_exc = numeric(0),
    expertsG2Pct25th_exc = numeric(0),
    expertsG2Pct75th_exc = numeric(0),
    expertsG2Pct95th_exc = numeric(0),
    expertsG2GeoMean_exc = numeric(0),
    expertsG2HdTrim_exc = numeric(0),
    expertsG2NeymanAgg_exc = numeric(0),
    domainExpertsMean_exc = numeric(0),
    domainExpertsMedian_exc = numeric(0),
    domainExpertsSd_exc = numeric(0),
    domainExpertsN_exc = numeric(0),
    domainExpertsTrimmedMean_exc = numeric(0),
    domainExpertsPct5th_exc = numeric(0),
    domainExpertsPct25th_exc = numeric(0),
    domainExpertsPct75th_exc = numeric(0),
    domainExpertsPct95th_exc = numeric(0),
    domainExpertsGeoMean_exc = numeric(0),
    domainExpertsHdTrim_exc = numeric(0),
    domainExpertsNeymanAgg_exc = numeric(0),
    nonDomainExpertsMean_exc = numeric(0),
    nonDomainExpertsMedian_exc = numeric(0),
    nonDomainExpertsSd_exc = numeric(0),
    nonDomainExpertsN_exc = numeric(0),
    nonDomainExpertsTrimmedMean_exc = numeric(0),
    nonDomainExpertsPct5th_exc = numeric(0),
    nonDomainExpertsPct25th_exc = numeric(0),
    nonDomainExpertsPct75th_exc = numeric(0),
    nonDomainExpertsPct95th_exc = numeric(0),
    nonDomainExpertsGeoMean_exc = numeric(0),
    nonDomainExpertsHdTrim_exc = numeric(0),
    nonDomainExpertsNeymanAgg_exc = numeric(0)
  ))
}

newRowInit <- function(metaTable, questionDataProcessed, currentSetName,
                       currentQuestionName, answerText, stage, specialty) {
  #' TODO Molly :)
  mean <- mean(questionDataProcessed$forecast)
  median <- median(questionDataProcessed$forecast)
  sd <- sd(questionDataProcessed$forecast)
  n <- nrow(questionDataProcessed)
  trimmedMean <- trim(questionDataProcessed$forecast)
  pct5th <- as.numeric(quantile(questionDataProcessed$forecast, 0.05))
  pct25th <- as.numeric(quantile(questionDataProcessed$forecast, 0.25))
  pct75th <- as.numeric(quantile(questionDataProcessed$forecast, 0.75))
  pct95th <- as.numeric(quantile(questionDataProcessed$forecast, 0.95))
  geoMean <- geoMeanCalc(questionDataProcessed$forecast)
  hdTrim <- hd_trim(questionDataProcessed$forecast)
  neymanAgg <- neymanAggCalc(questionDataProcessed$forecast)

  g1Processed <- questionDataProcessed %>% filter(userName %in% c(supers, expertsG1$userName))
  g1Mean <- mean(g1Processed$forecast)
  g1Median <- median(g1Processed$forecast)
  g1Sd <- sd(g1Processed$forecast)
  g1N <- nrow(g1Processed)
  g1TrimmedMean <- trim(g1Processed$forecast)
  g1Pct5th <- as.numeric(quantile(g1Processed$forecast, 0.05))
  g1Pct25th <- as.numeric(quantile(g1Processed$forecast, 0.25))
  g1Pct75th <- as.numeric(quantile(g1Processed$forecast, 0.75))
  g1Pct95th <- as.numeric(quantile(g1Processed$forecast, 0.95))
  g1GeoMean <- geoMeanCalc(g1Processed$forecast)
  g1HdTrim <- hd_trim(g1Processed$forecast)
  g1NeymanAgg <- neymanAggCalc(g1Processed$forecast)

  supersProcessed <- questionDataProcessed %>% filter(userName %in% supers)
  supersMean <- mean(supersProcessed$forecast)
  supersMedian <- median(supersProcessed$forecast)
  supersSd <- sd(supersProcessed$forecast)
  supersN <- nrow(supersProcessed)
  supersTrimmedMean <- trim(supersProcessed$forecast)
  supersPct5th <- as.numeric(quantile(supersProcessed$forecast, 0.05))
  supersPct25th <- as.numeric(quantile(supersProcessed$forecast, 0.25))
  supersPct75th <- as.numeric(quantile(supersProcessed$forecast, 0.75))
  supersPct95th <- as.numeric(quantile(supersProcessed$forecast, 0.95))
  supersGeoMean <- geoMeanCalc(supersProcessed$forecast)
  supersHdTrim <- hd_trim(supersProcessed$forecast)
  supersNeymanAgg <- neymanAggCalc(supersProcessed$forecast)

  expertsG1Processed <- questionDataProcessed %>% filter(userName %in% expertsG1$userName)
  expertsG1Mean <- mean(expertsG1Processed$forecast)
  expertsG1Median <- median(expertsG1Processed$forecast)
  expertsG1Sd <- sd(expertsG1Processed$forecast)
  expertsG1N <- nrow(expertsG1Processed)
  expertsG1TrimmedMean <- trim(expertsG1Processed$forecast)
  expertsG1Pct5th <- as.numeric(quantile(expertsG1Processed$forecast, 0.05))
  expertsG1Pct25th <- as.numeric(quantile(expertsG1Processed$forecast, 0.25))
  expertsG1Pct75th <- as.numeric(quantile(expertsG1Processed$forecast, 0.75))
  expertsG1Pct95th <- as.numeric(quantile(expertsG1Processed$forecast, 0.95))
  expertsG1GeoMean <- geoMeanCalc(expertsG1Processed$forecast)
  expertsG1HdTrim <- hd_trim(expertsG1Processed$forecast)
  expertsG1NeymanAgg <- neymanAggCalc(expertsG1Processed$forecast)

  expertsG2Processed <- questionDataProcessed %>% filter(userName %in% expertsG2)
  expertsG2Mean <- mean(expertsG2Processed$forecast)
  expertsG2Median <- median(expertsG2Processed$forecast)
  expertsG2Sd <- sd(expertsG2Processed$forecast)
  expertsG2N <- nrow(expertsG2Processed)
  expertsG2TrimmedMean <- trim(expertsG2Processed$forecast)
  expertsG2Pct5th <- as.numeric(quantile(expertsG2Processed$forecast, 0.05))
  expertsG2Pct25th <- as.numeric(quantile(expertsG2Processed$forecast, 0.25))
  expertsG2Pct75th <- as.numeric(quantile(expertsG2Processed$forecast, 0.75))
  expertsG2Pct95th <- as.numeric(quantile(expertsG2Processed$forecast, 0.95))
  expertsG2GeoMean <- geoMeanCalc(expertsG2Processed$forecast)
  expertsG2HdTrim <- hd_trim(expertsG2Processed$forecast)
  expertsG2NeymanAgg <- neymanAggCalc(expertsG2Processed$forecast)

  if (specialty != "") {
    currentSpecialty <- specialty
    specialists <- (expertsG1 %>% filter(specialty1 == specialty | specialty2 == specialty | specialty3 == specialty))$userName
    domainExpertsProcessed <- questionDataProcessed %>% filter(userName %in% specialists)
    domainExpertsMean <- mean(domainExpertsProcessed$forecast)
    domainExpertsMedian <- median(domainExpertsProcessed$forecast)
    domainExpertsSd <- sd(domainExpertsProcessed$forecast)
    domainExpertsN <- nrow(domainExpertsProcessed)
    domainExpertsTrimmedMean <- trim(domainExpertsProcessed$forecast)
    domainExpertsPct5th <- as.numeric(quantile(domainExpertsProcessed$forecast, 0.05))
    domainExpertsPct25th <- as.numeric(quantile(domainExpertsProcessed$forecast, 0.25))
    domainExpertsPct75th <- as.numeric(quantile(domainExpertsProcessed$forecast, 0.75))
    domainExpertsPct95th <- as.numeric(quantile(domainExpertsProcessed$forecast, 0.95))
    domainExpertsGeoMean <- geoMeanCalc(domainExpertsProcessed$forecast)
    if (nrow(domainExpertsProcessed) > 0) {
      domainExpertsHdTrim <- hd_trim(domainExpertsProcessed$forecast)
    } else {
      domainExpertsHdTrim <- NA
    }
    domainExpertsNeymanAgg <- neymanAggCalc(domainExpertsProcessed$forecast)
  } else {
    domainExpertsMean <- NA
    domainExpertsMedian <- NA
    domainExpertsSd <- NA
    domainExpertsN <- NA
    domainExpertsTrimmedMean <- NA
    domainExpertsPct5th <- NA
    domainExpertsPct25th <- NA
    domainExpertsPct75th <- NA
    domainExpertsPct95th <- NA
    domainExpertsGeoMean <- NA
    domainExpertsHdTrim <- NA
    domainExpertsNeymanAgg <- NA
  }

  if (specialty != "") {
    nonDomainExpertsProcessed <- questionDataProcessed %>%
      filter(!(userName %in% specialists)) %>%
      filter(userName %in% expertsG1$userName)
    nonDomainExpertsMean <- mean(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsMedian <- median(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsSd <- sd(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsN <- nrow(nonDomainExpertsProcessed)
    nonDomainExpertsTrimmedMean <- trim(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsPct5th <- as.numeric(quantile(nonDomainExpertsProcessed$forecast, 0.05))
    nonDomainExpertsPct25th <- as.numeric(quantile(nonDomainExpertsProcessed$forecast, 0.25))
    nonDomainExpertsPct75th <- as.numeric(quantile(nonDomainExpertsProcessed$forecast, 0.75))
    nonDomainExpertsPct95th <- as.numeric(quantile(nonDomainExpertsProcessed$forecast, 0.95))
    nonDomainExpertsGeoMean <- geoMeanCalc(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsHdTrim <- hd_trim(nonDomainExpertsProcessed$forecast)
    nonDomainExpertsNeymanAgg <- neymanAggCalc(nonDomainExpertsProcessed$forecast)
  } else {
    nonDomainExpertsMean <- NA
    nonDomainExpertsMedian <- NA
    nonDomainExpertsSd <- NA
    nonDomainExpertsN <- NA
    nonDomainExpertsTrimmedMean <- NA
    nonDomainExpertsPct5th <- NA
    nonDomainExpertsPct25th <- NA
    nonDomainExpertsPct75th <- NA
    nonDomainExpertsPct95th <- NA
    nonDomainExpertsGeoMean <- NA
    nonDomainExpertsHdTrim <- NA
    nonDomainExpertsNeymanAgg <- NA
  }

  # Filter out extreme outliers
  questionDataProcessed_exc <- questionDataProcessed %>%
    filter(forecast > median - (10 * sd)) %>%
    filter(forecast < median + (10 * sd))
  mean_exc <- mean(questionDataProcessed_exc$forecast)
  median_exc <- median(questionDataProcessed_exc$forecast)
  sd_exc <- sd(questionDataProcessed_exc$forecast)
  n_exc <- nrow(questionDataProcessed_exc)
  trimmedMean_exc <- trim(questionDataProcessed_exc$forecast)
  pct5th_exc <- as.numeric(quantile(questionDataProcessed_exc$forecast, 0.05))
  pct25th_exc <- as.numeric(quantile(questionDataProcessed_exc$forecast, 0.25))
  pct75th_exc <- as.numeric(quantile(questionDataProcessed_exc$forecast, 0.75))
  pct95th_exc <- as.numeric(quantile(questionDataProcessed_exc$forecast, 0.95))
  geoMean_exc <- geoMeanCalc(questionDataProcessed_exc$forecast)
  hdTrim_exc <- hd_trim(questionDataProcessed_exc$forecast)
  neymanAgg_exc <- neymanAggCalc(questionDataProcessed_exc$forecast)

  g1Processed_exc <- questionDataProcessed %>%
    filter(userName %in% c(supers, expertsG1$userName)) %>%
    filter(forecast > g1Median - (10 * g1Sd)) %>%
    filter(forecast < g1Median + (10 * g1Sd))
  g1Mean_exc <- mean(g1Processed_exc$forecast)
  g1Median_exc <- median(g1Processed_exc$forecast)
  g1Sd_exc <- sd(g1Processed_exc$forecast)
  g1N_exc <- nrow(g1Processed_exc)
  g1TrimmedMean_exc <- trim(g1Processed_exc$forecast)
  g1Pct5th_exc <- as.numeric(quantile(g1Processed_exc$forecast, 0.05))
  g1Pct25th_exc <- as.numeric(quantile(g1Processed_exc$forecast, 0.25))
  g1Pct75th_exc <- as.numeric(quantile(g1Processed_exc$forecast, 0.75))
  g1Pct95th_exc <- as.numeric(quantile(g1Processed_exc$forecast, 0.95))
  g1GeoMean_exc <- geoMeanCalc(g1Processed_exc$forecast)
  g1HdTrim_exc <- hd_trim(g1Processed_exc$forecast)
  g1NeymanAgg_exc <- neymanAggCalc(g1Processed_exc$forecast)

  supersProcessed_exc <- questionDataProcessed %>%
    filter(userName %in% supers) %>%
    filter(forecast > supersMedian - (10 * supersSd)) %>%
    filter(forecast < supersMedian + (10 * supersSd))
  supersMean_exc <- mean(supersProcessed_exc$forecast)
  supersMedian_exc <- median(supersProcessed_exc$forecast)
  supersSd_exc <- sd(supersProcessed_exc$forecast)
  supersN_exc <- nrow(supersProcessed_exc)
  supersTrimmedMean_exc <- trim(supersProcessed_exc$forecast)
  supersPct5th_exc <- as.numeric(quantile(supersProcessed_exc$forecast, 0.05))
  supersPct25th_exc <- as.numeric(quantile(supersProcessed_exc$forecast, 0.25))
  supersPct75th_exc <- as.numeric(quantile(supersProcessed_exc$forecast, 0.75))
  supersPct95th_exc <- as.numeric(quantile(supersProcessed_exc$forecast, 0.95))
  supersGeoMean_exc <- geoMeanCalc(supersProcessed_exc$forecast)
  supersHdTrim_exc <- hd_trim(supersProcessed_exc$forecast)
  supersNeymanAgg_exc <- neymanAggCalc(supersProcessed_exc$forecast)

  expertsG1Processed_exc <- questionDataProcessed %>%
    filter(userName %in% expertsG1$userName) %>%
    filter(forecast > expertsG1Median - (10 * expertsG1Sd)) %>%
    filter(forecast < expertsG1Median + (10 * expertsG1Sd))
  expertsG1Mean_exc <- mean(expertsG1Processed_exc$forecast)
  expertsG1Median_exc <- median(expertsG1Processed_exc$forecast)
  expertsG1Sd_exc <- sd(expertsG1Processed_exc$forecast)
  expertsG1N_exc <- nrow(expertsG1Processed_exc)
  expertsG1TrimmedMean_exc <- trim(expertsG1Processed_exc$forecast)
  expertsG1Pct5th_exc <- as.numeric(quantile(expertsG1Processed_exc$forecast, 0.05))
  expertsG1Pct25th_exc <- as.numeric(quantile(expertsG1Processed_exc$forecast, 0.25))
  expertsG1Pct75th_exc <- as.numeric(quantile(expertsG1Processed_exc$forecast, 0.75))
  expertsG1Pct95th_exc <- as.numeric(quantile(expertsG1Processed_exc$forecast, 0.95))
  expertsG1GeoMean_exc <- geoMeanCalc(expertsG1Processed_exc$forecast)
  if (length(expertsG1Processed_exc$forecast) > 0) {
    expertsG1HdTrim_exc <- hd_trim(expertsG1Processed_exc$forecast)
  } else {
    expertsG1HdTrim_exc <- NA
  }
  expertsG1NeymanAgg_exc <- neymanAggCalc(expertsG1Processed_exc$forecast)

  expertsG2Processed_exc <- questionDataProcessed %>%
    filter(userName %in% expertsG2) %>%
    filter(forecast > expertsG2Median - (10 * expertsG2Sd)) %>%
    filter(forecast < expertsG2Median + (10 * expertsG2Sd))
  expertsG2Mean_exc <- mean(expertsG2Processed_exc$forecast)
  expertsG2Median_exc <- median(expertsG2Processed_exc$forecast)
  expertsG2Sd_exc <- sd(expertsG2Processed_exc$forecast)
  expertsG2N_exc <- nrow(expertsG2Processed_exc)
  expertsG2TrimmedMean_exc <- trim(expertsG2Processed_exc$forecast)
  expertsG2Pct5th_exc <- as.numeric(quantile(expertsG2Processed_exc$forecast, 0.05))
  expertsG2Pct25th_exc <- as.numeric(quantile(expertsG2Processed_exc$forecast, 0.25))
  expertsG2Pct75th_exc <- as.numeric(quantile(expertsG2Processed_exc$forecast, 0.75))
  expertsG2Pct95th_exc <- as.numeric(quantile(expertsG2Processed_exc$forecast, 0.95))
  expertsG2GeoMean_exc <- geoMeanCalc(expertsG2Processed_exc$forecast)
  expertsG2HdTrim_exc <- hd_trim(expertsG2Processed_exc$forecast)
  expertsG2NeymanAgg_exc <- neymanAggCalc(expertsG2Processed_exc$forecast)

  if (specialty != "") {
    currentSpecialty <- specialty
    specialists <- (expertsG1 %>% filter(specialty1 == specialty | specialty2 == specialty | specialty3 == specialty))$userName
    domainExpertsProcessed_exc <- questionDataProcessed %>%
      filter(userName %in% specialists) %>%
      filter(forecast > domainExpertsMedian - (10 * domainExpertsSd)) %>%
      filter(forecast < domainExpertsMedian + (10 * domainExpertsSd))
    domainExpertsMean_exc <- mean(domainExpertsProcessed_exc$forecast)
    domainExpertsMedian_exc <- median(domainExpertsProcessed_exc$forecast)
    domainExpertsSd_exc <- sd(domainExpertsProcessed_exc$forecast)
    domainExpertsN_exc <- nrow(domainExpertsProcessed_exc)
    domainExpertsTrimmedMean_exc <- trim(domainExpertsProcessed_exc$forecast)
    domainExpertsPct5th_exc <- as.numeric(quantile(domainExpertsProcessed_exc$forecast, 0.05))
    domainExpertsPct25th_exc <- as.numeric(quantile(domainExpertsProcessed_exc$forecast, 0.25))
    domainExpertsPct75th_exc <- as.numeric(quantile(domainExpertsProcessed_exc$forecast, 0.75))
    domainExpertsPct95th_exc <- as.numeric(quantile(domainExpertsProcessed_exc$forecast, 0.95))
    domainExpertsGeoMean_exc <- geoMeanCalc(domainExpertsProcessed_exc$forecast)
    if (nrow(domainExpertsProcessed_exc) > 0) {
      domainExpertsHdTrim_exc <- hd_trim(domainExpertsProcessed_exc$forecast)
    } else {
      domainExpertsHdTrim_exc <- NA
    }
    domainExpertsNeymanAgg_exc <- neymanAggCalc(domainExpertsProcessed_exc$forecast)
  } else {
    domainExpertsMean_exc <- NA
    domainExpertsMedian_exc <- NA
    domainExpertsSd_exc <- NA
    domainExpertsN_exc <- NA
    domainExpertsTrimmedMean_exc <- NA
    domainExpertsPct5th_exc <- NA
    domainExpertsPct25th_exc <- NA
    domainExpertsPct75th_exc <- NA
    domainExpertsPct95th_exc <- NA
    domainExpertsGeoMean_exc <- NA
    domainExpertsHdTrim_exc <- NA
    domainExpertsNeymanAgg_exc <- NA
  }

  if (specialty != "") {
    nonDomainExpertsProcessed_exc <- questionDataProcessed %>%
      filter(!(userName %in% specialists)) %>%
      filter(userName %in% expertsG1$userName) %>%
      filter(forecast > nonDomainExpertsMedian - (10 * nonDomainExpertsSd)) %>%
      filter(forecast < nonDomainExpertsMedian + (10 * nonDomainExpertsSd))
    nonDomainExpertsMean_exc <- mean(nonDomainExpertsProcessed_exc$forecast)
    nonDomainExpertsMedian_exc <- median(nonDomainExpertsProcessed_exc$forecast)
    nonDomainExpertsSd_exc <- sd(nonDomainExpertsProcessed_exc$forecast)
    nonDomainExpertsN_exc <- nrow(nonDomainExpertsProcessed_exc)
    nonDomainExpertsTrimmedMean_exc <- trim(nonDomainExpertsProcessed_exc$forecast)
    nonDomainExpertsPct5th_exc <- as.numeric(quantile(nonDomainExpertsProcessed_exc$forecast, 0.05))
    nonDomainExpertsPct25th_exc <- as.numeric(quantile(nonDomainExpertsProcessed_exc$forecast, 0.25))
    nonDomainExpertsPct75th_exc <- as.numeric(quantile(nonDomainExpertsProcessed_exc$forecast, 0.75))
    nonDomainExpertsPct95th_exc <- as.numeric(quantile(nonDomainExpertsProcessed_exc$forecast, 0.95))
    nonDomainExpertsGeoMean_exc <- geoMeanCalc(nonDomainExpertsProcessed_exc$forecast)
    if (nrow(nonDomainExpertsProcessed_exc) > 0) {
      nonDomainExpertsHdTrim_exc <- hd_trim(nonDomainExpertsProcessed_exc$forecast)
    } else {
      nonDomainExpertsHdTrim_exc <- NA
    }
    nonDomainExpertsNeymanAgg_exc <- neymanAggCalc(nonDomainExpertsProcessed_exc$forecast)
  } else {
    nonDomainExpertsMean_exc <- NA
    nonDomainExpertsMedian_exc <- NA
    nonDomainExpertsSd_exc <- NA
    nonDomainExpertsN_exc <- NA
    nonDomainExpertsTrimmedMean_exc <- NA
    nonDomainExpertsPct5th_exc <- NA
    nonDomainExpertsPct25th_exc <- NA
    nonDomainExpertsPct75th_exc <- NA
    nonDomainExpertsPct95th_exc <- NA
    nonDomainExpertsGeoMean_exc <- NA
    nonDomainExpertsHdTrim_exc <- NA
    nonDomainExpertsNeymanAgg_exc <- NA
  }

  newRow <- data.frame(
    currentSetName,
    currentQuestionName,
    answerText,
    stage,
    specialty,
    mean,
    median,
    sd,
    n,
    trimmedMean,
    pct5th,
    pct25th,
    pct75th,
    pct95th,
    geoMean,
    hdTrim,
    neymanAgg,
    g1Mean,
    g1Median,
    g1Sd,
    g1N,
    g1TrimmedMean,
    g1Pct5th,
    g1Pct25th,
    g1Pct75th,
    g1Pct95th,
    g1GeoMean,
    g1HdTrim,
    g1NeymanAgg,
    supersMean,
    supersMedian,
    supersSd,
    supersN,
    supersTrimmedMean,
    supersPct5th,
    supersPct25th,
    supersPct75th,
    supersPct95th,
    supersGeoMean,
    supersHdTrim,
    supersNeymanAgg,
    expertsG1Mean,
    expertsG1Median,
    expertsG1Sd,
    expertsG1N,
    expertsG1TrimmedMean,
    expertsG1Pct5th,
    expertsG1Pct25th,
    expertsG1Pct75th,
    expertsG1Pct95th,
    expertsG1GeoMean,
    expertsG1HdTrim,
    expertsG1NeymanAgg,
    expertsG2Mean,
    expertsG2Median,
    expertsG2Sd,
    expertsG2N,
    expertsG2TrimmedMean,
    expertsG2Pct5th,
    expertsG2Pct25th,
    expertsG2Pct75th,
    expertsG2Pct95th,
    expertsG2GeoMean,
    expertsG2HdTrim,
    expertsG2NeymanAgg,
    domainExpertsMean,
    domainExpertsMedian,
    domainExpertsSd,
    domainExpertsN,
    domainExpertsTrimmedMean,
    domainExpertsPct5th,
    domainExpertsPct25th,
    domainExpertsPct75th,
    domainExpertsPct95th,
    domainExpertsGeoMean,
    domainExpertsHdTrim,
    domainExpertsNeymanAgg,
    nonDomainExpertsMean,
    nonDomainExpertsMedian,
    nonDomainExpertsSd,
    nonDomainExpertsN,
    nonDomainExpertsTrimmedMean,
    nonDomainExpertsPct5th,
    nonDomainExpertsPct25th,
    nonDomainExpertsPct75th,
    nonDomainExpertsPct95th,
    nonDomainExpertsGeoMean,
    nonDomainExpertsHdTrim,
    nonDomainExpertsNeymanAgg,
    mean_exc,
    median_exc,
    sd_exc,
    n_exc,
    trimmedMean_exc,
    pct5th_exc,
    pct25th_exc,
    pct75th_exc,
    pct95th_exc,
    geoMean_exc,
    hdTrim_exc,
    neymanAgg_exc,
    g1Mean_exc,
    g1Median_exc,
    g1Sd_exc,
    g1N_exc,
    g1TrimmedMean_exc,
    g1Pct5th_exc,
    g1Pct25th_exc,
    g1Pct75th_exc,
    g1Pct95th_exc,
    g1GeoMean_exc,
    g1HdTrim_exc,
    g1NeymanAgg_exc,
    supersMean_exc,
    supersMedian_exc,
    supersSd_exc,
    supersN_exc,
    supersTrimmedMean_exc,
    supersPct5th_exc,
    supersPct25th_exc,
    supersPct75th_exc,
    supersPct95th_exc,
    supersGeoMean_exc,
    supersHdTrim_exc,
    supersNeymanAgg_exc,
    expertsG1Mean_exc,
    expertsG1Median_exc,
    expertsG1Sd_exc,
    expertsG1N_exc,
    expertsG1TrimmedMean_exc,
    expertsG1Pct5th_exc,
    expertsG1Pct25th_exc,
    expertsG1Pct75th_exc,
    expertsG1Pct95th_exc,
    expertsG1GeoMean_exc,
    expertsG1HdTrim_exc,
    expertsG1NeymanAgg_exc,
    expertsG2Mean_exc,
    expertsG2Median_exc,
    expertsG2Sd_exc,
    expertsG2N_exc,
    expertsG2TrimmedMean_exc,
    expertsG2Pct5th_exc,
    expertsG2Pct25th_exc,
    expertsG2Pct75th_exc,
    expertsG2Pct95th_exc,
    expertsG2GeoMean_exc,
    expertsG2HdTrim_exc,
    expertsG2NeymanAgg_exc,
    domainExpertsMean_exc,
    domainExpertsMedian_exc,
    domainExpertsSd_exc,
    domainExpertsN_exc,
    domainExpertsTrimmedMean_exc,
    domainExpertsPct5th_exc,
    domainExpertsPct25th_exc,
    domainExpertsPct75th_exc,
    domainExpertsPct95th_exc,
    domainExpertsGeoMean_exc,
    domainExpertsHdTrim_exc,
    domainExpertsNeymanAgg_exc,
    nonDomainExpertsMean_exc,
    nonDomainExpertsMedian_exc,
    nonDomainExpertsSd_exc,
    nonDomainExpertsN_exc,
    nonDomainExpertsTrimmedMean_exc,
    nonDomainExpertsPct5th_exc,
    nonDomainExpertsPct25th_exc,
    nonDomainExpertsPct75th_exc,
    nonDomainExpertsPct95th_exc,
    nonDomainExpertsGeoMean_exc,
    nonDomainExpertsHdTrim_exc,
    nonDomainExpertsNeymanAgg_exc
  )
  return(newRow)
}

multiYearReciprocal <- function(metaTable, data) {
  #' Stats and graphs for the multi-year questions
  #'
  #' @description For the question sets that asked about a forecaster's own
  #' beliefs and what they thought the supers and the experts believed.
  #' This function builds out a folder structure and populates it with stats and graphs for these questions.
  #'
  #' @note All reciprocal questions were multi-year.
  #'
  #' @param metaTable - metadata for all the multi-year reciprocal question sets
  #' @param data - data for all users on all multi-year reciprocal question sets
  #'
  #' @export

  newAdd <- newAddInit()

  # For each of the risk categories (genetically engineered pathogen risk, AI catastrophe, etc.)
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    if (dir.exists(currentSetName)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName))
    } else {
      setwd(paste0(yourHome, "Summary Data"))
      dir.create(currentSetName)
      setwd(currentSetName)
    }

    # PHASE DATA

    if (dir.exists("Phase Data")) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    } else {
      dir.create("Phase Data")
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # Time horizons (2030...)
    years <- metaTable[i, ] %>% select(year1, year2, year3)
    years <- as.character(years)
    years <- years[years != ""]

    # Belief sets: yours, other experts', other superforecasters'
    beliefSets <- metaTable[i, ] %>% select(yourBeliefs, expertBeliefs, superBeliefs)
    beliefSets <- as.character(beliefSets)
    beliefSets <- beliefSets[beliefSets != ""]

    for (j in 1:length(years)) {
      if (!dir.exists(years[j])) {
        dir.create(years[j])
        setwd(years[j])
      } else {
        setwd(years[j])
      }
      for (k in 1:length(beliefSets)) {
        if (!dir.exists(beliefSets[k])) {
          dir.create(beliefSets[k])
        }
      }
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    defaultForecast <- metaTable[i, ]$defaultForecast50

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    # For each (year, belief set, user) combination, calculate the reciprocal score.
    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]
      for (k in 1:length(beliefSets)) {
        print(beliefSets[k])
        for (l in 1:length(years)) {
          print(years[l])
          currentQuestionName <- paste(years[l], beliefSets[k])

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[l], "/", beliefSets[k]))

          questionDataRaw <- data %>%
            filter(setName == currentSetName) %>%
            filter(questionName == currentQuestionName)
          users <- unique(questionDataRaw$userName)
          # users = users[!(users %in% c(supers, expertsG1$userName, expertsG2))]
          users <- users[(users %in% c(supers, expertsG1$userName, expertsG2))]

          questionDataProcessed <- data.frame(row.names = names(supers))

          # TODO: refactor
          if (currentStage == 1) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (!any(is.na(userForecasts$stageTwoTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageTwoTimestamp)
              } else if (!any(is.na(userForecasts$stageThreeTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
              } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 2) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (!any(is.na(userForecasts$stageThreeTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
              } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 3) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 4) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          }

          questionDataProcessed <- questionDataProcessed %>% filter(forecast != defaultForecast)

          questionDataProcessed_anon <- questionDataProcessed %>% select(!userName)

          write.csv(questionDataProcessed, paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, ".csv"), row.names = FALSE)
          write.csv(questionDataProcessed_anon, paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, "_ANON.csv"), row.names = FALSE)

          if (dir.exists("Histograms")) {
            setwd("Histograms")
          } else {
            dir.create("Histograms")
            setwd("Histograms")
          }

          filenameStart <- paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, " Histogram")
          histogram(questionDataProcessed, filenameStart, title = metaTable$title[i], stage = currentStage, specialty, expectedRisk, forecastMin, forecastMax)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[l], "/", beliefSets[k]))

          files <- c(paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, ".csv"))
          filenameStart <- paste0(currentSetName, " - ", currentQuestionName, " - ", currentStage, " Box Plot")
          boxPlot(files, type = "regGroups", specialty, title = metaTable$title[i], subtitle = paste0("Stage ", currentStage, " | ", years[l], " | ", beliefSets[k]), filenameStart, expectedRisk, forecastMin, forecastMax)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[l], "/", beliefSets[k]))

          newRow <- newRowInit(metaTable, questionDataProcessed, currentSetName, currentQuestionName, answerText = "", stage = currentStage, specialty = metaTable[i, ]$specialty)
          newAdd <- rbind(newAdd, newRow)
        }
      }
    }

    # CONVERGENCE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    print("Getting convergence data...")
    for (j in 1:length(years)) {
      setwd(years[j])
      print(years[j])

      for (k in 1:length(beliefSets)) {
        setwd(beliefSets[k])
        print(beliefSets[k])

        phase1 <- read.csv(list.files()[grep("Phase 1.csv", list.files())])
        phase1median <- median(phase1$forecast)
        phase1sd <- sd(phase1$forecast)
        phase1 <- phase1 %>%
          filter(forecast > phase1median - (10 * phase1sd)) %>%
          filter(forecast < phase1median + (10 * phase1sd))
        phase2 <- read.csv(list.files()[grep("Phase 2.csv", list.files())])
        phase2median <- median(phase2$forecast)
        phase2sd <- sd(phase2$forecast)
        phase2 <- phase2 %>%
          filter(forecast > phase2median - (10 * phase2sd)) %>%
          filter(forecast < phase2median + (10 * phase2sd))
        phase3 <- read.csv(list.files()[grep("Phase 3.csv", list.files())])
        phase3median <- median(phase3$forecast)
        phase3sd <- sd(phase3$forecast)
        phase3 <- phase3 %>%
          filter(forecast > phase3median - (10 * phase3sd)) %>%
          filter(forecast < phase3median + (10 * phase3sd))
        phase4 <- read.csv(list.files()[grep("Phase 4.csv", list.files())])
        phase4median <- median(phase4$forecast)
        phase4sd <- sd(phase4$forecast)
        phase4 <- phase4 %>%
          filter(forecast > phase4median - (10 * phase4sd)) %>%
          filter(forecast < phase4median + (10 * phase4sd))

        setwd(paste0(yourHome, "Summary Data"))
        convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
        convergenceTable <- rbind(convergenceTable, convergenceRow)

        write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    for (j in 1:length(years)) {
      if (!dir.exists(years[j])) {
        dir.create(years[j])
        setwd(years[j])
      } else {
        setwd(years[j])
      }
      for (k in 1:length(beliefSets)) {
        if (!dir.exists(beliefSets[k])) {
          dir.create(beliefSets[k])
        }
      }
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))
    }

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(beliefSets)) {
      print(beliefSets[j])

      for (k in 1:length(years)) {
        currentSetTimeSeries <- figureDataInit()

        print(years[k])
        currentQuestionName <- paste(years[k], beliefSets[j])

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[k], "/", beliefSets[j]))

        questionDataRaw <- data %>%
          filter(setName == currentSetName) %>%
          filter(questionName == currentQuestionName) %>%
          filter(forecast != defaultForecast)

        totalSupers <- nrow(unique(questionDataRaw %>% filter(userName %in% supers) %>% select(userName)))
        totalExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName) %>% select(userName)))
        totalDomainExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName[expertsG1$specialty1 == qSpecialty | expertsG1$specialty2 == qSpecialty | expertsG1$specialty3 == qSpecialty]) %>% select(userName)))

        for (l in 1:length(timeline)) {
          currentDate <- timeline[l]
          if (l == 1 | (l %% 30) == 0) {
            print(currentDate)
          }

          dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 1)
          if (l == length(timeline)) {
            dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 2)
          }
          users <- unique(dateDataRaw$userName)
          users <- users[users %in% c(supers, expertsG1$userName, expertsG2)]

          dateDataProcessed <- data.frame(row.names = names(dateDataRaw))

          for (m in 1:length(users)) {
            user <- users[m]
            userForecasts <- dateDataRaw %>% filter(userName == user)

            mostRecentForecast <- filter(userForecasts, timestamp == max(userForecasts$timestamp))

            dateDataProcessed <- rbind(dateDataProcessed, mostRecentForecast)
          }

          currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = beliefSets[j], year = years[k], date = currentDate, qSpecialty))
        }

        write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", currentQuestionName, ".csv"), row.names = FALSE)

        multiYearReciprocalGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName)
        multiYearReciprocalVarianceGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName)
      }
    }
  }
  return(newAdd)
}

pointDistrib <- function(metaTable, data) {
  #' Stats and Graphs for Point Distribution Questions
  #'
  #' @description For questions where we just asked for a distribution, like:
  #' By what year will humans go extinct or first have a population less than 5,000?
  #'
  #' Builds out the folder structure, populates with stats and graphs.
  #'
  #' @param metaTable - metadata for all the point distribution question sets
  #' @param data - data for all users on all point distribution question sets
  #'
  #' @export

  newAdd <- newAddInit()

  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

  # For each of the risk categories (genetically engineered pathogen risk, AI catastrophe, etc.)
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    if (dir.exists(currentSetName)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName))
    } else {
      setwd(paste0(yourHome, "Summary Data"))
      dir.create(currentSetName)
      setwd(currentSetName)
    }

    # PHASE DATA

    if (dir.exists("Phase Data")) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    } else {
      dir.create("Phase Data")
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(distrib)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(distrib[k])
        currentAnswerText <- distrib[k]

        if (dir.exists(currentAnswerText)) {
          setwd(currentAnswerText)
        } else {
          dir.create(currentAnswerText)
          setwd(currentAnswerText)
        }

        if (currentAnswerText == "5th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast5
        } else if (currentAnswerText == "25th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast25
        } else if (currentAnswerText == "50th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast50
        } else if (currentAnswerText == "75th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast75
        } else if (currentAnswerText == "95th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast95
        }

        questionDataRaw <- data %>% filter(setName == currentSetName)
        uniqueForecasts <- questionDataRaw %>% select(userName, timestamp)
        uniqueForecasts <- unique(uniqueForecasts)

        exclude <- data.frame(row.names = names(uniqueForecasts))

        # rm non monotonic
        for (l in 1:nrow(uniqueForecasts)) {
          # print(l)
          currRow <- questionDataRaw %>%
            filter(userName == uniqueForecasts[l, ]$userName) %>%
            filter(timestamp %in% seq(uniqueForecasts[l, ]$timestamp - 60, uniqueForecasts[l, ]$timestamp + 60, 1))
          if (any(
            (currRow[currRow$answerText == "5th %", ]$forecast > currRow[currRow$answerText == "25th %", ]$forecast) |
              (currRow[currRow$answerText == "25th %", ]$forecast > currRow[currRow$answerText == "50th %", ]$forecast) |
              (currRow[currRow$answerText == "50th %", ]$forecast > currRow[currRow$answerText == "75th %", ]$forecast) |
              (currRow[currRow$answerText == "75th %", ]$forecast > currRow[currRow$answerText == "95th %", ]$forecast)
          )) {
            exclude <- rbind(exclude, uniqueForecasts[l, ])
          }
        }

        for (l in 1:nrow(exclude)) {
          removeRow <- questionDataRaw %>%
            filter(userName == exclude[l, ]$userName) %>%
            filter(timestamp == exclude[l, ]$timestamp)
          for (m in 1:nrow(removeRow)) {
            currRow <- removeRow[m, ]
            questionDataRaw <- questionDataRaw[!((questionDataRaw$userName == currRow$userName) & (questionDataRaw$answerText == currRow$answerText) & (questionDataRaw$forecast == currRow$forecast) & questionDataRaw$timestamp == currRow$timestamp)]
          }
        }

        questionDataRaw <- questionDataRaw %>% filter(answerText == currentAnswerText)
        users <- unique(questionDataRaw$userName)
        # users = users[!(users %in% c(supers, expertsG1$userName, expertsG2))]
        users <- users[(users %in% c(supers, expertsG1$userName, expertsG2))]

        questionDataProcessed <- data.frame(row.names = names(supers))

        if (currentStage == 1) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (nrow(userForecasts) == 0) {
              print(user)
            }
            if (!any(is.na(userForecasts$stageTwoTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageTwoTimestamp)
            } else if (!any(is.na(userForecasts$stageThreeTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
            } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 2) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (!any(is.na(userForecasts$stageThreeTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
            } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 3) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 4) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        }

        questionDataProcessed <- questionDataProcessed %>% filter(forecast != defaultForecast)

        questionDataProcessed_anon <- questionDataProcessed %>% select(!userName)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentAnswerText))
        write.csv(questionDataProcessed, paste0(currentSetName, " - ", currentAnswerText, " - Phase ", currentStage, ".csv"), row.names = FALSE)
        write.csv(questionDataProcessed_anon, paste0(currentSetName, " - ", currentAnswerText, " - Phase ", currentStage, "_ANON.csv"), row.names = FALSE)

        if (dir.exists("Histograms")) {
          setwd("Histograms")
        } else {
          dir.create("Histograms")
          setwd("Histograms")
        }

        filenameStart <- paste0(currentSetName, " - ", currentAnswerText, " - Phase ", currentStage, " Histogram")
        histogram(questionDataProcessed, filenameStart, title = metaTable$title[i], stage = currentStage, specialty, expectedRisk, forecastMin, forecastMax)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        newRow <- newRowInit(metaTable, questionDataProcessed, currentSetName, currentQuestionName = "", answerText = currentAnswerText, stage = currentStage, specialty = metaTable[i, ]$specialty)
        newAdd <- rbind(newAdd, newRow)
      }
    }

    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

    phases <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4")

    print("Making box plots...")

    for (j in 1:length(phases)) {
      print(paste("Stage", j))

      currentPhase <- phases[j]

      for (k in 1:length(distrib)) {
        setwd(distrib[k])
        currentDistribFiles <- list.files()
        currFile <- currentDistribFiles[grep(currentPhase, currentDistribFiles)]
        currFile <- currFile[!grepl("ANON", currFile)]
        assign(paste0("file", k), read.csv(currFile))
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
      }

      phaseTbl <- rbind(file1, file2, file3, file4, file5)

      if (dir.exists("BoxPlots")) {
        setwd("BoxPlots")
      } else {
        dir.create("BoxPlots")
        setwd("BoxPlots")
      }

      boxPlot_distrib(tbl = phaseTbl, specialty, title = metaTable$title[i], forecastMin = metaTable$forecastMin[i], forecastMax = metaTable$forecastMax[i], stage = j, year = "")

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # CONVERGENCE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    print("Getting convergence data...")

    for (j in 1:length(distrib)) {
      setwd(distrib[j])
      print(distrib[j])

      phase1 <- read.csv(list.files()[grep("Phase 1.csv", list.files())])
      phase1median <- median(phase1$forecast)
      phase1sd <- sd(phase1$forecast)
      phase1 <- phase1 %>%
        filter(forecast > phase1median - (10 * phase1sd)) %>%
        filter(forecast < phase1median + (10 * phase1sd))
      phase2 <- read.csv(list.files()[grep("Phase 2.csv", list.files())])
      phase2median <- median(phase2$forecast)
      phase2sd <- sd(phase2$forecast)
      phase2 <- phase2 %>%
        filter(forecast > phase2median - (10 * phase2sd)) %>%
        filter(forecast < phase2median + (10 * phase2sd))
      phase3 <- read.csv(list.files()[grep("Phase 3.csv", list.files())])
      phase3median <- median(phase3$forecast)
      phase3sd <- sd(phase3$forecast)
      phase3 <- phase3 %>%
        filter(forecast > phase3median - (10 * phase3sd)) %>%
        filter(forecast < phase3median + (10 * phase3sd))
      phase4 <- read.csv(list.files()[grep("Phase 4.csv", list.files())])
      phase4median <- median(phase4$forecast)
      phase4sd <- sd(phase4$forecast)
      phase4 <- phase4 %>%
        filter(forecast > phase4median - (10 * phase4sd)) %>%
        filter(forecast < phase4median + (10 * phase4sd))

      setwd(paste0(yourHome, "Summary Data"))
      convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
      convergenceTable <- rbind(convergenceTable, convergenceRow)

      write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # FIGURE DATA

    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

    preQRaw <- data %>% filter(setName == currentSetName)

    uniqueForecasts <- unique(data %>% filter(setName == currentSetName) %>% select(userName, timestamp))
    exclude <- data.frame(row.names = names(uniqueForecasts))

    # rm non monotonic
    for (j in 1:nrow(uniqueForecasts)) {
      # print(l)
      currRow <- preQRaw %>%
        filter(userName == uniqueForecasts[j, ]$userName) %>%
        filter(timestamp %in% seq(uniqueForecasts[j, ]$timestamp - 60, uniqueForecasts[j, ]$timestamp + 60, 1))
      if (any(
        (currRow[currRow$answerText == "5th %", ]$forecast > currRow[currRow$answerText == "25th %", ]$forecast) |
          (currRow[currRow$answerText == "25th %", ]$forecast > currRow[currRow$answerText == "50th %", ]$forecast) |
          (currRow[currRow$answerText == "50th %", ]$forecast > currRow[currRow$answerText == "75th %", ]$forecast) |
          (currRow[currRow$answerText == "75th %", ]$forecast > currRow[currRow$answerText == "95th %", ]$forecast)
      )) {
        exclude <- rbind(exclude, uniqueForecasts[j, ])
      }
    }

    for (j in 1:nrow(exclude)) {
      removeRow <- preQRaw %>%
        filter(userName == exclude[j, ]$userName) %>%
        filter(timestamp == exclude[j, ]$timestamp)
      for (k in 1:nrow(removeRow)) {
        currRow <- removeRow[k, ]
        preQRaw <- preQRaw[!(preQRaw$userName == currRow$userName & preQRaw$timestamp == currRow$timestamp & preQRaw$answerText == currRow$answerText)]
      }
    }

    for (j in 1:length(distrib)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

      print(distrib[j])
      currentAnswerText <- distrib[j]

      if (dir.exists(distrib[j])) {
        setwd(distrib[j])
      } else {
        dir.create(distrib[j])
        setwd(distrib[j])
      }

      if (currentAnswerText == "5th %") {
        defaultForecast <- metaTable[i, ]$defaultForecast5
      } else if (currentAnswerText == "25th %") {
        defaultForecast <- metaTable[i, ]$defaultForecast25
      } else if (currentAnswerText == "50th %") {
        defaultForecast <- metaTable[i, ]$defaultForecast50
      } else if (currentAnswerText == "75th %") {
        defaultForecast <- metaTable[i, ]$defaultForecast75
      } else if (currentAnswerText == "95th %") {
        defaultForecast <- metaTable[i, ]$defaultForecast95
      }

      currentSetTimeSeries <- figureDataInit()

      questionDataRaw <- preQRaw %>%
        filter(setName == currentSetName) %>%
        filter(answerText == currentAnswerText) %>%
        filter(forecast != defaultForecast)

      totalSupers <- nrow(unique(questionDataRaw %>% filter(userName %in% supers) %>% select(userName)))
      totalExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName) %>% select(userName)))
      totalDomainExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName[expertsG1$specialty1 == qSpecialty | expertsG1$specialty2 == qSpecialty | expertsG1$specialty3 == qSpecialty]) %>% select(userName)))

      for (k in 1:length(timeline)) {
        currentDate <- timeline[k]
        if (k == 1 | k %% 30 == 0) {
          print(currentDate)
        }

        dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 1)
        if (k == length(timeline)) {
          dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 2)
        }
        users <- unique(dateDataRaw$userName)
        users <- users[users %in% c(supers, expertsG1$userName, expertsG2)]

        dateDataProcessed <- data.frame(row.names = names(dateDataRaw))

        for (l in 1:length(users)) {
          user <- users[l]
          userForecasts <- dateDataRaw %>% filter(userName == user)

          mostRecentForecast <- filter(userForecasts, timestamp == max(userForecasts$timestamp))

          dateDataProcessed <- rbind(dateDataProcessed, mostRecentForecast)
        }

        currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = "", date = currentDate, qSpecialty))
      }

      write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", currentAnswerText, ".csv"), row.names = FALSE)

      pointDistribGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, distrib[j])
      pointDistribVarianceGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, currentDistrib = distrib[j])
    }
  }
  return(newAdd)
}

multiYearDistrib <- function(metaTable, data) {
  #' Stats and graphs for Multi-year Distribution Questions
  #'
  #' @description For the questions where we ask for distributions over predefined years, like:
  #' How many total nuclear warheads will be in military inventories globally by
  #'    ...the end of 2024?
  #'    ...the end of 2030?
  #'    ...the end of 2040?
  #'
  #' Builds out the folder structure, populates with stats and graphs.
  #'
  #' @param metaTable - metadata for all the multi-year distribution question sets
  #' @param data - data for all users on all multi-year distribution question sets
  #'
  #' @export

  newAdd <- newAddInit()
  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

  for (i in 1:length(unique(metaTable$setName))) {
    # for(i in 19:length(unique(metaTable$setName))){
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    write.csv(newAdd, "multiYearDistrib_partial.csv", row.names = FALSE)

    if (dir.exists(currentSetName)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName))
    } else {
      setwd(paste0(yourHome, "Summary Data"))
      dir.create(currentSetName)
      setwd(currentSetName)
    }

    # PHASE DATA

    if (dir.exists("Phase Data")) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    } else {
      dir.create("Phase Data")
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(years)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(years[k])

        currentYear <- years[k]

        if (dir.exists(years[k])) {
          setwd(years[k])
        } else {
          dir.create(years[k])
          setwd(years[k])
        }

        for (l in 1:length(distrib)) {
          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))

          print(distrib[l])

          currentAnswerText <- distrib[l]

          if (dir.exists(currentAnswerText)) {
            setwd(currentAnswerText)
          } else {
            dir.create(currentAnswerText)
            setwd(currentAnswerText)
          }

          if (currentAnswerText == "5th %") {
            defaultForecast <- metaTable[i, ]$defaultForecast5
          } else if (currentAnswerText == "25th %") {
            defaultForecast <- metaTable[i, ]$defaultForecast25
          } else if (currentAnswerText == "50th %") {
            defaultForecast <- metaTable[i, ]$defaultForecast50
          } else if (currentAnswerText == "75th %") {
            defaultForecast <- metaTable[i, ]$defaultForecast75
          } else if (currentAnswerText == "95th %") {
            defaultForecast <- metaTable[i, ]$defaultForecast95
          }

          questionDataRaw <- data %>%
            filter(setName == currentSetName) %>%
            filter(questionName == currentYear)
          uniqueForecasts <- questionDataRaw %>% select(userName, timestamp)
          uniqueForecasts <- unique(uniqueForecasts)

          exclude <- data.frame(
            userName = character(0),
            timestamp = Date(0)
          )

          # rm non monotonic
          for (m in 1:nrow(uniqueForecasts)) {
            # print(m)
            currRow <- questionDataRaw %>%
              filter(userName == uniqueForecasts[m, ]$userName) %>%
              filter(timestamp %in% seq(uniqueForecasts[m, ]$timestamp - 60, uniqueForecasts[m, ]$timestamp + 60, 1))
            if (any(
              (currRow[currRow$answerText == "5th %", ]$forecast > currRow[currRow$answerText == "25th %", ]$forecast) |
                (currRow[currRow$answerText == "25th %", ]$forecast > currRow[currRow$answerText == "50th %", ]$forecast) |
                (currRow[currRow$answerText == "50th %", ]$forecast > currRow[currRow$answerText == "75th %", ]$forecast) |
                (currRow[currRow$answerText == "75th %", ]$forecast > currRow[currRow$answerText == "95th %", ]$forecast)
            )) {
              # print(m)
              exclude <- rbind(exclude, uniqueForecasts[m, ])
            }
          }

          if (nrow(exclude) > 0) {
            for (m in 1:nrow(exclude)) {
              removeRow <- questionDataRaw %>%
                filter(userName == exclude[m, ]$userName) %>%
                filter(timestamp == exclude[m, ]$timestamp)
              for (n in 1:nrow(removeRow)) {
                currRow <- removeRow[n, ]
                questionDataRaw <- questionDataRaw[!((questionDataRaw$userName == currRow$userName) & (questionDataRaw$answerText == currRow$answerText) & (questionDataRaw$forecast == currRow$forecast) & questionDataRaw$timestamp == currRow$timestamp)]
              }
            }
          }

          questionDataRaw <- questionDataRaw %>% filter(answerText == currentAnswerText)
          users <- unique(questionDataRaw$userName)
          # users = users[!(users %in% c(supers, expertsG1$userName, expertsG2))]
          users <- users[(users %in% c(supers, expertsG1$userName, expertsG2))]

          questionDataProcessed <- data.frame(row.names = names(supers))

          if (currentStage == 1) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (nrow(userForecasts) == 0) {
                print(user)
              }
              if (!any(is.na(userForecasts$stageTwoTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageTwoTimestamp)
              } else if (!any(is.na(userForecasts$stageThreeTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
              } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 2) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (!any(is.na(userForecasts$stageThreeTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
              } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 3) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 4) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          }

          questionDataProcessed <- questionDataProcessed %>% filter(forecast != defaultForecast)

          questionDataProcessed_anon <- questionDataProcessed %>% select(!userName)

          write.csv(questionDataProcessed, paste0(currentSetName, " - ", currentYear, " - ", currentAnswerText, " - Phase ", currentStage, ".csv"), row.names = FALSE)
          write.csv(questionDataProcessed_anon, paste0(currentSetName, " - ", currentYear, " - ", currentAnswerText, " - Phase ", currentStage, "_ANON.csv"), row.names = FALSE)

          if (dir.exists("Histograms")) {
            setwd("Histograms")
          } else {
            dir.create("Histograms")
            setwd("Histograms")
          }

          filenameStart <- paste0(currentSetName, " - ", currentYear, " - ", currentAnswerText, " - Phase ", currentStage, " Histogram")
          histogram(questionDataProcessed, filenameStart, title = metaTable$title[i], stage = currentStage, specialty, expectedRisk, forecastMin, forecastMax)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))

          newRow <- newRowInit(metaTable, questionDataProcessed, currentSetName, currentQuestionName = currentYear, answerText = currentAnswerText, stage = currentStage, specialty = metaTable[i, ]$specialty)
          newAdd <- rbind(newAdd, newRow)

          setwd(paste0(yourHome, "Summary Data"))
          write.csv(newAdd, "multiYearDistrib_partial.csv", row.names = FALSE)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))

          ####
        }
      }
    }

    for (j in 1:length(years)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))

      phases <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4")

      print(paste0("Making box plots for ", years[j], "..."))

      for (k in 1:length(phases)) {
        print(paste("Stage", k))

        currentPhase <- phases[k]

        for (l in 1:length(distrib)) {
          setwd(distrib[l])
          currentDistribFiles <- list.files()
          currFile <- currentDistribFiles[grep(currentPhase, currentDistribFiles)]
          currFile <- currFile[!grepl("ANON", currFile)]
          assign(paste0("file", l), read.csv(currFile))
          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
        }

        phaseTbl <- rbind(file1, file2, file3, file4, file5)

        if (dir.exists("BoxPlots")) {
          setwd("BoxPlots")
        } else {
          dir.create("BoxPlots")
          setwd("BoxPlots")
        }

        boxPlot_distrib(tbl = phaseTbl, specialty, title = metaTable$title[i], forecastMin = metaTable$forecastMin[i], forecastMax = metaTable$forecastMax[i], stage = k, year = years[j])

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }
    }

    # CONVERGENCE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    print("Getting convergence data...")

    for (j in 1:length(years)) {
      setwd(years[j])
      print(years[j])

      for (k in 1:length(distrib)) {
        setwd(distrib[k])
        print(distrib[k])

        phase1 <- read.csv(list.files()[grep("Phase 1.csv", list.files())])
        phase1median <- median(phase1$forecast)
        phase1sd <- sd(phase1$forecast)
        phase1 <- phase1 %>%
          filter(forecast > phase1median - (10 * phase1sd)) %>%
          filter(forecast < phase1median + (10 * phase1sd))
        phase2 <- read.csv(list.files()[grep("Phase 2.csv", list.files())])
        phase2median <- median(phase2$forecast)
        phase2sd <- sd(phase2$forecast)
        phase2 <- phase2 %>%
          filter(forecast > phase2median - (10 * phase2sd)) %>%
          filter(forecast < phase2median + (10 * phase2sd))
        phase3 <- read.csv(list.files()[grep("Phase 3.csv", list.files())])
        phase3median <- median(phase3$forecast)
        phase3sd <- sd(phase3$forecast)
        phase3 <- phase3 %>%
          filter(forecast > phase3median - (10 * phase3sd)) %>%
          filter(forecast < phase3median + (10 * phase3sd))
        phase4 <- read.csv(list.files()[grep("Phase 4.csv", list.files())])
        phase4median <- median(phase4$forecast)
        phase4sd <- sd(phase4$forecast)
        phase4 <- phase4 %>%
          filter(forecast > phase4median - (10 * phase4sd)) %>%
          filter(forecast < phase4median + (10 * phase4sd))

        setwd(paste0(yourHome, "Summary Data"))
        convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
        convergenceTable <- rbind(convergenceTable, convergenceRow)

        write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(years)) {
      print(years[j])

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

      if (dir.exists(years[j])) {
        setwd(years[j])
      } else {
        dir.create(years[j])
        setwd(years[j])
      }

      preQRaw <- data %>%
        filter(setName == currentSetName) %>%
        filter(questionName == years[j])

      uniqueForecasts <- unique(data %>% filter(setName == currentSetName) %>% select(userName, timestamp))
      exclude <- data.frame(row.names = names(uniqueForecasts))

      # rm non monotonic
      for (k in 1:nrow(uniqueForecasts)) {
        # print(l)
        currRow <- preQRaw %>%
          filter(userName == uniqueForecasts[k, ]$userName) %>%
          filter(timestamp %in% seq(uniqueForecasts[k, ]$timestamp - 60, uniqueForecasts[k, ]$timestamp + 60, 1))
        if (any(
          (currRow[currRow$answerText == "5th %", ]$forecast > currRow[currRow$answerText == "25th %", ]$forecast) |
            (currRow[currRow$answerText == "25th %", ]$forecast > currRow[currRow$answerText == "50th %", ]$forecast) |
            (currRow[currRow$answerText == "50th %", ]$forecast > currRow[currRow$answerText == "75th %", ]$forecast) |
            (currRow[currRow$answerText == "75th %", ]$forecast > currRow[currRow$answerText == "95th %", ]$forecast)
        )) {
          exclude <- rbind(exclude, uniqueForecasts[k, ])
        }
      }

      if (ncol(exclude) > 0) {
        for (k in 1:nrow(exclude)) {
          removeRow <- preQRaw %>%
            filter(userName == exclude[k, ]$userName) %>%
            filter(timestamp == exclude[k, ]$timestamp)
          for (l in 1:nrow(removeRow)) {
            currRow <- removeRow[l, ]
            preQRaw <- preQRaw[!(preQRaw$userName == currRow$userName & preQRaw$timestamp == currRow$timestamp & preQRaw$answerText == currRow$answerText)]
          }
        }
      }

      for (k in 1:length(distrib)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j]))

        print(distrib[k])
        currentAnswerText <- distrib[k]

        if (dir.exists(distrib[k])) {
          setwd(distrib[k])
        } else {
          dir.create(distrib[k])
          setwd(distrib[k])
        }

        if (currentAnswerText == "5th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast5
        } else if (currentAnswerText == "25th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast25
        } else if (currentAnswerText == "50th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast50
        } else if (currentAnswerText == "75th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast75
        } else if (currentAnswerText == "95th %") {
          defaultForecast <- metaTable[i, ]$defaultForecast95
        }

        currentSetTimeSeries <- figureDataInit()

        questionDataRaw <- preQRaw %>%
          filter(setName == currentSetName) %>%
          filter(answerText == currentAnswerText) %>%
          filter(forecast != defaultForecast)

        totalSupers <- nrow(unique(questionDataRaw %>% filter(userName %in% supers) %>% select(userName)))
        totalExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName) %>% select(userName)))
        totalDomainExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName[expertsG1$specialty1 == qSpecialty | expertsG1$specialty2 == qSpecialty | expertsG1$specialty3 == qSpecialty]) %>% select(userName)))

        for (l in 1:length(timeline)) {
          currentDate <- timeline[l]
          if (l == 1 | l %% 30 == 0) {
            print(currentDate)
          }

          dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 1)
          if (l == length(timeline)) {
            dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 2)
          }
          users <- unique(dateDataRaw$userName)
          users <- users[users %in% c(supers, expertsG1$userName, expertsG2)]

          dateDataProcessed <- data.frame(row.names = names(dateDataRaw))

          for (m in 1:length(users)) {
            user <- users[m]
            userForecasts <- dateDataRaw %>% filter(userName == user)

            mostRecentForecast <- filter(userForecasts, timestamp == max(userForecasts$timestamp))

            dateDataProcessed <- rbind(dateDataProcessed, mostRecentForecast)
          }

          currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = "", date = currentDate, qSpecialty))
        }

        write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", years[j], " - ", currentAnswerText, ".csv"), row.names = FALSE)

        multiYearDistribGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j], currentDistrib = distrib[k])
        multiYearDistribVarianceGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j], currentDistrib = distrib[k])
      }
    }
  }
  return(newAdd)
}

multiYearBinary <- function(metaTable, data) {
  #' Stats and graphs for Multi-year Binary Questions
  #'
  #' @description For yes/no multi-year questions, like:
  #' What is the probability that the use of a nuclear weapon (in a single
  #' event) will cause the death of more than 1,000 people...
  #' ...by 2024?
  #' ...by 2030?
  #'
  #' Builds out the folder structure, populates with stats and graphs.
  #'
  #' @param metaTable - metadata for all the multi-year reciprocal question sets
  #' @param data - data for all users on all multi-year reciprocal question sets
  #'
  #' @export

  newAdd <- newAddInit()

  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    write.csv(newAdd, "multiYearBinary_partial.csv", row.names = FALSE)

    if (dir.exists(currentSetName)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName))
    } else {
      setwd(paste0(yourHome, "Summary Data"))
      dir.create(currentSetName)
      setwd(currentSetName)
    }

    # PHASE DATA

    if (dir.exists("Phase Data")) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    } else {
      dir.create("Phase Data")
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(years)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(years[k])
        currentYear <- years[k]

        if (dir.exists(years[k])) {
          setwd(years[k])
        } else {
          dir.create(years[k])
          setwd(years[k])
        }

        defaultForecast <- metaTable[i, ]$defaultForecast50

        questionDataRaw <- data %>%
          filter(setName == currentSetName) %>%
          filter(questionName == currentYear)
        users <- unique(questionDataRaw$userName)
        # users = users[!(users %in% c(supers, expertsG1$userName, expertsG2))]
        users <- users[(users %in% c(supers, expertsG1$userName, expertsG2))]

        questionDataProcessed <- data.frame(row.names = names(supers))

        if (currentStage == 1) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (nrow(userForecasts) == 0) {
              print(user)
            }
            if (!any(is.na(userForecasts$stageTwoTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageTwoTimestamp)
            } else if (!any(is.na(userForecasts$stageThreeTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
            } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 2) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (!any(is.na(userForecasts$stageThreeTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
            } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 3) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 4) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        }

        questionDataProcessed <- questionDataProcessed %>% filter(forecast != defaultForecast)

        questionDataProcessed_anon <- questionDataProcessed %>% select(!userName)

        write.csv(questionDataProcessed, paste0(currentSetName, " - ", currentYear, " - Phase ", currentStage, ".csv"), row.names = FALSE)
        write.csv(questionDataProcessed_anon, paste0(currentSetName, " - ", currentYear, " - Phase ", currentStage, "_ANON.csv"), row.names = FALSE)

        if (dir.exists("Histograms")) {
          setwd("Histograms")
        } else {
          dir.create("Histograms")
          setwd("Histograms")
        }

        filenameStart <- paste0(currentSetName, " - ", currentYear, " - Phase ", currentStage, " Histogram")
        histogram(questionDataProcessed, filenameStart, title = metaTable$title[i], stage = currentStage, specialty, expectedRisk, forecastMin, forecastMax)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))

        newRow <- newRowInit(metaTable, questionDataProcessed, currentSetName, currentQuestionName = currentYear, answerText = "", stage = currentStage, specialty = metaTable[i, ]$specialty)
        newAdd <- rbind(newAdd, newRow)

        setwd(paste0(yourHome, "Summary Data"))
        write.csv(newAdd, "multiYearBinary_partial.csv", row.names = FALSE)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))
      }
    }

    for (j in 1:length(years)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))

      phases <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4")

      print(paste0("Making box plots for ", years[j], "..."))

      for (k in 1:length(phases)) {
        print(paste("Stage", k))

        currFiles <- list.files()
        currFile <- currFiles[grep(phases[k], currFiles)]
        currFile <- currFile[!grepl("ANON", currFile)]

        if (dir.exists("BoxPlots")) {
          setwd("BoxPlots")
        } else {
          dir.create("BoxPlots")
          setwd("BoxPlots")
        }

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))

        filenameStart <- paste(currentSetName, "-", "Stage", k)

        boxPlot(files = currFile, type = "regGroups", specialty, title = metaTable$title[i], subtitle = metaTable$subtitle[i], filenameStart, expectedRisk, forecastMin, forecastMax)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }
    }

    # CONVERGENCE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    print("Getting convergence data...")

    for (j in 1:length(years)) {
      setwd(years[j])
      print(years[j])

      phase1 <- read.csv(list.files()[grep("Phase 1.csv", list.files())])
      phase1median <- median(phase1$forecast)
      phase1sd <- sd(phase1$forecast)
      phase1 <- phase1 %>%
        filter(forecast > phase1median - (10 * phase1sd)) %>%
        filter(forecast < phase1median + (10 * phase1sd))
      phase2 <- read.csv(list.files()[grep("Phase 2.csv", list.files())])
      phase2median <- median(phase2$forecast)
      phase2sd <- sd(phase2$forecast)
      phase2 <- phase2 %>%
        filter(forecast > phase2median - (10 * phase2sd)) %>%
        filter(forecast < phase2median + (10 * phase2sd))
      phase3 <- read.csv(list.files()[grep("Phase 3.csv", list.files())])
      phase3median <- median(phase3$forecast)
      phase3sd <- sd(phase3$forecast)
      phase3 <- phase3 %>%
        filter(forecast > phase3median - (10 * phase3sd)) %>%
        filter(forecast < phase3median + (10 * phase3sd))
      phase4 <- read.csv(list.files()[grep("Phase 4.csv", list.files())])
      phase4median <- median(phase4$forecast)
      phase4sd <- sd(phase4$forecast)
      phase4 <- phase4 %>%
        filter(forecast > phase4median - (10 * phase4sd)) %>%
        filter(forecast < phase4median + (10 * phase4sd))

      setwd(paste0(yourHome, "Summary Data"))
      convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
      convergenceTable <- rbind(convergenceTable, convergenceRow)

      write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(years)) {
      print(years[j])

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

      if (dir.exists(years[j])) {
        setwd(years[j])
      } else {
        dir.create(years[j])
        setwd(years[j])
      }

      preQRaw <- data %>%
        filter(setName == currentSetName) %>%
        filter(questionName == years[j])

      currentSetTimeSeries <- figureDataInit()
      defaultForecast <- metaTable[i, ]$defaultForecast50

      questionDataRaw <- preQRaw %>%
        filter(setName == currentSetName) %>%
        filter(forecast != defaultForecast)

      totalSupers <- nrow(unique(questionDataRaw %>% filter(userName %in% supers) %>% select(userName)))
      totalExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName) %>% select(userName)))
      totalDomainExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName[expertsG1$specialty1 == qSpecialty | expertsG1$specialty2 == qSpecialty | expertsG1$specialty3 == qSpecialty]) %>% select(userName)))

      for (k in 1:length(timeline)) {
        currentDate <- timeline[k]
        if (k == 1 | k %% 30 == 0) {
          print(currentDate)
        }

        dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 1)
        if (k == length(timeline)) {
          dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 2)
        }
        users <- unique(dateDataRaw$userName)
        users <- users[users %in% c(supers, expertsG1$userName, expertsG2)]

        dateDataProcessed <- data.frame(row.names = names(dateDataRaw))

        for (l in 1:length(users)) {
          user <- users[l]
          userForecasts <- dateDataRaw %>% filter(userName == user)

          mostRecentForecast <- filter(userForecasts, timestamp == max(userForecasts$timestamp))

          dateDataProcessed <- rbind(dateDataProcessed, mostRecentForecast)
        }

        currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = "", date = currentDate, qSpecialty))
      }

      write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", years[j], ".csv"), row.names = FALSE)

      multiYearBinaryGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j])
      multiYearBinaryVarianceGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j])
    }
  }
  return(newAdd)
}

#####

multiYearCountryDistrib <- function(metaTable, data) {
  #' Stats and graphs for Multi-year Country Distribution Questions
  #'
  #' @description For questions where we ask for distributions over predefined
  #' years AND a set of countries, this function will build out the folder structure
  #' and populate it with stats and graphs.
  #'
  #' @param metaTable - metadata for all the multi-year country distribution question sets
  #' @param data - data for all users on all multi-year country distribution question sets
  #'
  #' @export

  newAdd <- newAddInit()

  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    write.csv(newAdd, "multiYearCountryDistrib_partial.csv", row.names = FALSE)

    if (dir.exists(currentSetName)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName))
    } else {
      setwd(paste0(yourHome, "Summary Data"))
      dir.create(currentSetName)
      setwd(currentSetName)
    }

    # PHASE DATA

    if (dir.exists("Phase Data")) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    } else {
      dir.create("Phase Data")
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    countries <- c(metaTable$country1[i], metaTable$country2[i], metaTable$country3[i], metaTable$country4[i], metaTable$country5[i], metaTable$country6[i], metaTable$country7[i], metaTable$country8[i], metaTable$country9[i], metaTable$country10[i], metaTable$country11[i], metaTable$country12[i])
    countries <- countries[countries != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(years)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(years[k])
        currentYear <- years[k]

        if (dir.exists(years[k])) {
          setwd(years[k])
        } else {
          dir.create(years[k])
          setwd(years[k])
        }

        for (l in 1:length(countries)) {
          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))

          print(countries[l])
          currentCountry <- countries[l]

          if (dir.exists(countries[l])) {
            setwd(countries[l])
          } else {
            dir.create(countries[l])
            setwd(countries[l])
          }

          defaultForecast <- metaTable[i, ]$defaultForecast50

          currentQuestionName <- paste0(currentYear, " (", currentCountry, ")")

          questionDataRaw <- data %>%
            filter(setName == currentSetName) %>%
            filter(questionName == currentQuestionName)
          users <- unique(questionDataRaw$userName)
          # users = users[!(users %in% c(supers, expertsG1$userName, expertsG2))]
          users <- users[(users %in% c(supers, expertsG1$userName, expertsG2))]

          questionDataProcessed <- data.frame(row.names = names(supers))

          if (currentStage == 1) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (nrow(userForecasts) == 0) {
                print(user)
              }
              if (!any(is.na(userForecasts$stageTwoTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageTwoTimestamp)
              } else if (!any(is.na(userForecasts$stageThreeTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
              } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 2) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (!any(is.na(userForecasts$stageThreeTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
              } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 3) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              if (!any(is.na(userForecasts$stageFourTimestamp))) {
                userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
              }
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          } else if (currentStage == 4) {
            for (m in 1:length(users)) {
              user <- users[m]
              userForecasts <- questionDataRaw %>% filter(userName == user)
              mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
              questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
            }
          }

          questionDataProcessed <- questionDataProcessed %>% filter(forecast != defaultForecast)

          questionDataProcessed_anon <- questionDataProcessed %>% select(!userName)

          write.csv(questionDataProcessed, paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, ".csv"), row.names = FALSE)
          write.csv(questionDataProcessed_anon, paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, "_ANON.csv"), row.names = FALSE)

          if (dir.exists("Histograms")) {
            setwd("Histograms")
          } else {
            dir.create("Histograms")
            setwd("Histograms")
          }

          filenameStart <- paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, " Histogram")
          histogram(questionDataProcessed, filenameStart, title = metaTable$title[i], stage = currentStage, specialty, expectedRisk, forecastMin, forecastMax)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear, "/", currentCountry))

          newRow <- newRowInit(metaTable, questionDataProcessed, currentSetName, currentQuestionName = currentYear, answerText = currentCountry, stage = currentStage, specialty = metaTable[i, ]$specialty)
          newAdd <- rbind(newAdd, newRow)

          setwd(paste0(yourHome, "Summary Data"))
          write.csv(newAdd, "multiYearCountryDistrib_partial.csv", row.names = FALSE)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear, "/", currentCountry))
        }
      }
    }

    for (j in 1:length(years)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))

      phases <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4")

      print(paste0("Making box plots for ", years[j], "..."))

      for (k in 1:length(phases)) {
        print(paste("Stage", k))

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j], "/", countries[1]))

        currFiles <- list.files()
        currFile <- currFiles[grep(phases[k], currFiles)]
        currFile <- read.csv(currFile[!grepl("ANON", currFile)])

        phaseTbl <- currFile

        for (l in 2:length(countries)) {
          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j], "/", countries[l]))

          currFiles <- list.files()
          currFile <- currFiles[grep(phases[k], currFiles)]
          currFile <- read.csv(currFile[!grepl("ANON", currFile)])

          phaseTbl <- rbind(phaseTbl, currFile)
        }

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))

        if (dir.exists("BoxPlots")) {
          setwd("BoxPlots")
        } else {
          dir.create("BoxPlots")
          setwd("BoxPlots")
        }

        boxPlot_distrib_country(tbl = phaseTbl, specialty, title = metaTable[i, ]$title, forecastMin, forecastMax, stage = k, year = years[j])

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }
    }

    # CONVERGENCE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    print("Getting convergence data...")

    for (j in 1:length(years)) {
      setwd(years[j])
      print(years[j])

      for (k in 1:length(countries)) {
        setwd(countries[k])
        print(countries[k])

        phase1 <- read.csv(list.files()[grep("Phase 1.csv", list.files())])
        phase1median <- median(phase1$forecast)
        phase1sd <- sd(phase1$forecast)
        phase1 <- phase1 %>%
          filter(forecast > phase1median - (10 * phase1sd)) %>%
          filter(forecast < phase1median + (10 * phase1sd))
        phase2 <- read.csv(list.files()[grep("Phase 2.csv", list.files())])
        phase2median <- median(phase2$forecast)
        phase2sd <- sd(phase2$forecast)
        phase2 <- phase2 %>%
          filter(forecast > phase2median - (10 * phase2sd)) %>%
          filter(forecast < phase2median + (10 * phase2sd))
        phase3 <- read.csv(list.files()[grep("Phase 3.csv", list.files())])
        phase3median <- median(phase3$forecast)
        phase3sd <- sd(phase3$forecast)
        phase3 <- phase3 %>%
          filter(forecast > phase3median - (10 * phase3sd)) %>%
          filter(forecast < phase3median + (10 * phase3sd))
        phase4 <- read.csv(list.files()[grep("Phase 4.csv", list.files())])
        phase4median <- median(phase4$forecast)
        phase4sd <- sd(phase4$forecast)
        phase4 <- phase4 %>%
          filter(forecast > phase4median - (10 * phase4sd)) %>%
          filter(forecast < phase4median + (10 * phase4sd))

        setwd(paste0(yourHome, "Summary Data"))
        convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
        convergenceTable <- rbind(convergenceTable, convergenceRow)

        write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(years)) {
      print(years[j])

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

      if (dir.exists(years[j])) {
        setwd(years[j])
      } else {
        dir.create(years[j])
        setwd(years[j])
      }

      for (k in 1:length(countries)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j]))

        print(countries[k])

        if (dir.exists(countries[k])) {
          setwd(countries[k])
        } else {
          dir.create(countries[k])
          setwd(countries[k])
        }

        preQRaw <- data %>%
          filter(setName == currentSetName) %>%
          filter(questionName == paste0(years[j], " (", countries[k], ")"))

        #####

        currentSetTimeSeries <- figureDataInit()
        defaultForecast <- metaTable[i, ]$defaultForecast50

        questionDataRaw <- preQRaw %>%
          filter(setName == currentSetName) %>%
          filter(forecast != defaultForecast)

        totalSupers <- nrow(unique(questionDataRaw %>% filter(userName %in% supers) %>% select(userName)))
        totalExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName) %>% select(userName)))
        totalDomainExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName[expertsG1$specialty1 == qSpecialty | expertsG1$specialty2 == qSpecialty | expertsG1$specialty3 == qSpecialty]) %>% select(userName)))

        for (l in 1:length(timeline)) {
          currentDate <- timeline[l]
          if (l == 1 | l %% 30 == 0) {
            print(currentDate)
          }

          dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 1)
          if (l == length(timeline)) {
            dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 2)
          }
          users <- unique(dateDataRaw$userName)
          users <- users[users %in% c(supers, expertsG1$userName, expertsG2)]

          dateDataProcessed <- data.frame(row.names = names(dateDataRaw))

          for (m in 1:length(users)) {
            user <- users[m]
            userForecasts <- dateDataRaw %>% filter(userName == user)

            mostRecentForecast <- filter(userForecasts, timestamp == max(userForecasts$timestamp))

            dateDataProcessed <- rbind(dateDataProcessed, mostRecentForecast)
          }

          currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = "", date = currentDate, qSpecialty))
        }

        write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", years[j], ".csv"), row.names = FALSE)

        multiYearCountryDistribGraphics(title = metaTable$title[i], subtitle = metaTable$subtitle[i], csv = currentSetTimeSeries, currentSetName, year = years[j], country = countries[k])
      }
    }
  }

  return(newAdd)
}

multiCountryBinary <- function(metaTable, data) {
  #' Stats and graphs for Multi-year Country Binary Questions
  #'
  #' @description For yes/no country questions, like country-by-country nuclear
  #' use by 2030, this function will build out the folder structure and populate
  #' it with stats and graphs.
  #'
  #' @param metaTable - metadata for all the multi-year country binary question sets
  #' @param data - data for all users on all multi-year country binary question sets
  #'
  #' @export

  newAdd <- newAddInit()

  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    write.csv(newAdd, "multiCountryBinary_partial.csv", row.names = FALSE)

    if (dir.exists(currentSetName)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName))
    } else {
      setwd(paste0(yourHome, "Summary Data"))
      dir.create(currentSetName)
      setwd(currentSetName)
    }

    # PHASE DATA

    if (dir.exists("Phase Data")) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    } else {
      dir.create("Phase Data")
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    countries <- c(metaTable$country1[i], metaTable$country2[i], metaTable$country3[i], metaTable$country4[i], metaTable$country5[i], metaTable$country6[i], metaTable$country7[i], metaTable$country8[i], metaTable$country9[i], metaTable$country10[i], metaTable$country11[i], metaTable$country12[i])
    countries <- countries[countries != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(countries)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(countries[k])
        currentCountry <- countries[k]

        if (dir.exists(countries[k])) {
          setwd(countries[k])
        } else {
          dir.create(countries[k])
          setwd(countries[k])
        }

        defaultForecast <- metaTable[i, ]$defaultForecast50

        questionDataRaw <- data %>%
          filter(setName == currentSetName) %>%
          filter(answerText == currentCountry)
        users <- unique(questionDataRaw$userName)
        # users = users[!(users %in% c(supers, expertsG1$userName, expertsG2))]
        users <- users[(users %in% c(supers, expertsG1$userName, expertsG2))]

        questionDataProcessed <- data.frame(row.names = names(supers))

        if (currentStage == 1) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (nrow(userForecasts) == 0) {
              print(user)
            }
            if (!any(is.na(userForecasts$stageTwoTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageTwoTimestamp)
            } else if (!any(is.na(userForecasts$stageThreeTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
            } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 2) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (!any(is.na(userForecasts$stageThreeTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
            } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 3) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            if (!any(is.na(userForecasts$stageFourTimestamp))) {
              userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
            }
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        } else if (currentStage == 4) {
          for (l in 1:length(users)) {
            user <- users[l]
            userForecasts <- questionDataRaw %>% filter(userName == user)
            mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
            questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
          }
        }

        questionDataProcessed <- questionDataProcessed %>% filter(forecast != defaultForecast)

        questionDataProcessed_anon <- questionDataProcessed %>% select(!userName)

        write.csv(questionDataProcessed, paste0(currentSetName, " - ", currentCountry, " - Phase ", currentStage, ".csv"), row.names = FALSE)
        write.csv(questionDataProcessed_anon, paste0(currentSetName, " - ", currentCountry, " - Phase ", currentStage, "_ANON.csv"), row.names = FALSE)

        if (dir.exists("Histograms")) {
          setwd("Histograms")
        } else {
          dir.create("Histograms")
          setwd("Histograms")
        }

        filenameStart <- paste0(currentSetName, " - ", currentCountry, " - Phase ", currentStage, " Histogram")
        histogram(questionDataProcessed, filenameStart, title = metaTable$title[i], stage = currentStage, specialty, expectedRisk, forecastMin, forecastMax)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentCountry))

        newRow <- newRowInit(metaTable, questionDataProcessed, currentSetName, currentQuestionName = "", answerText = currentCountry, stage = currentStage, specialty = metaTable[i, ]$specialty)
        newAdd <- rbind(newAdd, newRow)

        setwd(paste0(yourHome, "Summary Data"))
        write.csv(newAdd, "multiCountryBinary_partial.csv", row.names = FALSE)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentCountry))
      }
    }

    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

    phases <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4")

    print(paste0("Making box plots..."))

    for (j in 1:length(phases)) {
      print(paste("Stage:", j))

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", countries[1]))

      currFiles <- list.files()
      currFile <- currFiles[grep(phases[j], currFiles)]
      currFile <- read.csv(currFile[!grepl("ANON", currFile)])

      phaseTbl <- currFile

      for (k in 2:length(countries)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", countries[k]))

        currFiles <- list.files()
        currFile <- currFiles[grep(phases[j], currFiles)]
        currFile <- read.csv(currFile[!grepl("ANON", currFile)])

        phaseTbl <- rbind(phaseTbl, currFile)
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

      if (dir.exists("BoxPlots")) {
        setwd("BoxPlots")
      } else {
        dir.create("BoxPlots")
        setwd("BoxPlots")
      }

      boxPlot_country(tbl = phaseTbl, specialty, title = metaTable$title[i], forecastMin, forecastMax, stage = j)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # CONVERGENCE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

    for (j in 1:length(countries)) {
      setwd(countries[j])
      print(countries[j])

      phase1 <- read.csv(list.files()[grep("Phase 1.csv", list.files())])
      phase1median <- median(phase1$forecast)
      phase1sd <- sd(phase1$forecast)
      phase1 <- phase1 %>%
        filter(forecast > phase1median - (10 * phase1sd)) %>%
        filter(forecast < phase1median + (10 * phase1sd))
      phase2 <- read.csv(list.files()[grep("Phase 2.csv", list.files())])
      phase2median <- median(phase2$forecast)
      phase2sd <- sd(phase2$forecast)
      phase2 <- phase2 %>%
        filter(forecast > phase2median - (10 * phase2sd)) %>%
        filter(forecast < phase2median + (10 * phase2sd))
      phase3 <- read.csv(list.files()[grep("Phase 3.csv", list.files())])
      phase3median <- median(phase3$forecast)
      phase3sd <- sd(phase3$forecast)
      phase3 <- phase3 %>%
        filter(forecast > phase3median - (10 * phase3sd)) %>%
        filter(forecast < phase3median + (10 * phase3sd))
      phase4 <- read.csv(list.files()[grep("Phase 4.csv", list.files())])
      phase4median <- median(phase4$forecast)
      phase4sd <- sd(phase4$forecast)
      phase4 <- phase4 %>%
        filter(forecast > phase4median - (10 * phase4sd)) %>%
        filter(forecast < phase4median + (10 * phase4sd))

      setwd(paste0(yourHome, "Summary Data"))
      convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
      convergenceTable <- rbind(convergenceTable, convergenceRow)

      write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(countries)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

      print(countries[j])

      if (dir.exists(countries[j])) {
        setwd(countries[j])
      } else {
        dir.create(countries[j])
        setwd(countries[j])
      }

      currentSetTimeSeries <- figureDataInit()
      defaultForecast <- metaTable[i, ]$defaultForecast50

      questionDataRaw <- data %>%
        filter(setName == currentSetName) %>%
        filter(answerText == countries[j]) %>%
        filter(forecast != defaultForecast)

      totalSupers <- nrow(unique(questionDataRaw %>% filter(userName %in% supers) %>% select(userName)))
      totalExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName) %>% select(userName)))
      totalDomainExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName[expertsG1$specialty1 == qSpecialty | expertsG1$specialty2 == qSpecialty | expertsG1$specialty3 == qSpecialty]) %>% select(userName)))

      for (k in 1:length(timeline)) {
        currentDate <- timeline[k]
        if (k == 1 | k %% 30 == 0) {
          print(currentDate)
        }

        dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 1)
        if (k == length(timeline)) {
          dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 2)
        }
        users <- unique(dateDataRaw$userName)
        users <- users[users %in% c(supers, expertsG1$userName, expertsG2)]

        dateDataProcessed <- data.frame(row.names = names(dateDataRaw))

        for (l in 1:length(users)) {
          user <- users[l]
          userForecasts <- dateDataRaw %>% filter(userName == user)

          mostRecentForecast <- filter(userForecasts, timestamp == max(userForecasts$timestamp))

          dateDataProcessed <- rbind(dateDataProcessed, mostRecentForecast)
        }

        currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = "", date = currentDate, qSpecialty))
      }

      write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", countries[j], ".csv"), row.names = FALSE)

      multiCountryBinaryGraphics(title = metaTable$title[i], subtitle = metaTable$subtitle[i], csv = currentSetTimeSeries, currentSetName, country = countries[j])
    }
  }

  return(newAdd)
}

pointBinary <- function(metaTable, data) {
  #' Stats and graphs for classic binary questions
  #'
  #' @description For yes/no point questions, like
  #' "Will Robin Hanson win a bet that the GPT line of language models will
  #' generate less than $1 billion in customer revenue in total by the beginning
  #' of 2025?"
  #'
  #' This function builds out a folder structure and populates it with stats and
  #' graphs.
  #'
  #' @param metaTable - metadata for all the point binary question sets
  #' @param data - data for all users on all point binary question sets
  #'
  #' @export

  newAdd <- newAddInit()

  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    write.csv(newAdd, "pointBinary_partial.csv", row.names = FALSE)

    if (dir.exists(currentSetName)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName))
    } else {
      setwd(paste0(yourHome, "Summary Data"))
      dir.create(currentSetName)
      setwd(currentSetName)
    }

    # PHASE DATA

    if (dir.exists("Phase Data")) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    } else {
      dir.create("Phase Data")
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      defaultForecast <- metaTable[i, ]$defaultForecast50

      questionDataRaw <- data %>% filter(setName == currentSetName)
      users <- unique(questionDataRaw$userName)
      # users = users[!(users %in% c(supers, expertsG1$userName, expertsG2))]
      users <- users[(users %in% c(supers, expertsG1$userName, expertsG2))]

      questionDataProcessed <- data.frame(row.names = names(supers))

      if (currentStage == 1) {
        for (k in 1:length(users)) {
          user <- users[k]
          userForecasts <- questionDataRaw %>% filter(userName == user)
          if (nrow(userForecasts) == 0) {
            print(user)
          }
          if (!any(is.na(userForecasts$stageTwoTimestamp))) {
            userForecasts <- userForecasts %>% filter(timestamp < stageTwoTimestamp)
          } else if (!any(is.na(userForecasts$stageThreeTimestamp))) {
            userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
          } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
            userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
          }
          mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
          questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
        }
      } else if (currentStage == 2) {
        for (k in 1:length(users)) {
          user <- users[k]
          userForecasts <- questionDataRaw %>% filter(userName == user)
          if (!any(is.na(userForecasts$stageThreeTimestamp))) {
            userForecasts <- userForecasts %>% filter(timestamp < stageThreeTimestamp)
          } else if (!any(is.na(userForecasts$stageFourTimestamp))) {
            userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
          }
          mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
          questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
        }
      } else if (currentStage == 3) {
        for (k in 1:length(users)) {
          user <- users[k]
          userForecasts <- questionDataRaw %>% filter(userName == user)
          if (!any(is.na(userForecasts$stageFourTimestamp))) {
            userForecasts <- userForecasts %>% filter(timestamp < stageFourTimestamp)
          }
          mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
          questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
        }
      } else if (currentStage == 4) {
        for (k in 1:length(users)) {
          user <- users[k]
          userForecasts <- questionDataRaw %>% filter(userName == user)
          mostRecentForecast <- userForecasts %>% filter(timestamp == max(timestamp))
          questionDataProcessed <- rbind(questionDataProcessed, unique(mostRecentForecast))
        }
      }

      questionDataProcessed <- questionDataProcessed %>% filter(forecast != defaultForecast)

      questionDataProcessed_anon <- questionDataProcessed %>% select(!userName)

      write.csv(questionDataProcessed, paste0(currentSetName, " - Phase ", currentStage, ".csv"), row.names = FALSE)
      write.csv(questionDataProcessed_anon, paste0(currentSetName, " - Phase ", currentStage, "_ANON.csv"), row.names = FALSE)

      if (dir.exists("Histograms")) {
        setwd("Histograms")
      } else {
        dir.create("Histograms")
        setwd("Histograms")
      }

      filenameStart <- paste0(currentSetName, " - Phase ", currentStage, " Histogram")
      histogram(questionDataProcessed, filenameStart, title = metaTable$title[i], stage = currentStage, specialty, expectedRisk, forecastMin, forecastMax)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

      newRow <- newRowInit(metaTable, questionDataProcessed, currentSetName, currentQuestionName = "", answerText = "", stage = currentStage, specialty = metaTable[i, ]$specialty)
      newAdd <- rbind(newAdd, newRow)

      setwd(paste0(yourHome, "Summary Data"))
      write.csv(newAdd, "pointBinary_partial.csv", row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/"))
    }

    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

    phases <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4")

    print(paste0("Making box plots..."))

    for (j in 1:length(phases)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

      currFiles <- list.files()
      currFile <- currFiles[grep(phases[j], currFiles)]
      currFile <- currFile[!grepl("ANON", currFile)]

      boxPlot(files = currFile, type = "regGroups", specialty, title = metaTable$title[i], subtitle = metaTable$subtitle[i], filenameStart = paste0(currentSetName, " - Stage", j), expectedRisk, forecastMin, forecastMax)
    }

    # CONVERGENCE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    print("Getting convergence data...")

    phase1 <- read.csv(list.files()[grep("Phase 1.csv", list.files())])
    phase1median <- median(phase1$forecast)
    phase1sd <- sd(phase1$forecast)
    phase1 <- phase1 %>%
      filter(forecast > phase1median - (10 * phase1sd)) %>%
      filter(forecast < phase1median + (10 * phase1sd))
    phase2 <- read.csv(list.files()[grep("Phase 2.csv", list.files())])
    phase2median <- median(phase2$forecast)
    phase2sd <- sd(phase2$forecast)
    phase2 <- phase2 %>%
      filter(forecast > phase2median - (10 * phase2sd)) %>%
      filter(forecast < phase2median + (10 * phase2sd))
    phase3 <- read.csv(list.files()[grep("Phase 3.csv", list.files())])
    phase3median <- median(phase3$forecast)
    phase3sd <- sd(phase3$forecast)
    phase3 <- phase3 %>%
      filter(forecast > phase3median - (10 * phase3sd)) %>%
      filter(forecast < phase3median + (10 * phase3sd))
    phase4 <- read.csv(list.files()[grep("Phase 4.csv", list.files())])
    phase4median <- median(phase4$forecast)
    phase4sd <- sd(phase4$forecast)
    phase4 <- phase4 %>%
      filter(forecast > phase4median - (10 * phase4sd)) %>%
      filter(forecast < phase4median + (10 * phase4sd))

    setwd(paste0(yourHome, "Summary Data"))
    convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
    convergenceTable <- rbind(convergenceTable, convergenceRow)

    write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    currentSetTimeSeries <- figureDataInit()
    defaultForecast <- metaTable[i, ]$defaultForecast50

    questionDataRaw <- data %>%
      filter(setName == currentSetName) %>%
      filter(forecast != defaultForecast)

    totalSupers <- nrow(unique(questionDataRaw %>% filter(userName %in% supers) %>% select(userName)))
    totalExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName) %>% select(userName)))
    totalDomainExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName[expertsG1$specialty1 == qSpecialty | expertsG1$specialty2 == qSpecialty | expertsG1$specialty3 == qSpecialty]) %>% select(userName)))

    for (j in 1:length(timeline)) {
      currentDate <- timeline[j]
      if (j == 1 | j %% 30 == 0) {
        print(currentDate)
      }

      dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 1)
      if (j == length(timeline)) {
        dateDataRaw <- questionDataRaw %>% filter(timestamp < currentDate + 2)
      }
      users <- unique(dateDataRaw$userName)
      users <- users[users %in% c(supers, expertsG1$userName, expertsG2)]

      dateDataProcessed <- data.frame(row.names = names(dateDataRaw))

      for (k in 1:length(users)) {
        user <- users[k]
        userForecasts <- dateDataRaw %>% filter(userName == user)

        mostRecentForecast <- filter(userForecasts, timestamp == max(userForecasts$timestamp))

        dateDataProcessed <- rbind(dateDataProcessed, mostRecentForecast)
      }

      currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = "", date = currentDate, qSpecialty))
    }

    write.csv(currentSetTimeSeries, paste0(currentSetName, ".csv"), row.names = FALSE)

    pointBinaryGraphics(title = metaTable$title[i], subtitle = metaTable$subtitle[i], csv = currentSetTimeSeries, currentSetName)
    pointBinaryVarianceGraphics(title = metaTable$title[i], subtitle = metaTable$subtitle[i], csv = currentSetTimeSeries, currentSetName)
  }

  return(newAdd)
}
