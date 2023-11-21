library(dplyr)
library(docstring)

salienceInit <- function() {
  #' Initialize salience dataframe.
  #'
  #' @return An empty dataframe with the salience columns.

  return(data.frame(
    date = Date(0),
    g1Opt = numeric(0),
    supersOpt_g1 = numeric(0),
    expertsOpt_g1 = numeric(0),
    dExpertsOpt_g1 = numeric(0),
    ndExpertsOpt_g1 = numeric(0),
    t336Opt = numeric(0),
    t337Opt = numeric(0),
    t338Opt = numeric(0),
    t339Opt = numeric(0),
    t340Opt = numeric(0),
    t341Opt = numeric(0),
    t342Opt = numeric(0),
    t343Opt = numeric(0),
    t344Opt = numeric(0),
    t345Opt = numeric(0),
    overallOpt = numeric(0)
  ))
}

salienceCalc <- function(median, sd, g1Median, g1Sd, supersMedian,
                         dExpertsMedian, ndExpertsMedian, csv, expPosSalience) {
  #' Calculate salience.
  #'
  #' We wanted to track, over time, how much more optimistic or pessimistic
  #' supers and (g1) experts were compared to the median of the two groups
  #' combined after the independent phase of forecasting.
  #'
  #' @param median TODO
  #' @param sd -
  #' @param g1Median -
  #' @param g1Sd -
  #' @param supersMedian -
  #' @param dExpertsMedian -
  #' @param ndExpertsMedian -
  #' @param csv -
  #' @param expPosSalience -

  newTbl <- salienceInit()
  for (i in 1:length(timeline)) {
    date <- timeline[i]
    if (i == 1 | i %% 30 == 0) {
      print(date)
    }
    row <- filter(csv, currentDate == date)
    if (expPosSalience == "optimism") {
      g1Opt <- (row$g1Median - g1Median) / g1Sd
      supersOpt_g1 <- (row$supersMedian - g1Median) / g1Sd
      expertsOpt_g1 <- (row$expertsMedian - g1Median) / g1Sd
      dExpertsOpt_g1 <- (row$domainExpertsMedian - g1Median) / g1Sd
      ndExpertsOpt_g1 <- (row$nonDomainExpertsMedian - g1Median) / g1Sd
      t336Opt <- (row$t336Median - median) / sd
      t337Opt <- (row$t337Median - median) / sd
      t338Opt <- (row$t338Median - median) / sd
      t339Opt <- (row$t339Median - median) / sd
      t340Opt <- (row$t340Median - median) / sd
      t341Opt <- (row$t341Median - median) / sd
      t342Opt <- (row$t342Median - median) / sd
      t343Opt <- (row$t343Median - median) / sd
      t344Opt <- (row$t344Median - median) / sd
      t345Opt <- (row$t345Median - median) / sd
      overallOpt <- (row$median - median) / sd
    } else if (expPosSalience == "pessimism") {
      g1Opt <- (g1Median - row$g1Median) / g1Sd
      supersOpt_g1 <- (g1Median - row$supersMedian) / g1Sd
      expertsOpt_g1 <- (g1Median - row$expertsMedian) / g1Sd
      dExpertsOpt_g1 <- (g1Median - row$domainExpertsMedian) / g1Sd
      ndExpertsOpt_g1 <- (g1Median - row$nonDomainExpertsMedian) / g1Sd
      t336Opt <- (median - row$t336Median) / sd
      t337Opt <- (median - row$t337Median) / sd
      t338Opt <- (median - row$t338Median) / sd
      t339Opt <- (median - row$t339Median) / sd
      t340Opt <- (median - row$t340Median) / sd
      t341Opt <- (median - row$t341Median) / sd
      t342Opt <- (median - row$t342Median) / sd
      t343Opt <- (median - row$t343Median) / sd
      t344Opt <- (median - row$t344Median) / sd
      t345Opt <- (median - row$t345Median) / sd
      overallOpt <- (median - row$median) / sd
    }

    newTbl <- rbind(newTbl, data.frame(
      date,
      g1Opt,
      supersOpt_g1,
      expertsOpt_g1,
      dExpertsOpt_g1,
      ndExpertsOpt_g1,
      t336Opt,
      t337Opt,
      t338Opt,
      t339Opt,
      t340Opt,
      t341Opt,
      t342Opt,
      t343Opt,
      t344Opt,
      t345Opt,
      overallOpt
    ))
  }
  return(newTbl)
}

multiYearReciprocal_salience <- function(metaTable, data, summaryTable, expSalience) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    years <- metaTable[i, ] %>% select(year1, year2, year3)
    years <- as.character(years)
    years <- years[years != ""]

    beliefSets <- metaTable[i, ] %>% select(yourBeliefs, expertBeliefs, superBeliefs)
    beliefSets <- as.character(beliefSets)
    beliefSets <- beliefSets[beliefSets != ""]

    defaultForecast <- metaTable[i, ]$defaultForecast50

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    for (j in 1:length(beliefSets)) {
      # for(j in 1:1){
      print(beliefSets[j])

      for (k in 1:length(years)) {
        # for(k in length(years):length(years)){

        print(years[k])
        currentQuestionName <- paste(years[k], beliefSets[j])

        sn <- currentSetName
        qn <- currentQuestionName

        currentQSummary <- summaryTable %>%
          filter(currentSetName == sn) %>%
          filter(currentQuestionName == qn) %>%
          filter(stage == 1)

        median <- currentQSummary$median
        sd <- currentQSummary$sd
        g1Median <- currentQSummary$g1Median
        g1Sd <- currentQSummary$g1Sd
        supersMedian <- currentQSummary$supersMedian
        expertsMedian <- currentQSummary$expertsG1Median
        dExpertsMedian <- currentQSummary$domainExpertsMedian
        ndExpertsMedian <- currentQSummary$nonDomainExpertsMedian
        expPosSalience <- filter(expSalience, setName == currentSetName)
        expPosSalience <- expPosSalience$expPosSalience

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[k], "/", beliefSets[j]))

        qSpecialty <- metaTable[i, ]$specialty

        files <- list.files()
        csv <- read.csv(files[grepl(".csv", files)])

        salienceTbl <- salienceInit()
        salienceTbl <- rbind(salienceTbl, salienceCalc(median, sd, g1Median, g1Sd, supersMedian, dExpertsMedian, ndExpertsMedian, csv, expPosSalience))

        if (dir.exists("Salience")) {
          setwd("Salience")
        } else {
          dir.create("Salience")
          setwd("Salience")
        }

        filename <- paste0(currentSetName, " - ", currentQuestionName, " - SALIENCE.csv")
        write.csv(salienceTbl, filename, row.names = FALSE)

        title <- paste0(currentSetName, " - ", currentQuestionName)
        subtitle <- "Salience over Time"

        salienceGraphics(salienceTbl, title, subtitle, specialty)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[k], "/", beliefSets[j]))
      }
    }
  }
}

pointDistrib_salience <- function(metaTable, data, summaryTable, expSalience) {
  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

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

      sn <- currentSetName
      at <- currentAnswerText

      currentQSummary <- summaryTable %>%
        filter(currentSetName == sn) %>%
        filter(answerText == at) %>%
        filter(stage == 1)

      median <- currentQSummary$median
      sd <- currentQSummary$sd
      g1Median <- currentQSummary$g1Median
      g1Sd <- currentQSummary$g1Sd
      supersMedian <- currentQSummary$supersMedian
      expertsMedian <- currentQSummary$expertsG1Median
      dExpertsMedian <- currentQSummary$domainExpertsMedian
      ndExpertsMedian <- currentQSummary$nonDomainExpertsMedian
      expPosSalience <- filter(expSalience, setName == currentSetName)
      expPosSalience <- expPosSalience$expPosSalience

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", currentAnswerText))

      qSpecialty <- metaTable[i, ]$specialty

      files <- list.files()
      csv <- read.csv(files[grepl(".csv", files)])

      salienceTbl <- salienceInit()
      salienceTbl <- rbind(salienceTbl, salienceCalc(median, sd, g1Median, g1Sd, supersMedian, dExpertsMedian, ndExpertsMedian, csv, expPosSalience))

      if (dir.exists("Salience")) {
        setwd("Salience")
      } else {
        dir.create("Salience")
        setwd("Salience")
      }

      filename <- paste0(currentSetName, " - ", currentAnswerText, " - SALIENCE.csv")
      write.csv(salienceTbl, filename, row.names = FALSE)

      title <- paste0(currentSetName, " - ", currentAnswerText)
      subtitle <- "Salience over Time"

      salienceGraphics(salienceTbl, title, subtitle, specialty)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", currentAnswerText))
    }
  }
}

multiYearDistrib_salience <- function(metaTable, data, summaryTable, expSalience) {
  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

  for (i in 1:length(unique(metaTable$setName))) {
    # for(i in 19:length(unique(metaTable$setName))){
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

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

        sn <- currentSetName
        y <- years[j]
        at <- currentAnswerText

        currentQSummary <- summaryTable %>%
          filter(currentSetName == sn) %>%
          filter(currentQuestionName == y) %>%
          filter(answerText == at) %>%
          filter(stage == 1)

        median <- currentQSummary$median
        sd <- currentQSummary$sd
        g1Median <- currentQSummary$g1Median
        g1Sd <- currentQSummary$g1Sd
        supersMedian <- currentQSummary$supersMedian
        expertsMedian <- currentQSummary$expertsG1Median
        dExpertsMedian <- currentQSummary$domainExpertsMedian
        ndExpertsMedian <- currentQSummary$nonDomainExpertsMedian
        expPosSalience <- filter(expSalience, setName == currentSetName)
        expPosSalience <- expPosSalience$expPosSalience

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j], "/", currentAnswerText))

        qSpecialty <- metaTable[i, ]$specialty

        files <- list.files()
        csv <- read.csv(files[grepl(".csv", files)])

        salienceTbl <- salienceInit()
        salienceTbl <- rbind(salienceTbl, salienceCalc(median, sd, g1Median, g1Sd, supersMedian, dExpertsMedian, ndExpertsMedian, csv, expPosSalience))

        if (dir.exists("Salience")) {
          setwd("Salience")
        } else {
          dir.create("Salience")
          setwd("Salience")
        }

        filename <- paste0(currentSetName, " - ", years[j], " - ", currentAnswerText, " - SALIENCE.csv")
        write.csv(salienceTbl, filename, row.names = FALSE)

        title <- paste0(currentSetName, " - ", years[j], " - ", currentAnswerText)
        subtitle <- "Salience over Time"

        salienceGraphics(salienceTbl, title, subtitle, specialty)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j], "/", currentAnswerText))
      }
    }
  }
}

multiYearBinary_salience <- function(metaTable, data, summaryTable, expSalience) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

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

      defaultForecast <- metaTable[i, ]$defaultForecast50

      sn <- currentSetName
      y <- years[j]

      currentQSummary <- summaryTable %>%
        filter(currentSetName == sn) %>%
        filter(currentQuestionName == y) %>%
        filter(stage == 1)

      median <- currentQSummary$median
      sd <- currentQSummary$sd
      g1Median <- currentQSummary$g1Median
      g1Sd <- currentQSummary$g1Sd
      supersMedian <- currentQSummary$supersMedian
      expertsMedian <- currentQSummary$expertsG1Median
      dExpertsMedian <- currentQSummary$domainExpertsMedian
      ndExpertsMedian <- currentQSummary$nonDomainExpertsMedian
      expPosSalience <- filter(expSalience, setName == currentSetName)
      expPosSalience <- expPosSalience$expPosSalience

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j]))

      qSpecialty <- metaTable[i, ]$specialty

      files <- list.files()
      csv <- read.csv(files[grepl(".csv", files)])

      salienceTbl <- salienceInit()
      salienceTbl <- rbind(salienceTbl, salienceCalc(median, sd, g1Median, g1Sd, supersMedian, dExpertsMedian, ndExpertsMedian, csv, expPosSalience))

      if (dir.exists("Salience")) {
        setwd("Salience")
      } else {
        dir.create("Salience")
        setwd("Salience")
      }

      filename <- paste0(currentSetName, " - ", years[j], " - SALIENCE.csv")
      write.csv(salienceTbl, filename, row.names = FALSE)

      title <- paste0(currentSetName, " - ", years[j])
      subtitle <- "Salience over Time"

      salienceGraphics(salienceTbl, title, subtitle, specialty)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j]))
    }
  }
}

multiYearCountryDistrib_salience <- function(metaTable, data, summaryTable, expSalience) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    countries <- c(metaTable$country1[i], metaTable$country2[i], metaTable$country3[i], metaTable$country4[i], metaTable$country5[i], metaTable$country6[i], metaTable$country7[i], metaTable$country8[i], metaTable$country9[i], metaTable$country10[i], metaTable$country11[i], metaTable$country12[i])
    countries <- countries[countries != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

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

        defaultForecast <- metaTable[i, ]$defaultForecast50

        sn <- currentSetName
        y <- years[j]
        c <- countries[k]

        currentQSummary <- summaryTable %>%
          filter(currentSetName == sn) %>%
          filter(currentQuestionName == y) %>%
          filter(answerText == c) %>%
          filter(stage == 1)

        median <- currentQSummary$median
        sd <- currentQSummary$sd
        g1Median <- currentQSummary$g1Median
        g1Sd <- currentQSummary$g1Sd
        supersMedian <- currentQSummary$supersMedian
        expertsMedian <- currentQSummary$expertsG1Median
        dExpertsMedian <- currentQSummary$domainExpertsMedian
        ndExpertsMedian <- currentQSummary$nonDomainExpertsMedian
        expPosSalience <- filter(expSalience, setName == currentSetName)
        expPosSalience <- expPosSalience$expPosSalience

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j], "/", countries[k]))

        qSpecialty <- metaTable[i, ]$specialty

        files <- list.files()
        csv <- read.csv(files[grepl(".csv", files)])

        salienceTbl <- salienceInit()
        salienceTbl <- rbind(salienceTbl, salienceCalc(median, sd, g1Median, g1Sd, supersMedian, dExpertsMedian, ndExpertsMedian, csv, expPosSalience))

        if (dir.exists("Salience")) {
          setwd("Salience")
        } else {
          dir.create("Salience")
          setwd("Salience")
        }

        filename <- paste0(currentSetName, " - ", years[j], " - SALIENCE.csv")
        write.csv(salienceTbl, filename, row.names = FALSE)

        title <- paste0(currentSetName, " - ", years[j])
        subtitle <- "Salience over Time"

        salienceGraphics(salienceTbl, title, subtitle, specialty)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j], "/", countries[k]))
      }
    }
  }
}

multiCountryBinary_salience <- function(metaTable, data, summaryTable, expSalience) {
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

    countries <- c(metaTable$country1[i], metaTable$country2[i], metaTable$country3[i], metaTable$country4[i], metaTable$country5[i], metaTable$country6[i], metaTable$country7[i], metaTable$country8[i], metaTable$country9[i], metaTable$country10[i], metaTable$country11[i], metaTable$country12[i])
    countries <- countries[countries != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

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

      defaultForecast <- metaTable[i, ]$defaultForecast50

      sn <- currentSetName
      c <- countries[j]

      currentQSummary <- summaryTable %>%
        filter(currentSetName == sn) %>%
        filter(answerText == c) %>%
        filter(stage == 1)

      median <- currentQSummary$median
      sd <- currentQSummary$sd
      g1Median <- currentQSummary$g1Median
      g1Sd <- currentQSummary$g1Sd
      supersMedian <- currentQSummary$supersMedian
      expertsMedian <- currentQSummary$expertsG1Median
      dExpertsMedian <- currentQSummary$domainExpertsMedian
      ndExpertsMedian <- currentQSummary$nonDomainExpertsMedian
      expPosSalience <- filter(expSalience, setName == currentSetName)
      expPosSalience <- expPosSalience$expPosSalience

      if (expPosSalience != "other") {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", countries[j]))

        qSpecialty <- metaTable[i, ]$specialty

        files <- list.files()
        csv <- read.csv(files[grepl(".csv", files)])

        salienceTbl <- salienceInit()
        salienceTbl <- rbind(salienceTbl, salienceCalc(median, sd, g1Median, g1Sd, supersMedian, dExpertsMedian, ndExpertsMedian, csv, expPosSalience))

        if (dir.exists("Salience")) {
          setwd("Salience")
        } else {
          dir.create("Salience")
          setwd("Salience")
        }

        filename <- paste0(currentSetName, " - ", years[j], " - SALIENCE.csv")
        write.csv(salienceTbl, filename, row.names = FALSE)

        title <- paste0(currentSetName, " - ", years[j])
        subtitle <- "Salience over Time"

        salienceGraphics(salienceTbl, title, subtitle, specialty)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", countries[j]))
      }
    }
  }
}

pointBinary_salience <- function(metaTable, data, summaryTable, expSalience) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    defaultForecast <- metaTable[i, ]$defaultForecast50

    sn <- currentSetName

    currentQSummary <- summaryTable %>%
      filter(currentSetName == sn) %>%
      filter(stage == 1)

    median <- currentQSummary$median
    sd <- currentQSummary$sd
    g1Median <- currentQSummary$g1Median
    g1Sd <- currentQSummary$g1Sd
    supersMedian <- currentQSummary$supersMedian
    expertsMedian <- currentQSummary$expertsG1Median
    dExpertsMedian <- currentQSummary$domainExpertsMedian
    ndExpertsMedian <- currentQSummary$nonDomainExpertsMedian
    expPosSalience <- filter(expSalience, setName == currentSetName)
    expPosSalience <- expPosSalience$expPosSalience

    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

    qSpecialty <- metaTable[i, ]$specialty

    files <- list.files()
    csv <- read.csv(files[grepl(".csv", files)])

    salienceTbl <- salienceInit()
    salienceTbl <- rbind(salienceTbl, salienceCalc(median, sd, g1Median, g1Sd, supersMedian, dExpertsMedian, ndExpertsMedian, csv, expPosSalience))

    if (dir.exists("Salience")) {
      setwd("Salience")
    } else {
      dir.create("Salience")
      setwd("Salience")
    }

    filename <- paste0(currentSetName, " - SALIENCE.csv")
    write.csv(salienceTbl, filename, row.names = FALSE)

    title <- paste0(currentSetName)
    subtitle <- "Salience over Time"

    salienceGraphics(salienceTbl, title, subtitle, specialty)

    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))
  }
}
