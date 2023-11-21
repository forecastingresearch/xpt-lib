library(dplyr)
library(docstring)

multiYearReciprocal_629 <- function(metaTable, data, public_supplement1, public_supplement2, public_supplement3, survey_column_matches) {
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

    # Time horizons (2030...)
    years <- metaTable[i, ] %>% select(year1, year2, year3)
    years <- as.character(years)
    years <- years[years != ""]

    # Belief sets: yours, other experts', other superforecasters'
    beliefSets <- metaTable[i, ] %>% select(yourBeliefs, expertBeliefs, superBeliefs)
    beliefSets <- as.character(beliefSets)
    beliefSets <- beliefSets[beliefSets != ""]

    # TRUE/FALSE numerate citizens flag
    numerateCitizens <- metaTable[i, ]$numerateCitizens

    # y-axis labels
    yLabel <- metaTable[i, ]$yLabels

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

          files <- c(paste0(currentSetName, " - ", currentQuestionName, " - Phase ", currentStage, ".csv"))
          filenameStart <- paste0(currentSetName, " - ", currentQuestionName, " - ", currentStage, " Box Plot")

          boxPlot(files, type = "regGroups", specialty, title = metaTable$title[i], subtitle = paste0(years[l]), filenameStart, expectedRisk, forecastMin, forecastMax, numerateCitizens, yLabel, setName = currentSetName, beliefSet = beliefSets[k], year = years[l], distrib = "")


          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[l], "/", beliefSets[k]))
        }
      }
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(beliefSets)) {
      print(beliefSets[j])

      for (k in 1:length(years)) {
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

        currentSetTimeSeries <- read.csv(paste0(currentSetName, " - ", currentQuestionName, ".csv"))

        multiYearReciprocalGraphics(title = metaTable[i, ]$title, subtitle = "", csv = currentSetTimeSeries, currentSetName)
        multiYearReciprocalVarianceGraphics(title = metaTable[i, ]$title, subtitle = "", csv = currentSetTimeSeries, currentSetName)
      }
    }
  }
}

pointDistrib_629 <- function(metaTable, data, public_supplement1, public_supplement2, public_supplement3, survey_column_matches) {
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
  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

  # For each of the risk categories (genetically engineered pathogen risk, AI catastrophe, etc.)
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    # TRUE/FALSE numerate citizens flag
    numerateCitizens <- metaTable[i, ]$numerateCitizens

    # y-axis labels
    yLabel <- metaTable[i, ]$yLabels

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(distrib)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(distrib[k])
        currentAnswerText <- distrib[k]

        setwd(distrib[k])

        files <- paste0(currentSetName, " - ", currentAnswerText, " - Phase ", currentStage, ".csv")
        filenameStart <- paste0(currentSetName, " - ", currentAnswerText, " - ", currentStage, " Box Plot")

        boxPlot(files, type = "regGroups", specialty = specialty, title = metaTable$title[i], subtitle = currentAnswerText, filenameStart, expectedRisk, forecastMin, forecastMax, numerateCitizens, yLabel, setName = currentSetName, beliefSet = "", year = "", distrib = currentAnswerText)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
      }
    }

    # FIGURE DATA

    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    qSpecialty <- metaTable[i, ]$specialty

    distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

    for (j in 1:length(distrib)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

      print(distrib[j])
      currentAnswerText <- distrib[j]

      setwd(distrib[j])

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

      currentSetTimeSeries <- read.csv(paste0(currentSetName, " - ", currentAnswerText, ".csv"))

      pointDistribGraphics(title = metaTable[i, ]$title, subtitle = "", csv = currentSetTimeSeries, currentSetName, distrib[j])
      pointDistribVarianceGraphics(title = metaTable[i, ]$title, subtitle = "", csv = currentSetTimeSeries, currentSetName, currentDistrib = distrib[j])
    }
  }
}

multiYearDistrib_629 <- function(metaTable, data, public_supplement1, public_supplement2, public_supplement3, survey_column_matches) {
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

    # TRUE/FALSE numerate citizens flag
    numerateCitizens <- metaTable[i, ]$numerateCitizens

    # y-axis labels
    yLabel <- metaTable[i, ]$yLabels

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(years)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(years[k])

        currentYear <- years[k]

        for (l in 1:length(distrib)) {
          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))

          print(distrib[l])

          currentAnswerText <- distrib[l]

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

          files <- paste0(currentSetName, " - ", currentYear, " - ", currentAnswerText, " - Phase ", currentStage, ".csv")

          filenameStart <- paste0(currentSetName, " - ", currentYear, " - ", currentAnswerText, " - Phase ", currentStage, " Box Plot")

          setwd(distrib[l])

          boxPlot(files, type = "regGroups", specialty = specialty, title = metaTable$title[i], subtitle = paste(currentYear, "-", currentAnswerText), filenameStart, expectedRisk, forecastMin, forecastMax, numerateCitizens, yLabel, setName = currentSetName, beliefSet = "", year = currentYear, distrib = currentAnswerText)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))

          ####
        }
      }
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(years)) {
      print(years[j])

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

      for (k in 1:length(distrib)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j]))

        print(distrib[k])
        currentAnswerText <- distrib[k]

        setwd(distrib[k])

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

        currentSetTimeSeries <- read.csv(paste0(currentSetName, " - ", years[j], " - ", currentAnswerText, ".csv"))

        multiYearDistribGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j], currentDistrib = distrib[k])
        multiYearDistribVarianceGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j], currentDistrib = distrib[k])
      }
    }
  }
  return(newAdd)
}

multiYearBinary_629 <- function(metaTable, data, public_supplement1, public_supplement2, public_supplement3, survey_column_matches) {
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

  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    # TRUE/FALSE numerate citizens flag
    numerateCitizens <- metaTable[i, ]$numerateCitizens

    # y-axis labels
    yLabel <- metaTable[i, ]$yLabels

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(years)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(years[k])
        currentYear <- years[k]

        setwd(currentYear)

        defaultForecast <- metaTable[i, ]$defaultForecast50

        files <- paste0(currentSetName, " - ", currentYear, " - Phase ", currentStage, ".csv")
        filenameStart <- paste0(currentSetName, " - ", currentYear, " - Phase ", currentStage, " Box Plot")

        boxPlot(files, type = "regGroups", specialty = specialty, title = metaTable$title[i], subtitle = paste(currentYear), filenameStart, expectedRisk, forecastMin, forecastMax, numerateCitizens, yLabel, setName = currentSetName, beliefSet = "", year = currentYear, distrib = "")

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

        boxPlot(files = currFile, type = "regGroups", specialty, title = metaTable$title[i], subtitle = metaTable$subtitle[i], filenameStart, expectedRisk, forecastMin, forecastMax, numerateCitizens, yLabel, setName = currentSetName, beliefSet = "", year = currentYear, distrib = "")

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(years)) {
      print(years[j])
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))
      setwd(years[j])


      currentSetTimeSeries <- read.csv(paste0(currentSetName, " - ", years[j], ".csv"))

      multiYearBinaryGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j])
      multiYearBinaryVarianceGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j])
    }
  }
}

multiYearCountryDistrib_629 <- function(metaTable, data, public_supplement1, public_supplement2, public_supplement3, survey_column_matches) {
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

    # TRUE/FALSE numerate citizens flag
    numerateCitizens <- metaTable[i, ]$numerateCitizens

    # y-axis labels
    yLabel <- metaTable[i, ]$yLabels

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(years)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(years[k])
        currentYear <- years[k]

        setwd(years[k])

        for (l in 1:length(countries)) {
          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))

          print(countries[l])
          currentCountry <- countries[l]

          setwd(countries[l])

          defaultForecast <- metaTable[i, ]$defaultForecast50

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

        setwd("BoxPlots")

        boxPlot_distrib_country(tbl = phaseTbl, specialty, title = metaTable[i, ]$title, forecastMin, forecastMax, stage = k, year = years[j])

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }
    }

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(years)) {
      print(years[j])

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))

      for (k in 1:length(countries)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j]))

        print(countries[k])

        setwd(countries[k])

        currentSetTimeSeries <- read.csv(paste0(currentSetName, " - ", years[j], ".csv"))

        multiYearCountryDistribGraphics(title = metaTable$title[i], subtitle = metaTable$subtitle[i], csv = currentSetTimeSeries, currentSetName, year = years[j], country = countries[k])
      }
    }
  }
}

multiCountryBinary_629 <- function(metaTable, data, public_supplement1, public_supplement2, public_supplement3, survey_column_matches) {
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

  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    countries <- c(metaTable$country1[i], metaTable$country2[i], metaTable$country3[i], metaTable$country4[i], metaTable$country5[i], metaTable$country6[i], metaTable$country7[i], metaTable$country8[i], metaTable$country9[i], metaTable$country10[i], metaTable$country11[i], metaTable$country12[i])
    countries <- countries[countries != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    # TRUE/FALSE numerate citizens flag
    numerateCitizens <- metaTable[i, ]$numerateCitizens

    # y-axis labels
    yLabel <- metaTable[i, ]$yLabels

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      for (k in 1:length(countries)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        print(countries[k])
        currentCountry <- countries[k]

        setwd(countries[k])

        defaultForecast <- metaTable[i, ]$defaultForecast50

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

      # boxPlot_distrib_country(tbl = phaseTbl, specialty, title = metaTable$title[i], forecastMin, forecastMax, stage = j, year="")

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }


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

      currentSetTimeSeries <- read.csv(paste0(currentSetName, " - ", countries[j], ".csv"))

      multiCountryBinaryGraphics(title = metaTable$title[i], subtitle = metaTable$subtitle[i], csv = currentSetTimeSeries, currentSetName, country = countries[j])
    }
  }
}

pointBinary_629 <- function(metaTable, data, public_supplement1, public_supplement2, public_supplement3, survey_column_matches) {
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

  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    # TRUE/FALSE numerate citizens flag
    numerateCitizens <- metaTable[i, ]$numerateCitizens

    # y-axis labels
    yLabel <- metaTable[i, ]$yLabels

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]

      defaultForecast <- metaTable[i, ]$defaultForecast50

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

      files <- paste0(currentSetName, " - Phase ", currentStage, ".csv")
      filenameStart <- paste0(currentSetName, " - Phase ", currentStage, " Box Plot")

      boxPlot(files, type = "regGroups", specialty = specialty, title = metaTable$title[i], subtitle = "", filenameStart, expectedRisk, forecastMin, forecastMax, numerateCitizens, yLabel, setName = currentSetName, beliefSet = "", year = "", distrib = "")

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }
    # # CONVERGENCE DATA
    # setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    # print("Getting convergence data...")

    # phase1 <- read.csv(list.files()[grep("Phase 1.csv", list.files())])
    # phase1median <- median(phase1$forecast)
    # phase1sd <- sd(phase1$forecast)
    # phase1 <- phase1 %>%
    #   filter(forecast > phase1median - (10 * phase1sd)) %>%
    #   filter(forecast < phase1median + (10 * phase1sd))
    # phase2 <- read.csv(list.files()[grep("Phase 2.csv", list.files())])
    # phase2median <- median(phase2$forecast)
    # phase2sd <- sd(phase2$forecast)
    # phase2 <- phase2 %>%
    #   filter(forecast > phase2median - (10 * phase2sd)) %>%
    #   filter(forecast < phase2median + (10 * phase2sd))
    # phase3 <- read.csv(list.files()[grep("Phase 3.csv", list.files())])
    # phase3median <- median(phase3$forecast)
    # phase3sd <- sd(phase3$forecast)
    # phase3 <- phase3 %>%
    #   filter(forecast > phase3median - (10 * phase3sd)) %>%
    #   filter(forecast < phase3median + (10 * phase3sd))
    # phase4 <- read.csv(list.files()[grep("Phase 4.csv", list.files())])
    # phase4median <- median(phase4$forecast)
    # phase4sd <- sd(phase4$forecast)
    # phase4 <- phase4 %>%
    #   filter(forecast > phase4median - (10 * phase4sd)) %>%
    #   filter(forecast < phase4median + (10 * phase4sd))

    # setwd(paste0(yourHome, "Summary Data"))
    # convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
    # convergenceTable <- rbind(convergenceTable, convergenceRow)

    # write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

    # FIGURE DATA
    setwd(paste0(yourHome, "Summary Data/", currentSetName))

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    currentSetTimeSeries <- read.csv(paste0(currentSetName, ".csv"))

    pointBinaryGraphics(title = metaTable$title[i], subtitle = metaTable$subtitle[i], csv = currentSetTimeSeries, currentSetName)
    pointBinaryVarianceGraphics(title = metaTable$title[i], subtitle = metaTable$subtitle[i], csv = currentSetTimeSeries, currentSetName)
  }
}
