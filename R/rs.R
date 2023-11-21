library(dplyr)
library(docstring)

RSInit <- function() {
  #' Initialize reciprocal scoring dataframe.
  #'
  #' @return An empty dataframe with the reciprocal scoring columns.
  #'
  #' @export

  return(data.frame(
    currentSetName = character(0),
    currentQuestionName = character(0),
    answerText = character(0),
    stage = character(0),
    specialty = character(0),
    g1Mean_unincentivized = character(0),
    supersMean_unincentivized = character(0),
    expertsG1Mean_unincentivized = character(0),
    expertsG2Mean_unincentivized = character(0),
    domainExpertsMean_unincentivized = character(0),
    nonDomainExpertsMean_unincentivized = character(0),
    g1Mean_RS = character(0),
    supersMean_RS = character(0),
    expertsG1Mean_RS = character(0),
    expertsG2Mean_RS = character(0),
    domainExpertsMean_RS = character(0),
    nonDomainExpertsMean_RS = character(0)
  ))
}

RSRankingInit <- function() {
  #' Initialize reciprocal scoring ranking dataframe.
  #'
  #' @return An empty dataframe that will hold the reciprocal scoring ranking
  #' for each user.
  #'
  #' @export

  return(data.frame(
    userId = numeric(0),
    group = character(0),
    rankSum = numeric(0),
    numQuestions = numeric(0)
  ))
}

log_score <- function(actual, forecast) {
  #' @export
  
  return(log(1 - abs(actual / 100 - forecast / 100)))
}

multiYearReciprocal_RS <- function(metaTable, data, summaryTable) {
  #' Scores the multi-year reciprocal questions
  #'
  #' @description This scores everyone who answered the reciprocal questions
  #' (What do you believe? Ok, what do you think the [other supers, experts]
  #' believe?). They were asked about the MEDIAN belief of two groups.
  #'
  #' Scoring rule:
  #' `log(1 - abs(actual median - what you thought the median would be))`
  #'
  #' +------------+-----------------------+-------------------------------------+ # nolint
  #' |     x      |       A. Actual       | B. What _ believes Group X will say | # nolint
  #' +------------+-----------------------+-------------------------------------+ # nolint
  #' | 1. Yours   | Your true belief      | Your belief about Group X           | # nolint
  #' | 2. Group X | True Group X (median) | What Group X believes about itself  | # nolint
  #' +------------+-----------------------+-------------------------------------+ # nolint
  #'
  #' This produces two kinds of scores:
  #' score_unincentivized compares A2 to B1, and
  #' score_rs compares B2 to B1
  #'
  #' @param metaTable Metadata on the multi-year reciprocal questions
  #' @param data Data on the multi-year reciprocal questions (forecasts)
  #' @param summaryTable TODO
  #'
  #' @export

  # For each of the question sets (1. Genetically Engineered Pathogens, etc.)
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    # Create the directory for the question set if it doesn't exist
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

    years <- metaTable[i, ] %>% select(year1, year2, year3)
    years <- as.character(years)
    years <- years[years != ""]

    beliefSets <- metaTable[i, ] %>% select(yourBeliefs, expertBeliefs,
                                            superBeliefs)
    beliefSets <- as.character(beliefSets)
    beliefSets <- beliefSets[beliefSets != ""]

    defaultForecast <- metaTable[i, ]$defaultForecast50

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

    for (j in 1:length(unique(metaTable$stage))) {
      print(paste("Stage:", (unique(metaTable$stage)[j])))
      currentStage <- unique(metaTable$stage)[j]
      # for(k in 1:length(beliefSets)){
      for (k in 1:length(years)) {
        # for(l in length(years):length(years)){
        print(years[k])

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))

        sn <- currentSetName

        otherBeliefSets <- beliefSets[!grepl("Your", beliefSets)]

        medianBeliefs <- summaryTable %>%
          filter(currentSetName == sn) %>%
          filter(currentQuestionName == paste(years[k], beliefSets[grepl(
            "Your",
            beliefSets
          )])) %>%
          filter(stage == j) %>%
          select(
            currentSetName, currentQuestionName, answerText, stage,
            specialty, supersMedian, expertsG1Median, domainExpertsMedian
          )

        if (specialty == "") {
          medianBeliefs$domainExpertsMedian <- medianBeliefs$expertsG1Median
        }

        for (l in 1:length(otherBeliefSets)) {
          print(otherBeliefSets[l])
          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k], "/", otherBeliefSets[l]))
          files <- list.files()
          phase_csv <- read.csv(files[grepl(paste0("Phase ", j, ".csv"), files)])

          if (grepl("Experts", otherBeliefSets[l])) {
            phase_csv <- phase_csv %>%
              mutate(score_unincentivized = log_score(medianBeliefs$domainExpertsMedian, forecast))
          } else if (grepl("Superforecasters", otherBeliefSets[l])) {
            phase_csv <- phase_csv %>%
              mutate(score_unincentivized = log_score(medianBeliefs$supersMedian, forecast))
          } else {
            print("ERROR!!!")
          }

          if (nrow(phase_csv[is.nan(phase_csv$score_unincentivized), ]) > 0) {
            phase_csv[is.nan(phase_csv$score_unincentivized), ]$score_unincentivized <- -Inf
          }

          RSBeliefs <- summaryTable %>%
            filter(currentSetName == sn) %>%
            filter(currentQuestionName == paste(years[k], otherBeliefSets[l])) %>%
            filter(stage == j) %>%
            select(currentSetName, currentQuestionName, answerText, stage,
                   specialty, supersMedian, expertsG1Median,
                   domainExpertsMedian)

          if (specialty == "") {
            RSBeliefs$domainExpertsMedian <- RSBeliefs$expertsG1Median
          }

          if (grepl("Experts", otherBeliefSets[l])) {
            phase_csv <- phase_csv %>%
              mutate(score_rs = log_score(RSBeliefs$domainExpertsMedian, forecast))
          } else if (grepl("Superforecasters", otherBeliefSets[l])) {
            phase_csv <- phase_csv %>%
              mutate(score_rs = log_score(RSBeliefs$supersMedian, forecast))
          } else {
            print("ERROR!!!")
          }

          if (nrow(phase_csv[is.nan(phase_csv$score_rs), ]) > 0) {
            phase_csv[is.nan(phase_csv$score_rs), ]$score_rs <- -Inf
          }

          if (dir.exists("Reciprocal Scoring")) {
            setwd("Reciprocal Scoring")
          } else {
            dir.create("Reciprocal Scoring")
            setwd("Reciprocal Scoring")
          }

          phase_csv <- mutate(phase_csv, group = "")

          for (m in 1:nrow(phase_csv)) {
            if (phase_csv$userName[m] %in% supers) {
              phase_csv$group[m] <- "supers"
            } else if (phase_csv$userName[m] %in% expertsG1$userName) {
              if (specialty != "" & specialty %in% unlist(expertsG1 %>% filter(userId == phase_csv$userId[m]) %>% select(specialty1, specialty2, specialty3))) {
                phase_csv$group[m] <- "domain experts"
              } else {
                phase_csv$group[m] <- "non-domain experts"
              }
            } else if (phase_csv$userName[m] %in% expertsG2) {
              phase_csv$group[m] <- "experts (g2)"
            }
          }

          # phase_csv = phase_csv %>% arrange(forecast)
          # forecasts_5th = ceiling(nrow(phase_csv)*0.05)
          # forecasts_95th = floor(nrow(phase_csv)*0.95)

          # standard_subset = phase_csv[forecasts_5th:forecasts_95th,]
          # standard_mean_unincentivized = mean(standard_subset$score_unincentivized)
          # standard_sd_unincentivized = sd(standard_subset$score_unincentivized)
          # standard_mean_rs = mean(standard_subset$score_rs)
          # standard_sd_rs = sd(standard_subset$score_rs)

          # phase_csv = phase_csv %>% mutate(score_unincentivized_standardized = (score_unincentivized - 0)/(standard_sd_unincentivized))
          # phase_csv = phase_csv %>% mutate(score_rs_standardized = (score_rs - 0)/(standard_sd_rs))

          phase_csv <- phase_csv %>%
            mutate(rank_unincentivized = rank(-score_unincentivized, ties.method = "min", na.last = "keep"))
          phase_csv <- phase_csv %>%
            mutate(rank_rs = rank(-score_rs, ties.method = "min", na.last = "keep"))

          # This is the first csv we write: everyone's score on each question?
          csv <- select(phase_csv, userId, teamId, setName, setId,
                        questionName, questionId, answerText, answerId,
                        score_unincentivized, score_rs, rank_unincentivized,
                        rank_rs, group)
          csv <- unique(csv)

          write.csv(csv, paste0(currentSetName, " - ", csv$questionName[1],
                                " - Reciprocal Scores.csv"), row.names = FALSE)

          setwd(paste0(yourHome, "Summary Data"))

          # Group 1 (supers and "G1" experts)
          g1RS <- csv %>%
            filter(group %in% c("supers", "domain experts",
                                "non-domain experts"))
          # Supers by themselves
          supersRS <- csv %>% filter(group == "supers")
          # G1 experts by themselves
          expertsG1RS <- csv %>%
            filter(group %in% c("domain experts", "non-domain experts"))
          # G2 experts by themselves
          expertsG2RS <- csv %>% filter(group == "experts (g2)")
          # Domain experts (varies by question)
          domainExpertsRS <- csv %>% filter(group == "domain experts")
          # Non-domain experts (varies by question)
          nonDomainExpertsRS <- csv %>% filter(group == "non-domain experts")

          RSTable <- read.csv("RSTable.csv")
          RSTable <- rbind(RSTable, data.frame(
            currentSetName = currentSetName,
            currentQuestionName = csv$questionName[1],
            answerText = csv$answerText[1],
            stage = j,
            specialty = specialty,
            g1Mean_unincentivized = mean(g1RS$score_unincentivized),
            supersMean_unincentivized = mean(supersRS$score_unincentivized),
            expertsG1Mean_unincentivized = mean(expertsG1RS$score_unincentivized),
            expertsG2Mean_unincentivized = mean(expertsG2RS$score_unincentivized),
            domainExpertsMean_unincentivized = mean(domainExpertsRS$score_unincentivized),
            nonDomainExpertsMean_unincentivized = mean(nonDomainExpertsRS$score_unincentivized),
            g1Mean_RS = mean(g1RS$score_rs),
            supersMean_RS = mean(supersRS$score_rs),
            expertsG1Mean_RS = mean(expertsG1RS$score_rs),
            expertsG2Mean_RS = mean(expertsG2RS$score_rs),
            domainExpertsMean_RS = mean(domainExpertsRS$score_rs),
            nonDomainExpertsMean_RS = mean(nonDomainExpertsRS$score_rs)
          ))

          # For each question, mean unincentivized and RS scores for each group
          write.csv(RSTable, "RSTable.csv", row.names = FALSE)

          RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")

          g1RS <- g1RS %>%
            mutate(g1Rank_unincentivized = rank(-score_unincentivized, ties.method = "min", na.last = "keep")) %>%
            mutate(g1Rank_RS = rank(-score_rs, ties.method = "min", na.last = "keep"))

          # We only want stage 1
          if (j == 1) {
            for (m in 1:nrow(g1RS)) {
              if (g1RS$userId[m] %in% RSRanking_unincentivized$userId) {
                increment <- RSRanking_unincentivized[RSRanking_unincentivized$userId == g1RS$userId[m], ]$numQuestions + 1
                RSRanking_unincentivized[RSRanking_unincentivized$userId == g1RS$userId[m], ]$rankSum <- RSRanking_unincentivized[RSRanking_unincentivized$userId == g1RS$userId[m], ]$rankSum + g1RS[g1RS$userId == g1RS$userId[m], ]$g1Rank_unincentivized
                # RSRanking_unincentivized[RSRanking_unincentivized$userId==g1RS$userId[m],]$rankSum = RSRanking_unincentivized[RSRanking_unincentivized$userId==g1RS$userId[m],]$rankSum+g1RS[g1RS$userId==g1RS$userId[m],]$g1Rank_RS
                RSRanking_unincentivized[RSRanking_unincentivized$userId == g1RS$userId[m], ]$numQuestions <- increment
              } else { # initializing (this is the first time we've seen this user)
                RSRanking_unincentivized <- rbind(RSRanking_unincentivized, data.frame(
                  userId = g1RS$userId[m],
                  group = g1RS$group[m],
                  rankSum = g1RS$g1Rank_unincentivized[m],
                  numQuestions = 1
                ))
              }
            }
          }

          # Write the G1 rankings unincentivized RS rankings to CSV
          write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k], "/", otherBeliefSets[l]))
        }

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))
      }
    }
  }
  setwd(paste0(yourHome, "Summary Data"))
  RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")
  # Average ranking means we should expect the highest (and lowest) ranked will
  # be people who answered very few questions
  RSRanking_unincentivized <- RSRanking_unincentivized %>%
    mutate(avgRank = rankSum / numQuestions) %>%
    arrange(avgRank)
  write.csv(RSRanking_unincentivized, "RSRanking_unincentivized_first10.csv", row.names = FALSE)
}

pointDistrib_RS <- function(metaTable, data, summaryTable) {
  #' Scores the point distribution reciprocal questions
  #'
  #' @description This scores everyone who answered the reciprocal questions
  #' (What do you believe? Ok, what do you think the [other supers, experts]
  #' believe?). They were asked about the MEDIAN belief of two groups, for
  #' each quintile.
  #'
  #' Scoring rule:
  #' `log(1 - abs(action median - what you thought the median would be))`
  #'
  #' @param metaTable Metadata on the point distrib reciprocal questions
  #' @param data Data on the point distrib reciprocal questions (forecasts)
  #' @param summaryTable TODO
  #'
  #' @note Were there any PD RS questions?
  #'
  #' @export

  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")
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

      quantiles <- c("5th", "25th", "50th", "75th", "95th")

      for (k in 1:length(distrib)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

        setwd(distrib[k])

        files <- list.files()
        assign(paste0("csv_", quantiles[k]), read.csv(files[grepl(paste0("Phase ", j, ".csv"), files)]))

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
      }

      users <- unique(c(csv_5th$userName, csv_25th$userName, csv_50th$userName, csv_75th$userName, csv_95th$userName))
      users <- users[users %in% csv_5th$userName & users %in% csv_25th$userName & users %in% csv_50th$userName & users %in% csv_75th$userName & users %in% csv_95th$userName]

      sn <- currentSetName

      summary_5th <- summaryTable %>%
        filter(currentSetName == sn) %>%
        filter(stage == j) %>%
        filter(answerText == "5th %")
      summary_25th <- summaryTable %>%
        filter(currentSetName == sn) %>%
        filter(stage == j) %>%
        filter(answerText == "25th %")
      summary_50th <- summaryTable %>%
        filter(currentSetName == sn) %>%
        filter(stage == j) %>%
        filter(answerText == "50th %")
      summary_75th <- summaryTable %>%
        filter(currentSetName == sn) %>%
        filter(stage == j) %>%
        filter(answerText == "75th %")
      summary_95th <- summaryTable %>%
        filter(currentSetName == sn) %>%
        filter(stage == j) %>%
        filter(answerText == "95th %")

      super_5th <- summary_5th$supersMedian
      super_25th <- summary_25th$supersMedian
      super_50th <- summary_50th$supersMedian
      super_75th <- summary_75th$supersMedian
      super_95th <- summary_95th$supersMedian

      if (specialty != "") {
        expert_5th <- summary_5th$domainExpertsMedian
        expert_25th <- summary_25th$domainExpertsMedian
        expert_50th <- summary_50th$domainExpertsMedian
        expert_75th <- summary_75th$domainExpertsMedian
        expert_95th <- summary_95th$domainExpertsMedian
      } else {
        expert_5th <- summary_5th$expertsG1Median
        expert_25th <- summary_25th$expertsG1Median
        expert_50th <- summary_50th$expertsG1Median
        expert_75th <- summary_75th$expertsG1Median
        expert_95th <- summary_95th$expertsG1Median
      }

      scoreTbl <- data.frame(
        userId = numeric(0),
        teamId = numeric(0),
        setName = character(0),
        setId = numeric(0),
        questionName = character(0),
        questionId = numeric(0),
        answerText = character(0),
        answerId = numeric(0),
        score_unincentivized = numeric(0),
        score_RS = numeric(0),
        rank_unincentivized = numeric(0),
        rank_rs = numeric(0),
        group = character(0)
      )

      for (k in 1:length(users)) {
        group <- ""
        user5th <- csv_5th %>% filter(userName == users[k])
        user25th <- csv_25th %>% filter(userName == users[k])
        user50th <- csv_50th %>% filter(userName == users[k])
        user75th <- csv_75th %>% filter(userName == users[k])
        user95th <- csv_95th %>% filter(userName == users[k])

        if (users[k] %in% supers) {
          group <- "supers"
        } else if (users[k] %in% expertsG1$userName) {
          if (specialty != "") {
            if (expertsG1[expertsG1$userName == users[k], ]$specialty == specialty) {
              group <- "domain experts"
            } else {
              group <- "non-domain experts"
            }
          } else {
            group <- "experts"
          }
        } else if (users[k] %in% expertsG2) {
          group <- "experts (g2)"
        }

        if (group == "supers") {
          score_unincentivized <- 0
          score_unincentivized <- score_unincentivized + (super_5th - user5th$forecast)^2
          score_unincentivized <- score_unincentivized + (super_25th - user25th$forecast)^2
          score_unincentivized <- score_unincentivized + (super_50th - user50th$forecast)^2
          score_unincentivized <- score_unincentivized + (super_75th - user75th$forecast)^2
          score_unincentivized <- score_unincentivized + (super_95th - user95th$forecast)^2
          score_unincentivized <- score_unincentivized / 5
        } else if (group %in% c("experts", "domain experts", "non-domain experts")) {
          score_unincentivized <- 0
          score_unincentivized <- score_unincentivized + (expert_5th - user5th$forecast)^2
          score_unincentivized <- score_unincentivized + (expert_25th - user25th$forecast)^2
          score_unincentivized <- score_unincentivized + (expert_50th - user50th$forecast)^2
          score_unincentivized <- score_unincentivized + (expert_75th - user75th$forecast)^2
          score_unincentivized <- score_unincentivized + (expert_95th - user95th$forecast)^2
          score_unincentivized <- score_unincentivized / 5
        } else {
          score_unincentivized <- NA
        }

        scoreTbl <- rbind(scoreTbl, data.frame(
          userId = csv_5th[csv_5th$userName == users[k], ]$userId,
          teamId = csv_5th[csv_5th$userName == users[k], ]$teamId,
          setName = csv_5th[csv_5th$userName == users[k], ]$setName,
          setId = csv_5th[csv_5th$userName == users[k], ]$setId,
          questionName = csv_5th[csv_5th$userName == users[k], ]$questionName,
          questionId = csv_5th[csv_5th$userName == users[k], ]$questionId,
          answerText = "",
          answerId = "",
          score_unincentivized = score_unincentivized,
          score_RS = NA,
          rank_unincentivized = NA,
          rank_RS = NA,
          group = group
        ))
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

      scoreTbl$rank_unincentivized <- rank(-scoreTbl$score_unincentivized,
                                           ties.method = "min", na.last = "keep")

      if (dir.exists("Reciprocal Scoring")) {
        setwd("Reciprocal Scoring")
      } else {
        dir.create("Reciprocal Scoring")
        setwd("Reciprocal Scoring")
      }

      write.csv(scoreTbl, paste0(currentSetName,
                                 " - Reciprocal Scores.csv"), row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data"))

      RSTable <- read.csv("RSTable.csv")

      g1Mean <- mean((scoreTbl %>%
        filter(group %in% c("supers", "domain experts", "non-domain experts", "experts")) %>%
        select(score_unincentivized))$score_unincentivized)
      supersMean <- mean((scoreTbl %>% filter(group == "supers") %>%
        select(score_unincentivized))$score_unincentivized)
      expertsG1Mean <- mean((scoreTbl %>%
        filter(group %in% c("domain experts", "non-domain experts", "experts")) %>%
        select(score_unincentivized))$score_unincentivized)
      expertsG2Mean <- mean((scoreTbl %>%
        filter(group == "experts (g2)") %>%
        select(score_unincentivized))$score_unincentivized)
      domainExpertsMean <- mean((scoreTbl %>%
        filter(group == "domain experts") %>%
        select(score_unincentivized))$score_unincentivized)
      nonDomainExpertsMean <- mean((scoreTbl %>%
        filter(group == "non-domain experts") %>%
        select(score_unincentivized))$score_unincentivized)

      RSTable <- rbind(RSTable, data.frame(
        currentSetName = currentSetName,
        currentQuestionName = "",
        answerText = "",
        stage = j,
        specialty = specialty,
        g1Mean_unincentivized = g1Mean,
        supersMean_unincentivized = supersMean,
        expertsG1Mean_unincentivized = expertsG1Mean,
        expertsG2Mean_unincentivized = expertsG2Mean,
        domainExpertsMean_unincentivized = domainExpertsMean,
        nonDomainExpertsMean_unincentivized = nonDomainExpertsMean,
        g1Mean_RS = NA,
        supersMean_RS = NA,
        expertsG1Mean_RS = NA,
        expertsG2Mean_RS = NA,
        domainExpertsMean_RS = NA,
        nonDomainExpertsMean_RS = NA
      ))

      write.csv(RSTable, "RSTable.csv", row.names = FALSE)

      RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")

      if (grepl("Expert", otherBeliefSets[l])) {
        if (j == 1) {
          for (m in 1:nrow(scoreTbl)) {
            if (scoreTbl$userId[m] %in% RSRanking_unincentivized$userId) {
              pop <- RSRanking_unincentivized[RSRanking_unincentivized$userId == scoreTbl$userId[m], ]$numQuestions + 1
              RSRanking_unincentivized[RSRanking_unincentivized$userId == scoreTbl$userId[m], ]$rankSum <- RSRanking_unincentivized[RSRanking_unincentivized$userId == scoreTbl$userId[m], ]$rankSum + scoreTbl[scoreTbl$userId == scoreTbl$userId[m], ]$rank_unincentivized
              # RSRanking_unincentivized[RSRanking_unincentivized$userId==scoreTbl$userId[m],]$rankSum = RSRanking_unincentivized[RSRanking_unincentivized$userId==scoreTbl$userId[m],]$rankSum+scoreTbl[scoreTbl$userId==scoreTbl$userId[m],]$rank_rs
              RSRanking_unincentivized[RSRanking_unincentivized$userId == scoreTbl$userId[m], ]$n <- pop
            } else {
              RSRanking_unincentivized <- rbind(RSRanking_unincentivized, data.frame(
                userId = scoreTbl$userId[m],
                group = scoreTbl$group[m],
                rankSum = scoreTbl$rank_unincentivized[m],
                # rankSum = scoreTbl$rank_rs[m],
                numQuestions = 1
              ))
            }
          }
        }
      }

      write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }
  }
}

multiYearDistrib_RS <- function(metaTable, data, summaryTable) {
  #' Scores the multi-year distribution reciprocal questions
  #'
  #' @description This scores everyone who answered the multi-year distribution
  #' reciprocal questions (What do you believe? Ok, what do you think the [other
  #' supers, experts] believe?). They were asked about the MEDIAN belief of two
  #' groups, for each year and quintile combination.
  #'
  #' Scoring rule:
  #' `log(1 - abs(action median - what you thought the median would be))`
  #'
  #' @param metaTable Metadata on the multi-year distribution reciprocal questions
  #' @param data Data on the multi-year distribution reciprocal questions (forecasts)
  #' @param summaryTable TODO
  #'
  #' @note Were there any of these questions?
  #'
  #' @export

  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

  for (i in 1:length(unique(metaTable$setName))) {
    # for(i in 19:length(unique(metaTable$setName))){
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
        if (years[k] > 2030) {
          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

          print(years[k])

          currentYear <- years[k]

          if (dir.exists(years[k])) {
            setwd(years[k])
          } else {
            dir.create(years[k])
            setwd(years[k])
          }

          quantiles <- c("5th", "25th", "50th", "75th", "95th")

          for (l in 1:length(distrib)) {
            setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))

            setwd(distrib[l])

            files <- list.files()
            assign(paste0("csv_", quantiles[l]), read.csv(files[grepl(paste0("Phase ", j, ".csv"), files)]))

            setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))
          }

          users <- unique(c(csv_5th$userName, csv_25th$userName, csv_50th$userName, csv_75th$userName, csv_95th$userName))
          users <- users[users %in% csv_5th$userName & users %in% csv_25th$userName & users %in% csv_50th$userName & users %in% csv_75th$userName & users %in% csv_95th$userName]

          sn <- currentSetName
          qn <- years[k]

          summary_5th <- summaryTable %>%
            filter(currentSetName == sn) %>%
            filter(stage == j) %>%
            filter(currentQuestionName == qn) %>%
            filter(answerText == "5th %")
          summary_25th <- summaryTable %>%
            filter(currentSetName == sn) %>%
            filter(stage == j) %>%
            filter(currentQuestionName == qn) %>%
            filter(answerText == "25th %")
          summary_50th <- summaryTable %>%
            filter(currentSetName == sn) %>%
            filter(stage == j) %>%
            filter(currentQuestionName == qn) %>%
            filter(answerText == "50th %")
          summary_75th <- summaryTable %>%
            filter(currentSetName == sn) %>%
            filter(stage == j) %>%
            filter(currentQuestionName == qn) %>%
            filter(answerText == "75th %")
          summary_95th <- summaryTable %>%
            filter(currentSetName == sn) %>%
            filter(stage == j) %>%
            filter(currentQuestionName == qn) %>%
            filter(answerText == "95th %")

          super_5th <- summary_5th$supersMedian
          super_25th <- summary_25th$supersMedian
          super_50th <- summary_50th$supersMedian
          super_75th <- summary_75th$supersMedian
          super_95th <- summary_95th$supersMedian

          if (specialty != "") {
            expert_5th <- summary_5th$domainExpertsMedian
            expert_25th <- summary_25th$domainExpertsMedian
            expert_50th <- summary_50th$domainExpertsMedian
            expert_75th <- summary_75th$domainExpertsMedian
            expert_95th <- summary_95th$domainExpertsMedian
          } else {
            expert_5th <- summary_5th$expertsG1Median
            expert_25th <- summary_25th$expertsG1Median
            expert_50th <- summary_50th$expertsG1Median
            expert_75th <- summary_75th$expertsG1Median
            expert_95th <- summary_95th$expertsG1Median
          }

          scoreTbl <- data.frame(
            userId = numeric(0),
            teamName = character(0),
            teamId = numeric(0),
            setName = character(0),
            setId = numeric(0),
            questionName = character(0),
            questionId = numeric(0),
            answerText = character(0),
            answerId = numeric(0),
            score_unincentivized = numeric(0),
            score_RS = numeric(0),
            rank_unincentivized = numeric(0),
            rank_rs = numeric(0),
            group = character(0)
          )

          for (l in 1:length(users)) {
            group <- ""
            user5th <- csv_5th %>% filter(userName == users[l])
            user25th <- csv_25th %>% filter(userName == users[l])
            user50th <- csv_50th %>% filter(userName == users[l])
            user75th <- csv_75th %>% filter(userName == users[l])
            user95th <- csv_95th %>% filter(userName == users[l])

            if (users[l] %in% supers) {
              group <- "supers"
            } else if (users[l] %in% expertsG1$userName) {
              if (specialty != "") {
                if (expertsG1[expertsG1$userName == users[l], ]$specialty == specialty) {
                  group <- "domain experts"
                } else {
                  group <- "non-domain experts"
                }
              } else {
                group <- "experts"
              }
            } else if (users[l] %in% expertsG2) {
              group <- "experts (g2)"
            }

            if (group == "supers") {
              score_unincentivized <- 0
              score_unincentivized <- score_unincentivized + (super_5th - user5th$forecast)^2
              score_unincentivized <- score_unincentivized + (super_25th - user25th$forecast)^2
              score_unincentivized <- score_unincentivized + (super_50th - user50th$forecast)^2
              score_unincentivized <- score_unincentivized + (super_75th - user75th$forecast)^2
              score_unincentivized <- score_unincentivized + (super_95th - user95th$forecast)^2
              score_unincentivized <- score_unincentivized / 5
            } else if (group %in% c("experts", "domain experts", "non-domain experts")) {
              score_unincentivized <- 0
              score_unincentivized <- score_unincentivized + (expert_5th - user5th$forecast)^2
              score_unincentivized <- score_unincentivized + (expert_25th - user25th$forecast)^2
              score_unincentivized <- score_unincentivized + (expert_50th - user50th$forecast)^2
              score_unincentivized <- score_unincentivized + (expert_75th - user75th$forecast)^2
              score_unincentivized <- score_unincentivized + (expert_95th - user95th$forecast)^2
              score_unincentivized <- score_unincentivized / 5
            } else {
              score_unincentivized <- NA
            }

            scoreTbl <- rbind(scoreTbl, data.frame(
              userId = csv_5th[csv_5th$userName == users[l], ]$userId,
              teamName = csv_5th[csv_5th$userName == users[l], ]$teamName,
              teamId = csv_5th[csv_5th$userName == users[l], ]$teamId,
              setName = csv_5th[csv_5th$userName == users[l], ]$setName,
              setId = csv_5th[csv_5th$userName == users[l], ]$setId,
              questionName = csv_5th[csv_5th$userName == users[l], ]$questionName,
              questionId = csv_5th[csv_5th$userName == users[l], ]$questionId,
              answerText = "",
              answerId = "",
              score_unincentivized = score_unincentivized,
              score_RS = NA,
              rank_unincentivized = NA,
              rank_rs = NA,
              group = group
            ))
          }

          # Rank in reverse order (lower score better)
          scoreTbl$rank_unincentivized <- rank(-scoreTbl$score_unincentivized, ties.method = "min", na.last = "keep")

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))

          if (dir.exists("Reciprocal Scoring")) {
            setwd("Reciprocal Scoring")
          } else {
            dir.create("Reciprocal Scoring")
            setwd("Reciprocal Scoring")
          }

          scoreTbl <- unique(scoreTbl)

          write.csv(scoreTbl, paste0(currentSetName,
                                     " - Reciprocal Scores.csv"), row.names = FALSE)

          setwd(paste0(yourHome, "Summary Data"))

          RSTable <- read.csv("RSTable.csv")

          g1Mean <- mean((scoreTbl %>%
            filter(group %in% c("supers", "domain experts", "non-domain experts", "experts")) %>%
            select(score_unincentivized))$score_unincentivized)
          supersMean <- mean((scoreTbl %>% filter(group == "supers") %>%
            select(score_unincentivized))$score_unincentivized)
          expertsG1Mean <- mean((scoreTbl %>%
            filter(group %in% c("domain experts", "non-domain experts", "experts")) %>%
            select(score_unincentivized))$score_unincentivized)
          expertsG2Mean <- mean((scoreTbl %>%
            filter(group == "experts (g2)") %>%
            select(score_unincentivized))$score_unincentivized)
          domainExpertsMean <- mean((scoreTbl %>%
            filter(group == "domain experts") %>%
            select(score_unincentivized))$score_unincentivized)
          nonDomainExpertsMean <- mean((scoreTbl %>%
            filter(group == "non-domain experts") %>%
            select(score_unincentivized))$score_unincentivized)

          RSTable <- rbind(RSTable, data.frame(
            currentSetName = currentSetName,
            currentQuestionName = "",
            answerText = "",
            stage = j,
            specialty = specialty,
            g1Mean_unincentivized = g1Mean,
            supersMean_unincentivized = supersMean,
            expertsG1Mean_unincentivized = expertsG1Mean,
            expertsG2Mean_unincentivized = expertsG2Mean,
            domainExpertsMean_unincentivized = domainExpertsMean,
            nonDomainExpertsMean_unincentivized = nonDomainExpertsMean,
            g1Mean_RS = NA,
            supersMean_RS = NA,
            expertsG1Mean_RS = NA,
            expertsG2Mean_RS = NA,
            domainExpertsMean_RS = NA,
            nonDomainExpertsMean_RS = NA
          ))

          write.csv(RSTable, "RSTable.csv", row.names = FALSE)

          RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")

          if (grepl("Expert", otherBeliefSets[l])) {
            if (j == 1) {
              for (m in 1:nrow(scoreTbl)) {
                if (scoreTbl$userId[m] %in% RSRanking_unincentivized$userId) {
                  pop <- RSRanking_unincentivized[RSRanking_unincentivized$userId == scoreTbl$userId[m], ]$numQuestions + 1
                  RSRanking_unincentivized[RSRanking_unincentivized$userId == scoreTbl$userId[m], ]$rankSum <- RSRanking_unincentivized[RSRanking_unincentivized$userId == scoreTbl$userId[m], ]$rankSum + scoreTbl[scoreTbl$userId == scoreTbl$userId[m], ]$rank_unincentivized
                  # RSRanking_unincentivized[RSRanking_unincentivized$userId==scoreTbl$userId[m],]$rankSum = RSRanking_unincentivized[RSRanking_unincentivized$userId==scoreTbl$userId[m],]$rankSum+scoreTbl[scoreTbl$userId==scoreTbl$userId[m],]$rank_rs
                  RSRanking_unincentivized[RSRanking_unincentivized$userId == scoreTbl$userId[m], ]$n <- pop
                } else {
                  RSRanking_unincentivized <- rbind(RSRanking_unincentivized, data.frame(
                    userId = scoreTbl$userId[m],
                    group = scoreTbl$group[m],
                    rankSum = scoreTbl$rank_unincentivized[m],
                    # rankSum = scoreTbl$rank_rs[m],
                    numQuestions = 1
                  ))
                }
              }
            }

            write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)

            setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
          }
        }
      }
    }
  }
}

multiYearBinary_RS <- function(metaTable, data, summaryTable) {
  #' Scores the multi-year binary questions
  #'
  #' @description This scores everyone who answered the multi-year binary
  #' reciprocal questions (What do you believe? Ok, what do you think the [other
  #' supers, experts] believe?). They were asked about the MEDIAN belief of two
  #' groups, for each year (yes/no).
  #'
  #' Scoring rule:
  #' `log(1 - abs(action median - what you thought the median would be))`
  #'
  #' @param metaTable Metadata on the multi-year binary reciprocal questions
  #' @param data Data on the multi-year reciprocal binary questions (forecasts)
  #' @param summaryTable TODO
  #'
  #' @note "Median" belief on yes/no questions seems silly; did we ask any of these
  #'
  #' @export

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
        if (years[k] > 2030) {
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

          files <- list.files()
          phase_csv <- read.csv(files[grepl(paste0("Phase ", j, ".csv"), files)])

          sn <- currentSetName
          qn <- years[k]

          medianBeliefs <- summaryTable %>%
            filter(currentSetName == sn) %>%
            filter(currentQuestionName == qn) %>%
            filter(stage == j) %>%
            select(currentSetName, currentQuestionName, answerText, stage,
                   specialty, supersMedian, expertsG1Median,
                   domainExpertsMedian)

          if (specialty == "") {
            medianBeliefs$domainExpertsMedian <- medianBeliefs$expertsG1Median
          }

          phase_csv <- phase_csv %>%
            mutate(score_unincentivized = "", score_RS = "", group = "")

          for (l in 1:nrow(phase_csv)) {
            if (phase_csv$userName[l] %in% supers) {
              phase_csv$group[l] <- "supers"
            } else if (phase_csv$userName[l] %in% expertsG1$userName) {
              if (specialty != "") {
                if (expertsG1[expertsG1$userName == phase_csv$userName[l], ]$specialty == specialty) {
                  phase_csv$group[l] <- "domain experts"
                } else {
                  phase_csv$group[l] <- "non-domain experts"
                }
              } else {
                phase_csv$group[l] <- "experts"
              }
            } else if (phase_csv$userName[l] %in% expertsG2) {
              phase_csv$group[l] <- "experts (g2)"
            }
            if (phase_csv$group[l] == "supers") {
              phase_csv$score_unincentivized[l] <- log_score(medianBeliefs$supersMedian, phase_csv$forecast[l])
            } else if (phase_csv$group[l] %in% c("domain experts", "non-domain experts", "experts")) {
              phase_csv$score_unincentivized[l] <- log_score(medianBeliefs$domainExpertsMedian, phase_csv$forecast[l])
            } else {
              phase_csv$score_unincentivized[l] <- NA
            }
          }

          phase_csv$score_unincentivized <- as.numeric(phase_csv$score_unincentivized)

          phase_csv <- phase_csv %>%
            mutate(rank_unincentivized = rank(-score_unincentivized, ties.method = "min", na.last = "keep"))
          phase_csv <- phase_csv %>% mutate(rank_rs = NA)

          csv <- select(phase_csv, userId, teamName, teamId, setName, setId,
                        questionName, questionId, answerText, answerId,
                        score_unincentivized, score_RS, rank_unincentivized,
                        rank_rs, group)
          csv$score_unincentivized <- as.numeric(csv$score_unincentivized)
          csv <- unique(csv)

          if (dir.exists("Reciprocal Scoring")) {
            setwd("Reciprocal Scoring")
          } else {
            dir.create("Reciprocal Scoring")
            setwd("Reciprocal Scoring")
          }

          write.csv(csv, paste0(currentSetName, " - ", csv$questionName[1],
                                " - Reciprocal Scores.csv"), row.names = FALSE)

          setwd(paste0(yourHome, "Summary Data"))

          g1RS <- csv %>% filter(group %in% c("supers", "domain experts", "non-domain experts", "experts"))
          supersRS <- csv %>% filter(group == "supers")
          expertsG1RS <- csv %>% filter(group %in% c("domain experts", "non-domain experts", "experts"))
          expertsG2RS <- csv %>% filter(group == "experts (g2)")
          domainExpertsRS <- csv %>% filter(group == "domain experts")
          nonDomainExpertsRS <- csv %>% filter(group == "non-domain experts")

          RSTable <- read.csv("RSTable.csv")
          RSTable <- rbind(RSTable, data.frame(
            currentSetName = currentSetName,
            currentQuestionName = csv$questionName[1],
            answerText = csv$answerText[1],
            stage = j,
            specialty = specialty,
            g1Mean_unincentivized = mean(g1RS$score_unincentivized),
            supersMean_unincentivized = mean(supersRS$score_unincentivized),
            expertsG1Mean_unincentivized = mean(expertsG1RS$score_unincentivized),
            expertsG2Mean_unincentivized = mean(expertsG2RS$score_unincentivized),
            domainExpertsMean_unincentivized = mean(domainExpertsRS$score_unincentivized),
            nonDomainExpertsMean_unincentivized = mean(nonDomainExpertsRS$score_unincentivized),
            g1Mean_RS = NA,
            supersMean_RS = NA,
            expertsG1Mean_RS = NA,
            expertsG2Mean_RS = NA,
            domainExpertsMean_RS = NA,
            nonDomainExpertsMean_RS = NA
          ))
          write.csv(RSTable, "RSTable.csv", row.names = FALSE)

          RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")

          if (grepl("Expert", otherBeliefSets[l])) {
            if (j == 1) {
              for (m in 1:nrow(csv)) {
                if (csv$userId[m] %in% RSRanking_unincentivized$userId) {
                  pop <- RSRanking_unincentivized[RSRanking_unincentivized$userId == csv$userId[m], ]$numQuestions + 1
                  RSRanking_unincentivized[RSRanking_unincentivized$userId == csv$userId[m], ]$rankSum <- RSRanking_unincentivized[RSRanking_unincentivized$userId == csv$userId[m], ]$rankSum + csv[csv$userId == csv$userId[m], ]$rank_unincentivized
                  # RSRanking_unincentivized[RSRanking_unincentivized$userId==csv$userId[m],]$rankSum = RSRanking_unincentivized[RSRanking_unincentivized$userId==csv$userId[m],]$rankSum+csv[csv$userId==csv$userId[m],]$rank_unincentivized
                  RSRanking_unincentivized[RSRanking_unincentivized$userId == csv$userId[m], ]$n <- pop
                } else {
                  RSRanking_unincentivized <- rbind(RSRanking_unincentivized, data.frame(
                    userId = csv$userId[m],
                    group = csv$group[m],
                    rankSum = csv$rank_unincentivized[m],
                    # rankSum = csv$rank_rs[m],
                    numQuestions = 1
                  ))
                }
              }
            }
          }

          write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)

          setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
        }
      }
    }
  }
}

multiYearCountryDistrib_RS <- function(metaTable, data, summaryTable) {
  #' Scores the multi-year distribution reciprocal question sets where country also varied
  #'
  #' @description This scores everyone who answered the reciprocal questions
  #' (What do you believe? Ok, what do you think the [other supers, experts]
  #' believe?). They were asked about the MEDIAN belief of two groups, for each
  #' year and country combination.
  #'
  #' Scoring rule:
  #' `log(1 - abs(action median - what you thought the median would be))`
  #'
  #' @param metaTable Metadata on the multi-year country distribution reciprocal questions
  #' @param data Data on the multi-year country distribution reciprocal questions (forecasts)
  #' @param summaryTable TODO
  #'
  #' @note Do we use this
  #'
  #' @export

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

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    countries <- c(metaTable$country1[i], metaTable$country2[i],
                   metaTable$country3[i], metaTable$country4[i],
                   metaTable$country5[i], metaTable$country6[i],
                   metaTable$country7[i], metaTable$country8[i],
                   metaTable$country9[i], metaTable$country10[i],
                   metaTable$country11[i], metaTable$country12[i])
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

        if (years[k] > 2030) {
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

            files <- list.files()
            phase_csv <- read.csv(files[grepl(paste0("Phase ", j, ".csv"), files)])

            sn <- currentSetName
            qn <- years[k]
            at <- countries[l]

            medianBeliefs <- summaryTable %>%
              filter(currentSetName == sn) %>%
              filter(currentQuestionName == qn) %>%
              filter(answerText == at) %>%
              filter(stage == j) %>%
              select(currentSetName, currentQuestionName, answerText, stage,
                     specialty, supersMedian, expertsG1Median,
                     domainExpertsMedian)

            if (specialty == "") {
              medianBeliefs$domainExpertsMedian <- medianBeliefs$expertsG1Median
            }

            phase_csv <- phase_csv %>% mutate(score_unincentivized = "", score_RS = "", group = "")

            for (m in 1:nrow(phase_csv)) {
              if (phase_csv$userName[m] %in% supers) {
                phase_csv$group[m] <- "supers"
              } else if (phase_csv$userName[m] %in% expertsG1$userName) {
                if (specialty != "") {
                  if (expertsG1[expertsG1$userName == phase_csv$userName[m], ]$specialty == specialty) {
                    phase_csv$group[m] <- "domain experts"
                  } else {
                    phase_csv$group[m] <- "non-domain experts"
                  }
                } else {
                  phase_csv$group[m] <- "experts"
                }
              } else if (phase_csv$userName[m] %in% expertsG2) {
                phase_csv$group[m] <- "experts (g2)"
              }
              if (phase_csv$group[m] == "supers") {
                phase_csv$score_unincentivized[m] <- log_score(medianBeliefs$supersMedian, phase_csv$forecast[m])
              } else if (phase_csv$group[m] %in% c("domain experts", "non-domain experts", "experts")) {
                phase_csv$score_unincentivized[m] <- log_score(medianBeliefs$domainExpertsMedian, phase_csv$forecast[m])
              } else {
                phase_csv$score_unincentivized[m] <- NA
              }
            }

            phase_csv$score_unincentivized <- as.numeric(phase_csv$score_unincentivized)

            phase_csv <- phase_csv %>% mutate(rank_unincentivized = rank(-score_unincentivized, ties.method = "min", na.last = "keep"))
            phase_csv <- phase_csv %>% mutate(rank_rs = NA)

            csv <- select(phase_csv, userId, teamName, teamId, setName, setId, questionName, questionId, answerText, answerId, score_unincentivized, score_RS, rank_unincentivized, rank_rs, group)
            csv$score_unincentivized <- as.numeric(csv$score_unincentivized)
            csv <- unique(csv)

            if (dir.exists("Reciprocal Scoring")) {
              setwd("Reciprocal Scoring")
            } else {
              dir.create("Reciprocal Scoring")
              setwd("Reciprocal Scoring")
            }

            write.csv(csv, paste0(currentSetName, " - ", csv$questionName[1], " - Reciprocal Scores.csv"), row.names = FALSE)

            setwd(paste0(yourHome, "Summary Data"))

            g1RS <- csv %>% filter(group %in% c("supers", "domain experts", "non-domain experts", "experts"))
            supersRS <- csv %>% filter(group == "supers")
            expertsG1RS <- csv %>% filter(group %in% c("domain experts", "non-domain experts", "experts"))
            expertsG2RS <- csv %>% filter(group == "experts (g2)")
            domainExpertsRS <- csv %>% filter(group == "domain experts")
            nonDomainExpertsRS <- csv %>% filter(group == "non-domain experts")

            RSTable <- read.csv("RSTable.csv")
            RSTable <- rbind(RSTable, data.frame(
              currentSetName = currentSetName,
              currentQuestionName = csv$questionName[1],
              answerText = csv$answerText[1],
              stage = j,
              specialty = specialty,
              g1Mean_unincentivized = mean(g1RS$score_unincentivized),
              supersMean_unincentivized = mean(supersRS$score_unincentivized),
              expertsG1Mean_unincentivized = mean(expertsG1RS$score_unincentivized),
              expertsG2Mean_unincentivized = mean(expertsG2RS$score_unincentivized),
              domainExpertsMean_unincentivized = mean(domainExpertsRS$score_unincentivized),
              nonDomainExpertsMean_unincentivized = mean(nonDomainExpertsRS$score_unincentivized),
              g1Mean_RS = NA,
              supersMean_RS = NA,
              expertsG1Mean_RS = NA,
              expertsG2Mean_RS = NA,
              domainExpertsMean_RS = NA,
              nonDomainExpertsMean_RS = NA
            ))
            write.csv(RSTable, "RSTable.csv", row.names = FALSE)

            RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")

            if (grepl("Expert", otherBeliefSets[l])) {
              if (j == 1) {
                for (m in 1:nrow(csv)) {
                  if (csv$userId[m] %in% RSRanking_unincentivized$userId) {
                    pop <- RSRanking_unincentivized[RSRanking_unincentivized$userId == csv$userId[m], ]$numQuestions + 1
                    RSRanking_unincentivized[RSRanking_unincentivized$userId == csv$userId[m], ]$rankSum <- RSRanking_unincentivized[RSRanking_unincentivized$userId == csv$userId[m], ]$rankSum + csv[csv$userId == csv$userId[m], ]$rank_unincentivized
                    # RSRanking_unincentivized[RSRanking_unincentivized$userId==csv$userId[m],]$rankSum = RSRanking_unincentivized[RSRanking_unincentivized$userId==csv$userId[m],]$rankSum+csv[csv$userId==csv$userId[m],]$rank_unincentivized
                    RSRanking_unincentivized[RSRanking_unincentivized$userId == csv$userId[m], ]$n <- pop
                  } else {
                    RSRanking_unincentivized <- rbind(RSRanking_unincentivized, data.frame(
                      userId = csv$userId[m],
                      group = csv$group[m],
                      rankSum = csv$rank_unincentivized[m],
                      # rankSum = csv$rank_rs[m],
                      numQuestions = 1
                    ))
                  }
                }
              }
            }

            write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)

            setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", currentYear))
          }
        }
      }
    }
  }
}
