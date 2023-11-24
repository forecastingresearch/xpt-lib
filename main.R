rm(list = ls())

# Change this to the dir that houses the git repo (xpt-lib) # nolint
yourHome <<- "/path/to/xpt/"

# Set-up
options(scipen = 999)
setwd(paste0(yourHome, "sources"))

library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(xpt)
library(aggutils)

# Sometimes these files have had userName taken out; if that's the case, assign userId to userName # nolint
assign_user_id <- function(df) {
  if (!"userName" %in% names(df)) {
    df$userName <- df$userId
  }
  return(df)
}

data <- fread("forecasts_anon.csv")
data <- assign_user_id(data)
# data$timestamp <- ymd_hms(data$timestamp)
# data$stageOneTimestamp <- mdy_hms(data$stageOneTimestamp)
# data$stageTwoTimestamp <- mdy_hms(data$stageTwoTimestamp)
# data$stageThreeTimestamp <- mdy_hms(data$stageThreeTimestamp)
# data$stageFourTimestamp <- mdy_hms(data$stageFourTimestamp)
data$forecast <- as.numeric(data$forecast)

supers <- fread("supers_anon.csv")
supers <- supers$x

expertsG1 <- fread("expertsG1_anon.csv")
expertsG1 <- assign_user_id(expertsG1)

teams <- data %>% select(userName, userId, teamId)
teams <- unique(teams)

meta <- fread("questionMetadata.csv")
# meta = fread("GDPGrowthMetadata.csv")
# questionTypes <- unique(meta$questionType)
questions <- unique(meta$setName)

# fix #40 question name
data$setName[grep("40.", data$setName)] <- unique(meta$setName[grep("40.", meta$setName)])

setwd(paste0(yourHome, "sources/public-survey"))
survey_column_matches <- read.csv("survey_column_matches.csv")
survey_column_matches[is.na(survey_column_matches)] <- ""

public_supplement1 <- read.csv("public_supplement1_anon.csv")
public_supplement1 <- public_supplement1 %>%
  filter(Finished == TRUE)
public_supplement2 <- read.csv("public_supplement2_anon.csv")
public_supplement2 <- public_supplement2 %>%
  filter(Finished == TRUE)
public_supplement3 <- read.csv("public_supplement3_anon.csv")
public_supplement3 <- public_supplement3 %>%
  filter(Finished == TRUE)

setwd(paste0(yourHome, "Summary Data"))

##### Main Analysis #####

if ("summaryTable.csv" %in% list.files()) {
  summaryTable <- read.csv("summaryTable.csv")
} else {
  summaryTable <- newAddInit()
}
write.csv(summaryTable, "summaryTable.csv", row.names = FALSE)

#####

startDate <- as.Date("2022-06-16 UTC")
phaseTwoMedian <- as.Date("2022-07-16")
endDate <- ymd("2022 10 31")

timeline <- seq(startDate, endDate, 1)

for (i in 1:length(questions)) {
  metaTable <- meta %>% filter(setName == questions[i])
  if (metaTable$questionType[1] == "multiYearReciprocal") {
    newAdd <- multiYearReciprocal(metaTable, data)
    summaryTable <- rbind(summaryTable, newAdd)
  }
  if (metaTable$questionType[1] == "pointDistrib") {
    newAdd <- pointDistrib(metaTable, data)
    summaryTable <- rbind(summaryTable, newAdd)
  }
  if (metaTable$questionType[1] == "multiYearDistrib") {
    newAdd <- multiYearDistrib(metaTable, data)
    summaryTable <- rbind(summaryTable, newAdd)
  }
  if (metaTable$questionType[1] == "multiYearBinary") {
    newAdd <- multiYearBinary(metaTable, data)
    summaryTable <- rbind(summaryTable, newAdd)
  }
  if (metaTable$questionType[1] == "multiYearCountryDistrib") {
    newAdd <- multiYearCountryDistrib(metaTable, data)
    summaryTable <- rbind(summaryTable, newAdd)
  }
  if (metaTable$questionType[1] == "multiCountryBinary") {
    newAdd <- multiCountryBinary(metaTable, data)
    summaryTable <- rbind(summaryTable, newAdd)
  }
  if (metaTable$questionType[1] == "pointBinary") {
    newAdd <- pointBinary(metaTable, data)
    summaryTable <- rbind(summaryTable, newAdd)
  }
  setwd(paste0(yourHome, "Summary Data"))
  write.csv(summaryTable, "summaryTable.csv", row.names = FALSE)
  print("summaryTable.csv updated")
}

if ("RSTable.csv" %in% list.files()) {
  RSTable <- read.csv("RSTable.csv")
} else {
  RSTable <- RSInit()
  write.csv(RSTable, "RSTable.csv", row.names = FALSE)
}

##### Reciprocal Scoring #####

if ("RSRanking_unincentivized.csv" %in% list.files()) {
  RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")
} else {
  RSRanking_unincentivized <- RSRankingInit()
  write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)
}

metaTable <- meta %>% filter(questionType == "multiYearReciprocal")
multiYearReciprocal_RS(metaTable, data, summaryTable)

setwd(paste0(yourHome, "Summary Data"))
RSTable <- read.csv("RSTable.csv")
RSRanking_unincentivized <- read.csv("RSRanking_unincentivized.csv")
RSRanking_unincentivized <- RSRanking_unincentivized %>% mutate(avgRank = rankSum / numQuestions)
write.csv(RSRanking_unincentivized, "RSRanking_unincentivized.csv", row.names = FALSE)

##### Produce Reciprocal Scoring Quintile Graphs #####
rs_rank <- read.csv("RSRanking_unincentivized_first10.csv")

rs_rank <- rs_rank %>%
  filter(numQuestions >= 30) %>%
  filter(group %in% c("supers", "domain experts", "non-domain experts"))
rs_rank$group[rs_rank$group %in% c("domain experts", "non-domain experts")] <- "experts"

rs_rank <- arrange(rs_rank, avgRank)

rs_rank <- mutate(rs_rank, specialty1 = "", specialty2 = "")

for (i in 1:nrow(rs_rank)) {
  if (rs_rank$group[i] == "experts" & rs_rank$userId[i] %in% expertsG1$userId) {
    rs_rank$specialty1[i] <- expertsG1[expertsG1$userId == rs_rank$userId[i], ]$specialty1
    rs_rank$specialty2[i] <- expertsG1[expertsG1$userId == rs_rank$userId[i], ]$specialty2
  }
}

d_20th <- round(0.2 * nrow(rs_rank))
d_40th <- round(0.4 * nrow(rs_rank))
d_60th <- round(0.6 * nrow(rs_rank))
d_80th <- round(0.8 * nrow(rs_rank))

q1 <- rs_rank[1:d_20th, ]
q2 <- rs_rank[d_20th:d_40th, ]
q3 <- rs_rank[d_40th:d_60th, ]
q4 <- rs_rank[d_60th:d_80th, ]
q5 <- rs_rank[d_80th:nrow(rs_rank), ]

setwd(paste0(yourHome, "Summary Data"))

metaTable <- meta %>% filter(questionType == "multiYearReciprocal")

for (i in 1:length(unique(metaTable$setName))) {
  setwd(paste0(yourHome, "Summary Data"))

  print(unique(metaTable$setName)[i])
  currentSetName <- unique(metaTable$setName)[i]

  setwd(currentSetName)

  setwd("Phase Data")

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

  for (j in 1:length(unique(metaTable$stage))) {
    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

    print(paste("Stage:", (unique(metaTable$stage)[j])))
    currentStage <- unique(metaTable$stage)[j]

    for (k in 1:length(years)) {
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))

      print(years[k])
      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))

      for (l in 1:length(beliefSets)) {
        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))

        print(beliefSets[l])
        setwd(beliefSets[l])

        files <- list.files()
        csv <- read.csv(files[grepl(paste0("Phase ", j, ".csv"), files)])
        csv <- csv %>% filter(userId %in% rs_rank$userId)

        tbl <- data.frame(
          userType = character(0),
          forecast = numeric(0),
          quintile = character(0)
        )

        for (m in 1:nrow(q1)) {
          currentId <- q1$userId[m]
          forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast
          if (length(forecast) > 0) {
            tbl <- rbind(tbl, data.frame(
              userType = q1[m, ]$group,
              forecast = forecast,
              quintile = "Q1"
            ))
          }
        }

        for (m in 1:nrow(q2)) {
          currentId <- q2$userId[m]
          forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast
          if (length(forecast) > 0) {
            tbl <- rbind(tbl, data.frame(
              userType = q2[m, ]$group,
              forecast = forecast,
              quintile = "Q2"
            ))
          }
        }

        for (m in 1:nrow(q3)) {
          currentId <- q3$userId[m]
          forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast
          if (length(forecast) > 0) {
            tbl <- rbind(tbl, data.frame(
              userType = q3[m, ]$group,
              forecast = forecast,
              quintile = "Q3"
            ))
          }
        }

        for (m in 1:nrow(q4)) {
          currentId <- q4$userId[m]
          forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast
          if (length(forecast) > 0) {
            tbl <- rbind(tbl, data.frame(
              userType = q4[m, ]$group,
              forecast = forecast,
              quintile = "Q4"
            ))
          }
        }

        for (m in 1:nrow(q5)) {
          currentId <- q5$userId[m]
          forecast <- (csv %>% filter(userId == currentId) %>% select(forecast))$forecast
          if (length(forecast) > 0) {
            tbl <- rbind(tbl, data.frame(
              userType = q5[m, ]$group,
              forecast = forecast,
              quintile = "Q5"
            ))
          }
        }

        tbl$quintile <- factor(tbl$quintile, levels = unique(tbl$quintile), ordered = TRUE)
        tbl$userType <- factor(tbl$userType, levels = c("supers", "experts"), ordered = TRUE)

        title <- paste(currentSetName, "by", years[k])
        subtitle <- paste("Stage", j, "|", beliefSets[l], "|", "RS Accuracy (Unincentivized, First 10)")

        plot <- ggplot(tbl, aes(x = quintile, y = forecast, group = quintile)) +
          geom_boxplot(outlier.shape = NA) +
          ylab("Forecast") +
          xlab("Quintile") +
          labs(title = title, subtitle = subtitle) +
          theme_bw() +
          coord_trans(y = pseudo_log_trans(base = 10), ylim = c(0, 100)) +
          scale_y_continuous(breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100)) +
          scale_color_manual(values = hue_pal()(3)) +
          theme(
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.title = element_text(size = 9, vjust = -37)
          ) +
          geom_point(position = position_jitterdodge(), aes(x = quintile, y = forecast, color = userType)) +
          geom_label(aes(x = 1, y = median((tbl %>% filter(quintile == "Q1") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q1") %>% select(forecast))$forecast), group = userType)) +
          geom_label(aes(x = 2, y = median((tbl %>% filter(quintile == "Q2") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q2") %>% select(forecast))$forecast), group = userType)) +
          geom_label(aes(x = 3, y = median((tbl %>% filter(quintile == "Q3") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q3") %>% select(forecast))$forecast), group = userType)) +
          geom_label(aes(x = 4, y = median((tbl %>% filter(quintile == "Q4") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q4") %>% select(forecast))$forecast), group = userType)) +
          geom_label(aes(x = 5, y = median((tbl %>% filter(quintile == "Q5") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q5") %>% select(forecast))$forecast), group = userType))
        plot$labels$colour <- ""
        plot$labels$fill <- ""

        if (dir.exists("RS Accuracy")) {
          setwd("RS Accuracy")
        } else {
          dir.create("RS Accuracy")
          setwd("RS Accuracy")
        }

        ggsave(paste0(title, "-", subtitle, ".png"), plot, width = 3600, height = 2000, units = c("px"))

        setwd(paste0(yourHome, "Summary Data"))

        if (dir.exists("RS Accuracy")) {
          setwd("RS Accuracy")
        } else {
          dir.create("RS Accuracy")
          setwd("RS Accuracy")
        }

        ggsave(paste0(title, "-", subtitle, ".png"), plot, width = 3600, height = 2000, units = c("px"))

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[k]))
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }

    setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
  }

  setwd(paste0(yourHome, "Summary Data"))
}

##### Aggregation Methods #####

main1 <- fread("public-survey/public_supplement1_anon.csv") %>%
  filter(Finished == "TRUE")
main2 <- fread("public-survey/public_supplement2_anon.csv") %>%
  filter(Finished == "TRUE")
main3 <- fread("public-survey/public_supplement3_anon.csv") %>%
  filter(Finished == "TRUE")
mapping <- fread("public-survey/survey_column_matches.csv")

getPublicSurveyForecasts <- function(sn, bs, y, d) {
  sheetInfo <- mapping %>%
    rowwise() %>%
    filter(grepl(sn, setName)) %>%
    filter(grepl(bs, beliefSet)) %>%
    filter(distrib == d)
  if (is.na(y)) {
    sheetInfo <- sheetInfo %>%
      filter(is.na(year))
  } else {
    sheetInfo <- sheetInfo %>%
      filter(year == y)
  }
  if (nrow(sheetInfo) > 0) {
    if (sheetInfo$sheet == "public_supplement1") {
      publicSurvey <- as.numeric(unlist(main1 %>%
        select(all_of(sheetInfo$colName))))
      publicSurvey <- publicSurvey[!is.na(publicSurvey)]
    } else if (sheetInfo$sheet == "public_supplement2") {
      publicSurvey <- as.numeric(unlist(main2 %>%
        select(all_of(sheetInfo$colName))))
      publicSurvey <- publicSurvey[!is.na(publicSurvey)]
    } else if (sheetInfo$sheet == "public_supplement3") {
      publicSurvey <- as.numeric(unlist(main3 %>%
        select(all_of(sheetInfo$colName))))
    }
  }
  publicSurvey <- publicSurvey[publicSurvey >= 0]
  publicSurvey <- publicSurvey[publicSurvey <= 100]

  data <- data.frame(
    userId = "Public Survey Respondent",
    forecast = publicSurvey
  )

  return(data)
}

make_agg_methods_table <- function(set, year, data = fread("forecasts_anon.csv") %>% mutate(questionName = tolower(questionName))) {
  #' Make a table for a given question with a few different aggregation
  #' methods, by group. Groups and methods currently hardcoded.
  #'
  #' @usage make_agg_methods_table("AI Catastrophic Risk", 2100)

  if (grepl("Future Human", set)) {
    data <- data %>% filter(questionName == "Future Humans")
    psData <- getPublicSurveyForecasts("12. Future Human Births", "", NA, "50th %")
  } else if (grepl("Year of Extinction", set)) {
    data <- data %>% filter(questionName == "Extinction Year")
    psData <- getPublicSurveyForecasts("11. Year of Extinction", "", NA, "50th %")
  } else {
    data <- data %>%
      filter(
        grepl(set, setName),
        questionName == paste0(year, " (your beliefs)")
      )
    psData <- getPublicSurveyForecasts(set, "Your", year, "")
  }

  if ("timestampId" %in% colnames(data)) {
    data <- data %>%
      group_by(userId) %>%
      slice_max(timestampId) %>%
      ungroup() %>%
      select(userId, forecast, timestampId, setName) %>%
      mutate(forecast = as.numeric(forecast))
  }

  if (nrow(data) == 0) {
    stop("No data for this question (did you perhaps typo something).")
  }

  # Map questions to domains and merge with data, IF it's not the post mortem data.
  if (!grepl("_you", set)) {
    questionMeta <- fread("questionMetadata.csv") %>%
      mutate(domain = specialty) %>%
      mutate(defaultForecast = defaultForecast50) %>%
      select(setName, domain, defaultForecast) %>%
      distinct(setName, .keep_all = TRUE)
    data <- merge(data, questionMeta, by = "setName")
  }

  # Get what group people belonged to (supers, G1 experts)
  supers <- fread("supers_anon.csv") %>%
    rename(userId = x) %>%
    mutate(group = "super") %>%
    select(userId, group)

  experts <- fread("expertsG1_anon.csv") %>%
    mutate(
      specialty = ifelse(specialty2 != "", paste(specialty1, specialty2, sep = ", "), specialty1),
      specialty = ifelse(specialty3 != "", paste(specialty, specialty3, sep = ", "), specialty)
    ) %>%
    select(userId, specialty) %>%
    mutate(group = "expertG1")

  # Join supers with experts
  supers$specialty <- NA
  allUsers <- rbind(supers, experts)

  # Merge specialties with data.
  data <- merge(data, allUsers, by = "userId", all.x = TRUE)

  # Filter out G2 experts and distinguish domain from non-domain experts.
  if (all(data$domain == "")) {
    data <- data %>%
      filter(!(group == "expertG2")) %>%
      rowwise() %>%
      mutate(group = case_when(
        group == "super" ~ "super",
        grepl("General", specialty) ~ "generalist",
        (group == "expertG1" && domain == "") ~ "experts (w/o generalists)",
        .default = "non-domain expert"
      ))
  } else {
    data <- data %>%
      filter(!(group == "expertG2")) %>%
      rowwise() %>%
      mutate(group = case_when(
        group == "super" ~ "super",
        grepl(domain, specialty) ~ "domain expert",
        grepl("General", specialty) ~ "generalist",
        .default = "non-domain expert"
      ))
  }

  # Check whether "forecast" in colnames
  if ("forecast" %in% colnames(psData)) {
    # Merge with public survey data now
    psData$group <- "public survey"
    data <- data %>% mutate(userId = as.character(userId))
    psData <- psData %>% mutate(forecast = as.numeric(forecast))
    # NO default forecast for public survey (effect: don't drop any forecasts)
    psData$defaultForecast <- 101
    data <- bind_rows(data, psData)
  }

  # Filter out default forecasts.
  warning(paste0("Filtering out ", sum(data$forecast == data$defaultForecast), " default forecasts."))
  data <- data %>%
    filter(forecast != defaultForecast)

  # Group by group and get the median, mean, HD trimmed mean, and Neyman's.
  our_table <- data %>%
    group_by(group) %>%
    summarize(
      n_ids = n(),
      median = median(forecast),
      median_confint = boot_results(forecast, statistic = "median"),
      mean = mean(forecast),
      mean_confint = boot_results(forecast, statistic = "mean"),
      geom_mean = geoMeanCalc(forecast),
      geom_mean_confint = boot_results(forecast, statistic = "geoMeanCalc"),
      hd_trim = hd_trim(forecast),
      hd_trim_confint = boot_results(forecast, statistic = "hd_trim"),
      simple_trim = trim(forecast),
      simple_trim_confint = boot_results(forecast, statistic = "trim"),
      neyman = neymanAggCalc(forecast),
      neyman_confint = boot_results(forecast, statistic = "neymanAggCalc"),
      geom_mean_of_odds = geoMeanOfOddsCalc(forecast),
      geom_mean_of_odds_confint = boot_results(forecast, statistic = "geoMeanOfOddsCalc")
    )

  # Now add the "all experts" group back
  data_experts <- data %>%
    rowwise() %>%
    mutate(group = case_when(
      (group != "super" && group != "public survey") ~ "all experts",
    )) %>%
    filter(group == "all experts")

  data_experts <- data_experts %>%
    group_by(group) %>%
    summarize(
      n_ids = n(),
      median = median(forecast),
      median_confint = boot_results(forecast, statistic = "median"),
      mean = mean(forecast),
      mean_confint = boot_results(forecast, statistic = "mean"),
      geom_mean = geoMeanCalc(forecast),
      geom_mean_confint = boot_results(forecast, statistic = "geoMeanCalc"),
      hd_trim = hd_trim(forecast),
      hd_trim_confint = boot_results(forecast, statistic = "hd_trim"),
      simple_trim = trim(forecast),
      simple_trim_confint = boot_results(forecast, statistic = "trim"),
      neyman = neymanAggCalc(forecast),
      neyman_confint = boot_results(forecast, statistic = "neymanAggCalc"),
      geom_mean_of_odds = geoMeanOfOddsCalc(forecast),
      geom_mean_of_odds_confint = boot_results(forecast, statistic = "geoMeanOfOddsCalc")
    )

  our_table <- bind_rows(our_table, data_experts)

  return(our_table)
}

# Create an empty list
agg_methods_tables <- list()

for (question in c(
  "AI Catastrophic Risk", "AI Extinction Risk",
  "Nuclear Catastrophic Risk", "Nuclear Extinction Risk",
  "Non-Anthropogenic Catastrophic Risk", "Non-Anthropogenic Extinction Risk",
  "Total Catastrophic", "Total Extinction"
)) {
  print(question)

  a <- make_agg_methods_table(question, 2100)

  # For each of the columns that are lists, make them into two separate columns
  a <- a %>%
    mutate(
      median_confint_lower = median_confint$confint_lower,
      median_confint_upper = median_confint$confint_upper,
      mean_confint_lower = mean_confint$confint_lower,
      mean_confint_upper = mean_confint$confint_upper,
      geom_mean_confint_lower = geom_mean_confint$confint_lower,
      geom_mean_confint_upper = geom_mean_confint$confint_upper,
      hd_trim_confint_lower = hd_trim_confint$confint_lower,
      hd_trim_confint_upper = hd_trim_confint$confint_upper,
      simple_trim_confint_lower = simple_trim_confint$confint_lower,
      simple_trim_confint_upper = simple_trim_confint$confint_upper,
      neyman_confint_lower = neyman_confint$confint_lower,
      neyman_confint_upper = neyman_confint$confint_upper,
      geom_mean_of_odds_confint_lower = geom_mean_of_odds_confint$confint_lower,
      geom_mean_of_odds_confint_upper = geom_mean_of_odds_confint$confint_upper
    ) %>%
    select(-median_confint, -mean_confint, -geom_mean_confint, -hd_trim_confint, -simple_trim_confint, -neyman_confint, -geom_mean_of_odds_confint)

  # Append to the list
  agg_methods_tables[[question]] <- a
}

all_tables <- bind_rows(agg_methods_tables, .id = "question")
all_tables_next <- all_tables %>%
  select(-contains("simple_trim"))

# Put the columns in alphabetical order, except question, group, n_ids (put them first)
all_tables_next <- all_tables_next[, c("question", "group", "n_ids", sort(colnames(all_tables_next)[!(colnames(all_tables_next) %in% c("question", "group", "n_ids"))]))]

all_tables_next <- distinct(all_tables_next)

# Round to 5 significant digits, which will always be enough AND avoid the weird base R rounding
all_tables_next <- all_tables_next %>%
  mutate_if(is.numeric, ~ signif(., 5))

# Write to csv with no row names
write.csv(all_tables_next, "agg_methods_tables_with_ci.csv", row.names = FALSE)
