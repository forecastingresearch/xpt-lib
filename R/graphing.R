library(dplyr)
library(docstring)
library(ggthemes)
library(lubridate)
library(boot)
library(ggplot2)
library(scales)
library(ncar, include.only = "Round")
library(gdata, include.only = "combine")

# Generate a colorblind-friendly palette with six colors
cb_pal <- colorblind_pal()(8)

# Exclude black from the palette
cb_pal <- tail(cb_pal, -1)

group_colors <- list(
  "Superforecasters" = cb_pal[1],
  "Domain Experts" = cb_pal[2],
  "Other Experts" = cb_pal[2],
  "General X-risk Experts" = cb_pal[3],
  "Non-domain Experts" = cb_pal[4],
  "Public Survey" = cb_pal[5],
  "Biorisk Experts" = cb_pal[2],
  "AI Experts" = cb_pal[2],
  "Climate Experts" = cb_pal[2],
  "Nuclear Experts" = cb_pal[2],
  "Experts" = cb_pal[2]
)

boot_results <- function(plotTable, statistic = "median", width = 0.95) {
  #' Get bootstrapped confidence intervals
  #'
  #' `do` function applies the `boot` function to each combination of (group,
  #' currentDate) and creates a new dataframe with a row for each combination...
  #' Assumes this is being called from within figureDataMetrics (one question,
  #' one group at a time)
  #'
  #' @param plotTable Data frame with column forecast, OR vector of forecasts
  #' @param statistic Statistic to calculate (default is median)
  #' @param width Width of confidence interval (default is 0.95)
  #'
  #' @importFrom boot boot boot.ci
  #' @export

  set.seed(123)

  stat_fun <- match.fun(statistic)

  # If plotTable is NOT a dataframe, make it one
  if (!is.data.frame(plotTable)) {
    plotTable <- data.frame(plotTable) %>%
      rename(forecast = plotTable)
  }

  # If plotTable is empty, return NA's
  if (nrow(plotTable) == 0) {
    return(data.frame(confint_lower = NA, confint_upper = NA))
  }

  interval <- plotTable %>%
    do({
      x <- .$forecast
      res <- boot(x, statistic = function(x, i) stat_fun(x[i]), R = 1000)
      if (all(res$t == res$t[1], na.rm = TRUE)) {
        data.frame(confint_lower = NA, confint_upper = NA)
      } else {
        a <- boot.ci(res, conf = width, type = "perc")
        data.frame(confint_lower = a$percent[4], confint_upper = a$percent[5])
      }
    })
  return(interval)
}

plot_with_ribbons <- function(plotTable, title, subtitle, phaseTwoMedian, fname) {
  #' Plot time series with confidence interval ribbons.
  #'
  #' @param plotTable Data frame with columns currentDate, median, group
  #' @param title Title of plot
  #' @param subtitle Subtitle of plot
  #' @param phaseTwoMedian Date of median start time for Phase 2 (we cut the plot off on the left
  #' at this date)
  #'
  #' @export

  if (FALSE) {
    plotTable <- plotTable %>%
      rename(confint_lower = contains("confint_lower"), confint_upper = contains("confint_upper"))
  }

  if ("Domain Experts" %in% plotTable$group) {
    plotTable <- plotTable %>% filter(group != "Experts")
  }

  # Table for legend labels
  group_counts <- plotTable %>%
    group_by(group) %>%
    summarise(n = max(n))
  legend_labels <- paste0(group_counts$group, " (n = ", group_counts$n, ")")

  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, color = group, fill = group)) +
    geom_line() +
    ylab("Median") +
    xlab("Date") +
    labs(title = title, subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels) +
    scale_fill_manual(values = unlist(group_colors)) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    xlim(phaseTwoMedian, NA)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/", fname, ".png")), plot, width = 9.18, height = 5.78, units = c("in"))
  ggsave(gsub("%", "%%", paste0(file_path, "/", fname, "_vector.ps")), plot, width = 9.18, height = 5.78, units = c("in"))

  if (FALSE) {
    plot <- plot +
      geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group, color = group), alpha = 0.1, linetype = "dotted")
    ggsave(paste0(fname, "_with_CI.png"), plot, width = 9.18, height = 5.78, units = c("in"))
  }

  return(plot)
}

mutate_figure_data_median <- function(csv) {
  #' Mutate figure data for plotting
  #'
  #' @export

  # REMOVE the levels to begin with
  plotTable <- csv %>%
    mutate(group = as.character(group))

  # Rename the columns
  plotTable <- plotTable %>%
    select(group, year, currentDate, median, n) %>%
    mutate(
      currentDate = ymd(currentDate),
      group = case_when(
        group == "supers" ~ "Superforecasters",
        group == "experts" ~ "Experts",
        group == "domainExperts" ~ "Domain Experts",
        group == "nonDomainExperts" ~ "Non-domain Experts",
        group == "general" ~ "General X-risk Experts"
      )
    )

  # Who's getting dropped? Print out the groups where n < 10
  # print(plotTable %>% group_by(group) %>% summarize(n = first(n)))

  # Filter and re-instate the levels now that the group names are correct
  plotTable <- plotTable %>%
    filter(group %in% c(
      "Superforecasters", "Experts", "Domain Experts",
      "Non-domain Experts", "General X-risk Experts"
    )) %>%
    mutate(
      group = factor(group, levels = unique(group), ordered = TRUE),
      median = replace(median, n < 10 | (group == "Non-domain Experts" & n < 4), NA)
    ) %>%
    filter(currentDate > ymd("2022 07 14"))

  # Remove groups with no data
  plotTable <- plotTable %>%
    group_by(group) %>%
    filter(!all(is.na(median))) %>%
    ungroup()

  return(plotTable)
}

mutate_figure_data_sd <- function(csv) {
  #' Mutate figure data for plotting
  #'
  #' @export

  # REMOVE the levels to begin with
  plotTable <- csv %>%
    mutate(group = as.character(group))

  # Rename the columns
  plotTable <- plotTable %>%
    select(group, year, currentDate, sd, n) %>%
    mutate(
      currentDate = ymd(currentDate),
      group = case_when(
        group == "supers" ~ "Superforecasters",
        group == "experts" ~ "Experts",
        group == "domainExperts" ~ "Domain Experts",
        group == "nonDomainExperts" ~ "Non-domain Experts",
        group == "general" ~ "General X-risk Experts"
      )
    )

  # Filter and re-instate the levels now that the group names are correct
  plotTable <- plotTable %>%
    filter(group %in% c(
      "Superforecasters", "Experts", "Domain Experts",
      "Non-domain Experts", "General X-risk Experts"
    )) %>%
    mutate(
      group = factor(group, levels = unique(group), ordered = TRUE),
      sd = replace(sd, n < 10 | (group == "Non-domain Experts" & n < 4), NA)
    ) %>%
    filter(currentDate > ymd("2022 07 14"))

  # Remove groups with no data
  plotTable <- plotTable %>%
    group_by(group) %>%
    filter(!all(is.na(sd))) %>%
    ungroup()

  return(plotTable)
}

mutate_figure_data_hd_trim <- function(csv) {
  #' Mutate figure data for plotting
  #'
  #' @export

  # REMOVE the levels to begin with
  plotTable <- csv %>%
    mutate(group = as.character(group))

  # Rename the columns
  plotTable <- plotTable %>%
    select(group, year, currentDate, hd_trim, n) %>%
    mutate(
      currentDate = ymd(currentDate),
      group = case_when(
        group == "supers" ~ "Superforecasters",
        group == "experts" ~ "Experts",
        group == "domainExperts" ~ "Domain Experts",
        group == "nonDomainExperts" ~ "Non-domain Experts",
        group == "general" ~ "General X-risk Experts"
      )
    )

  # Filter and re-instate the levels now that the group names are correct
  plotTable <- plotTable %>%
    filter(group %in% c(
      "Superforecasters", "Experts", "Domain Experts",
      "Non-domain Experts", "General X-risk Experts"
    )) %>%
    mutate(
      group = factor(group, levels = unique(group), ordered = TRUE),
      hd_trim = replace(hd_trim, n < 10 | (group == "Non-domain Experts" & n < 4), NA),
      # hd_trim_confint_lower = replace(hd_trim_confint_lower, n < 10 | (group == "Non-domain Experts" & n < 4), NA),
      # hd_trim_confint_upper = replace(hd_trim_confint_upper, n < 10 | (group == "Non-domain Experts" & n < 4), NA)
    ) %>%
    filter(currentDate > ymd("2022 07 14"))

  # Remove groups with no data
  plotTable <- plotTable %>%
    group_by(group) %>%
    filter(!all(is.na(hd_trim))) %>%
    ungroup()

  return(plotTable)
}

histogram <- function(questionDataProcessed, filenameStart, title, stage,
                      specialty, expectedRisk, forecastMin, forecastMax) {
  #' Histogram
  #'
  #' @import ggplot2
  #' @import scales
  #' @export

  if (grepl("%", filenameStart)) {
    filenameStart <- gsub("%", "%%", filenameStart)
  }

  median <- median(questionDataProcessed$forecast)
  plot <- ggplot(questionDataProcessed, aes(x = forecast)) +
    geom_histogram(bins = 30, boundary = 0) +
    geom_vline(aes(color = paste0("Median: ", median), xintercept = median)) +
    xlab("Forecast") +
    ylab("Count") +
    labs(title = title, subtitle = paste0("All Forecasters (n=", nrow(questionDataProcessed), ") | Stage: ", stage)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    )
  plot$labels$color <- ""
  if (expectedRisk == "low" & forecastMin == 0 & forecastMax == 100) {
    plot <- plot +
      scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100))
  }
  ggsave(paste0(filenameStart, ".png"), plot, width = 9.18, height = 5.78, units = c("in"))

  supersSubset <- questionDataProcessed %>% filter(userName %in% supers)
  median <- median(supersSubset$forecast)
  plot <- ggplot(supersSubset, aes(x = forecast)) +
    geom_histogram(bins = 30, boundary = 0) +
    geom_vline(aes(color = paste0("Median: ", median), xintercept = median)) +
    xlab("Forecast") +
    ylab("Count") +
    labs(title = title, subtitle = paste0("Superforecasters (n=", nrow(supersSubset), ") | Stage: ", stage)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    )
  plot$labels$color <- ""
  if (expectedRisk == "low" & forecastMin == 0 & forecastMax == 100) {
    plot <- plot +
      scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100))
  }
  ggsave(paste0(filenameStart, " Supers.png"), plot, width = 9.18, height = 5.78, units = c("in"))

  expertsSubset <- questionDataProcessed %>% filter(userName %in% expertsG1$userName)
  median <- median(expertsSubset$forecast)
  plot <- ggplot(expertsSubset, aes(x = forecast)) +
    geom_histogram(bins = 30, boundary = 0) +
    geom_vline(aes(color = paste0("Median: ", median), xintercept = median)) +
    xlab("Forecast") +
    ylab("Count") +
    labs(title = title, subtitle = paste0("Experts (n=", nrow(expertsSubset), ") | Stage: ", stage)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    )
  plot$labels$color <- ""
  if (expectedRisk == "low" & forecastMin == 0 & forecastMax == 100) {
    plot <- plot +
      scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100))
  }
  ggsave(paste0(filenameStart, " Experts.png"), plot, width = 9.18, height = 5.78, units = c("in"))

  if (specialty != "") {
    field <- specialty
    domainExperts <- filter(expertsG1, specialty1 == field | specialty2 == field | specialty3 == field)
    domainExpertsSubset <- questionDataProcessed %>% filter(userName %in% domainExperts$userName)
    median <- median(domainExpertsSubset$forecast)
    plot <- ggplot(domainExpertsSubset, aes(x = forecast)) +
      geom_histogram(bins = 30, boundary = 0) +
      geom_vline(aes(color = paste0("Median: ", median), xintercept = median)) +
      xlab("Forecast") +
      ylab("Count") +
      labs(title = title, subtitle = paste0(field, " Experts (n=", nrow(domainExpertsSubset), ") | Stage: ", stage)) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank()
      )
    plot$labels$color <- ""
    if (expectedRisk == "low" & forecastMin == 0 & forecastMax == 100) {
      plot <- plot +
        scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100))
    }
    ggsave(paste0(filenameStart, " ", field, " Experts.png"), plot, width = 9.18, height = 5.78, units = c("in"))
  }
}

boxPlot <- function(files, type, specialty, title, subtitle, filenameStart,
                    expectedRisk, forecastMin, forecastMax, numerateCitizens, yLabel, setName, beliefSet, year, distrib) {
  #' Basic boxplot function
  #'
  #' @importFrom ncar Round
  #' @import ggplot2
  #' @import scales
  #' @export

  tbl <- read.csv(files[1])

  if (type == "distrib") {
    # FOR LOOP TO ADD TO TBL AND REST
  }

  if (type == "regGroups") {
    boxData_supers <- tbl %>% filter(userName %in% supers)
    boxData <- boxData_supers %>% mutate(group = "Superforecasters")
    boxData_experts <- tbl %>% filter(userName %in% expertsG1$userName)
    boxData_general <- tbl %>% filter(userName %in% filter(expertsG1, specialty1 == "General" | specialty2 == "General" | specialty3 == "General")$userName)
    if (specialty != "") {
      field <- specialty
      specialists <- expertsG1 %>% filter(field == specialty1 | field == specialty2 | field == specialty3)
      boxData_special <- tbl %>% filter(userName %in% specialists$userName)
      boxData <- rbind(boxData, boxData_special %>% mutate(group = paste0(field, " Experts")))
      boxData_nonSpecial <- tbl %>%
        filter(userName %in% expertsG1$userName) %>%
        filter(!(userName %in% specialists$userName)) %>%
        filter(!(userName %in% boxData_general$userName))
      boxData <- rbind(boxData, boxData_nonSpecial %>% mutate(group = "Non-domain Experts"))
    } else {
      boxData <- rbind(boxData, boxData_experts %>% mutate(group = "Experts"))
    }
    boxData <- rbind(boxData, boxData_general %>% mutate(group = "General X-risk Experts"))

    boxData <- select(boxData, group, forecast)

    sn <- setName
    bs <- beliefSet
    y <- year
    d <- distrib

    if (numerateCitizens == TRUE) {
      wd <- getwd()
      sheetInfo <- survey_column_matches %>%
        rowwise() %>%
        filter(setName == sn) %>%
        filter(grepl(beliefSet, bs)) %>%
        filter(year == y) %>%
        filter(distrib == d)
      if (nrow(sheetInfo) > 0) {
        if (sheetInfo$sheet == "public_supplement1") {
          publicSurvey <- as.numeric(unlist(public_supplement1 %>%
            select(all_of(sheetInfo$colName))))
          publicSurvey <- publicSurvey[!is.na(publicSurvey)]
        } else if (sheetInfo$sheet == "public_supplement2") {
          publicSurvey <- as.numeric(unlist(public_supplement2 %>%
            select(all_of(sheetInfo$colName))))
          publicSurvey <- publicSurvey[!is.na(publicSurvey)]
        } else if (sheetInfo$sheet == "public_supplement3") {
          publicSurvey <- as.numeric(unlist(public_supplement3 %>%
            select(all_of(sheetInfo$colName))))
        }
        publicSurvey <- publicSurvey[!is.na(publicSurvey)]
        if (!is.na(forecastMin)) {
          publicSurvey <- publicSurvey[publicSurvey >= forecastMin]
        }
        if (!is.na(forecastMax)) {
          publicSurvey <- publicSurvey[publicSurvey <= forecastMax]
        }
        addPublic <- data.frame(
          group = rep("Public Survey", length(publicSurvey)),
          forecast = publicSurvey
        )
        boxData <- rbind(boxData, addPublic)
      } else {
        print(paste("no sheet info found:", setName, beliefSet, year, distrib))
      }
    }

    boxData$group <- factor(boxData$group, levels = unique(boxData$group), ordered = TRUE)

    rounded_medians <- aggregate(forecast ~ group, median, data = boxData)

    boxPlot <- ggplot(boxData, aes(x = group, y = forecast, color = group)) +
      geom_boxplot(outlier.shape = NA, coef = 0) +
      ylab(yLabel) +
      xlab("Group") +
      labs(title = title, subtitle = subtitle) +
      theme_bw() +
      scale_color_manual(values = unlist(group_colors)) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        axis.title.x = element_blank()
      ) +
      geom_point(position = position_jitterdodge())

    # Add (n=numrows) for the x-axis labels
    boxPlot <- boxPlot +
      scale_x_discrete(
        labels = function(x) {
          x <- as.character(x)
          paste0(x, " (n=", table(boxData$group)[x], ")")
        },
        guide = guide_axis(n.dodge = 2)
      )

    boxPlot$labels$color <- ""
    if (expectedRisk == "low" & forecastMin == 0 && forecastMax == 100) {
      boxPlot <- boxPlot +
        scale_y_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100))
    }
  }

  boxPlot <- boxPlot +
    stat_summary(
      fun.y = median, geom = "label",
      data = rounded_medians,
      aes(label = Round(..y.., 2)),
      position = position_dodge2(width = 0.75, preserve = "single"),
      vjust = 0.5,
      size = 3,
      fill = "white",
      show.legend = FALSE
    )

  tournamentParticipants_95thpctile <- boxData %>%
    filter(group != "Public Survey") %>%
    group_by(group) %>%
    summarize(percentile_95 = quantile(forecast, 0.95))

  boxPlot <- boxPlot +
    coord_cartesian(ylim = c(NA, max(tournamentParticipants_95thpctile$percentile_95)))

  weirdCases <- c("NYT Bestsellers Written by AI", "Generation Attitudes")

  if (title %in% weirdCases) {
    worthPlotting <- boxData %>% filter(forecast < 1e200)
    boxPlot <- boxPlot +
      coord_cartesian(ylim = c(NA, max(worthPlotting$forecast)))
  }

  manualCases <- c("Future Worries and Children")

  if (title %in% manualCases) {
    options(scipen = 7)
  }

  if (dir.exists("BoxPlots")) {
    setwd("BoxPlots")
  } else {
    dir.create("BoxPlots")
    setwd("BoxPlots")
  }

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/", filenameStart, ".png")), boxPlot, width = 9.18, height = 5.78, units = c("in"))
  ggsave(gsub("%", "%%", paste0(file_path, "/", filenameStart, "_vector.ps")), boxPlot, width = 9.18, height = 5.78, units = c("in"))

  if (title %in% manualCases) {
    options(scipen = 999)
  }
}

boxPlot_distrib <- function(tbl, specialty, title, forecastMin, forecastMax,
                            stage, year, numerateCitizens, yLabel, setName, beliefSet, distrib) {
  #' Box Plot for Distribution Questions
  #'
  #' @import ggplot2
  #' @import scales
  #' @export

  boxData <- tbl %>% filter(userName %in% c(supers, expertsG1$userName))
  boxData$answerText <- factor(boxData$answerText, levels = unique(boxData$answerText), ordered = TRUE)
  boxData_supers <- tbl %>% filter(userName %in% supers)
  boxData_supers$answerText <- factor(boxData_supers$answerText, levels = unique(boxData_supers$answerText), ordered = TRUE)
  boxData_experts <- tbl %>% filter(userName %in% expertsG1$userName)
  boxData_experts$answerText <- factor(boxData_experts$answerText, levels = unique(boxData_experts$answerText), ordered = TRUE)
  if (specialty != "") {
    field <- specialty
    specialists <- expertsG1 %>% filter(field %in% c(specialty1, specialty2, specialty3))
    boxData_special <- tbl %>% filter(userName %in% specialists$userName)
    boxData_special$answerText <- factor(boxData_special$answerText, levels = unique(boxData_special$answerText), ordered = TRUE)
  }

  boxPlot <- ggplot(boxData, aes(x = answerText, y = forecast, color = answerText)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    ylab("Forecast") +
    xlab("Percentile") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "All Forecasters (n=", length(unique(boxData$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = unlist(group_colors)) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_point(position = position_jitterdodge()) +
    geom_label(aes(x = 1, y = median((boxData %>% filter(answerText == "5th %"))$forecast), label = median((boxData %>% filter(answerText == "5th %"))$forecast)), color = cb_pal[1]) +
    geom_label(aes(x = 2, y = median((boxData %>% filter(answerText == "25th %"))$forecast), label = median((boxData %>% filter(answerText == "25th %"))$forecast)), color = cb_pal[2]) +
    geom_label(aes(x = 3, y = median((boxData %>% filter(answerText == "50th %"))$forecast), label = median((boxData %>% filter(answerText == "50th %"))$forecast)), color = cb_pal[3]) +
    geom_label(aes(x = 4, y = median((boxData %>% filter(answerText == "75th %"))$forecast), label = median((boxData %>% filter(answerText == "75th %"))$forecast)), color = cb_pal[4]) +
    geom_label(aes(x = 5, y = median((boxData %>% filter(answerText == "95th %"))$forecast), label = median((boxData %>% filter(answerText == "95th %"))$forecast)), color = cb_pal[5])
  if (is.na(forecastMax)) {
    if ((max(boxData$forecast) > 10 * as.numeric(quantile(boxData$forecast, 0.95)))) {
      boxPlot <- boxPlot +
        coord_cartesian(ylim = c(forecastMin, as.numeric(quantile(boxData$forecast, 0.95))))
    } else {
      boxPlot <- boxPlot +
        coord_cartesian(ylim = c(forecastMin, forecastMax))
    }
  } else {
    boxPlot <- boxPlot +
      coord_cartesian(ylim = c(forecastMin, forecastMax))
  }
  boxPlot$labels$color <- ""
  if (year != "") {
    boxPlot$labels$subtitle <- paste0("Stage ", stage, " | ", year, " | All Forecasters (n=", length(unique(boxData$userName)), ")")
  }

  if (grepl("%", boxPlot$data$setName[1])) {
    ggsave(paste0(boxPlot$data$setName[1], "% (All Forecasters) - Stage ", stage, " - Box Plot.png"), boxPlot, width = 9.18, height = 5.78, units = c("in"))
  } else {
    ggsave(paste0(boxPlot$data$setName[1], " (All Forecasters) - Stage ", stage, " - Box Plot.png"), boxPlot, width = 9.18, height = 5.78, units = c("in"))
  }

  boxPlot <- ggplot(boxData_supers, aes(x = answerText, y = forecast, color = answerText)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    ylab("Forecast") +
    xlab("Percentile") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "Superforecasters (n=", length(unique(boxData_supers$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = unlist(group_colors)) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_point(position = position_jitterdodge()) +
    geom_label(aes(x = 1, y = median((boxData_supers %>% filter(answerText == "5th %"))$forecast), label = median((boxData_supers %>% filter(answerText == "5th %"))$forecast)), color = cb_pal[1]) +
    geom_label(aes(x = 2, y = median((boxData_supers %>% filter(answerText == "25th %"))$forecast), label = median((boxData_supers %>% filter(answerText == "25th %"))$forecast)), color = cb_pal[2]) +
    geom_label(aes(x = 3, y = median((boxData_supers %>% filter(answerText == "50th %"))$forecast), label = median((boxData_supers %>% filter(answerText == "50th %"))$forecast)), color = cb_pal[3]) +
    geom_label(aes(x = 4, y = median((boxData_supers %>% filter(answerText == "75th %"))$forecast), label = median((boxData_supers %>% filter(answerText == "75th %"))$forecast)), color = cb_pal[4]) +
    geom_label(aes(x = 5, y = median((boxData_supers %>% filter(answerText == "95th %"))$forecast), label = median((boxData_supers %>% filter(answerText == "95th %"))$forecast)), color = cb_pal[5])
  if (is.na(forecastMax)) {
    if ((max(boxData_supers$forecast) > 10 * as.numeric(quantile(boxData_supers$forecast, 0.95)))) {
      boxPlot <- boxPlot +
        coord_cartesian(ylim = c(forecastMin, as.numeric(quantile(boxData_supers$forecast, 0.95))))
    } else {
      boxPlot <- boxPlot +
        coord_cartesian(ylim = c(forecastMin, forecastMax))
    }
  } else {
    boxPlot <- boxPlot +
      coord_cartesian(ylim = c(forecastMin, forecastMax))
  }
  boxPlot$labels$color <- ""
  if (year != "") {
    boxPlot$labels$subtitle <- paste0("Stage ", stage, " | ", year, " | Superforecasters (n=", length(unique(boxData_supers$userName)), ")")
  }

  if (grepl("%", boxPlot$data$setName[1])) {
    ggsave(paste0(boxPlot$data$setName[1], "% (Superforecasters) - Stage ", stage, " - Box Plot.png"), boxPlot, width = 9.18, height = 5.78, units = c("in"))
  } else {
    ggsave(paste0(boxPlot$data$setName[1], " (Superforecasters) - Stage ", stage, " - Box Plot.png"), boxPlot, width = 9.18, height = 5.78, units = c("in"))
  }

  boxPlot <- ggplot(boxData_experts, aes(x = answerText, y = forecast, color = answerText)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    ylab("Forecast") +
    xlab("Percentile") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "Experts (n=", length(unique(boxData_experts$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = unlist(group_colors)) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_point(position = position_jitterdodge()) +
    geom_label(aes(x = 1, y = median((boxData_experts %>% filter(answerText == "5th %"))$forecast), label = median((boxData_experts %>% filter(answerText == "5th %"))$forecast)), color = cb_pal[1]) +
    geom_label(aes(x = 2, y = median((boxData_experts %>% filter(answerText == "25th %"))$forecast), label = median((boxData_experts %>% filter(answerText == "25th %"))$forecast)), color = cb_pal[2]) +
    geom_label(aes(x = 3, y = median((boxData_experts %>% filter(answerText == "50th %"))$forecast), label = median((boxData_experts %>% filter(answerText == "50th %"))$forecast)), color = cb_pal[3]) +
    geom_label(aes(x = 4, y = median((boxData_experts %>% filter(answerText == "75th %"))$forecast), label = median((boxData_experts %>% filter(answerText == "75th %"))$forecast)), color = cb_pal[4]) +
    geom_label(aes(x = 5, y = median((boxData_experts %>% filter(answerText == "95th %"))$forecast), label = median((boxData_experts %>% filter(answerText == "95th %"))$forecast)), color = cb_pal[5])
  if (is.na(forecastMax)) {
    if ((max(boxData_experts$forecast) > 10 * as.numeric(quantile(boxData_experts$forecast, 0.95)))) {
      boxPlot <- boxPlot +
        coord_cartesian(ylim = c(forecastMin, as.numeric(quantile(boxData_experts$forecast, 0.95))))
    } else {
      boxPlot <- boxPlot +
        coord_cartesian(ylim = c(forecastMin, forecastMax))
    }
  } else {
    boxPlot <- boxPlot +
      coord_cartesian(ylim = c(forecastMin, forecastMax))
  }
  boxPlot$labels$color <- ""
  if (year != "") {
    boxPlot$labels$subtitle <- paste0("Stage ", stage, " | ", year, " | Experts (n=", length(unique(boxData_experts$userName)), ")")
  }

  if (grepl("%", boxPlot$data$setName[1])) {
    ggsave(paste0(boxPlot$data$setName[1], "% (Experts) - Stage ", stage, " - Box Plot.png"), boxPlot, width = 9.18, height = 5.78, units = c("in"))
  } else {
    ggsave(paste0(boxPlot$data$setName[1], " (Experts) - Stage ", stage, " - Box Plot.png"), boxPlot, width = 9.18, height = 5.78, units = c("in"))
  }

  if (specialty != "") {
    if (nrow(boxData_special) > 0) {
      boxPlot <- ggplot(boxData_special, aes(x = answerText, y = forecast, color = answerText)) +
        geom_boxplot(outlier.shape = NA, coef = 0) +
        ylab("Forecast") +
        xlab("Percentile") +
        labs(title = title, subtitle = paste0("Stage ", stage, " | ", specialty, " Specialists (n=", length(unique(boxData_special$userName)), ")")) +
        theme_bw() +
        scale_color_manual(values = unlist(group_colors)) +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_blank()
        ) +
        geom_point(position = position_jitterdodge()) +
        geom_label(aes(x = 1, y = median((boxData_special %>% filter(answerText == "5th %"))$forecast), label = median((boxData_special %>% filter(answerText == "5th %"))$forecast)), color = cb_pal[1]) +
        geom_label(aes(x = 2, y = median((boxData_special %>% filter(answerText == "25th %"))$forecast), label = median((boxData_special %>% filter(answerText == "25th %"))$forecast)), color = cb_pal[2]) +
        geom_label(aes(x = 3, y = median((boxData_special %>% filter(answerText == "50th %"))$forecast), label = median((boxData_special %>% filter(answerText == "50th %"))$forecast)), color = cb_pal[3]) +
        geom_label(aes(x = 4, y = median((boxData_special %>% filter(answerText == "75th %"))$forecast), label = median((boxData_special %>% filter(answerText == "75th %"))$forecast)), color = cb_pal[4]) +
        geom_label(aes(x = 5, y = median((boxData_special %>% filter(answerText == "95th %"))$forecast), label = median((boxData_special %>% filter(answerText == "95th %"))$forecast)), color = cb_pal[5])
      if (is.na(forecastMax)) {
        if ((max(boxData_special$forecast) > 10 * as.numeric(quantile(boxData_special$forecast, 0.95)))) {
          boxPlot <- boxPlot +
            coord_cartesian(ylim = c(forecastMin, as.numeric(quantile(boxData_special$forecast, 0.95))))
        } else {
          boxPlot <- boxPlot +
            coord_cartesian(ylim = c(forecastMin, forecastMax))
        }
      } else {
        boxPlot <- boxPlot +
          coord_cartesian(ylim = c(forecastMin, forecastMax))
      }
      boxPlot$labels$color <- ""
      if (year != "") {
        boxPlot$labels$subtitle <- paste0("Stage ", stage, " | ", year, " | ", specialty, "Specialists (n=", length(unique(boxData_special$userName)), ")")
      }

      if (grepl("%", boxPlot$data$setName[1])) {
        ggsave(paste0(boxPlot$data$setName[1], "% ( ", specialty, " Specialists) - Stage ", stage, " - Box Plot.png"), boxPlot, width = 9.18, height = 5.78, units = c("in"))
      } else {
        ggsave(paste0(boxPlot$data$setName[1], " ( ", specialty, " Specialists) - Stage ", stage, " - Box Plot.png"), boxPlot, width = 9.18, height = 5.78, units = c("in"))
      }
    }
  }
}

boxPlot_distrib_country <- function(tbl, specialty, title, forecastMin,
                                    forecastMax, stage, year) {
  #' Box Plot for Distribution Country Questions
  #'
  #' @import ggplot2
  #' @import scales
  #' @export

  boxData <- tbl %>% filter(userName %in% c(supers, expertsG1$userName))
  boxData$questionName <- factor(boxData$questionName, levels = unique(boxData$questionName), ordered = TRUE)

  if (length(unique(boxData$questionName)) <= 7) {
    countryPal <- cb_pal
  } else {
    countryPal <- c(
      "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
      "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"
    )
  }


  boxPlot <- ggplot(boxData, aes(x = questionName, y = forecast, color = questionName)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    ylab("Forecast") +
    xlab("Country") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "All Forecasters (n=", length(unique(boxData$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = countryPal) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_point(position = position_jitterdodge())
  # for(i in 1:length(unique(boxData$questionName))){
  #   boxPlot = boxPlot +
  #     geom_label(aes(x=i, y=median((boxData %>% filter(questionName == unique(boxData$questionName)[i]))$forecast), label=median((boxData %>% filter(questionName == unique(boxData$questionName)[i]))$forecast)), color=countryPal[i])
  # }
  boxPlot$labels$color <- ""
  if (year != "") {
    boxPlot$labels$subtitle <- paste0("Stage ", stage, " | ", year, " | All Forecasters (n=", length(unique(boxData$userName)), ")")
  }

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/", boxPlot$data$setName[1], " (All Forecasters) - Stage ", stage, " - ", year, " - Box Plot.png")), boxPlot, width = 9.18, height = 5.78, units = c("in"))
}

boxPlot_country <- function(tbl, specialty, title,
                            forecastMin, forecastMax, stage) {
  #' Box Plot for Country Questions
  #'
  #' @import ggplot2
  #' @import scales
  #' @export

  boxData <- tbl %>% filter(userName %in% c(supers, expertsG1$userName))
  boxData$answerText <- factor(boxData$answerText, levels = unique(boxData$answerText), ordered = TRUE)

  if (length(unique(boxData$answerText)) <= 7) {
    countryPal <- cb_pal
  } else {
    countryPal <- c(
      "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
      "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"
    )
  }

  boxPlot <- ggplot(boxData, aes(x = answerText, y = forecast, color = answerText)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    ylab("Forecast") +
    xlab("Country") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "All Forecasters (n=", length(unique(boxData$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = countryPal) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_point(position = position_jitterdodge())
  # for(i in 1:length(unique(boxData$questionName))){
  #   boxPlot = boxPlot +
  #     geom_label(aes(x=i, y=median((boxData %>% filter(questionName == unique(boxData$questionName)[i]))$forecast), label=median((boxData %>% filter(questionName == unique(boxData$questionName)[i]))$forecast)), color=countryPal[i])
  # }
  boxPlot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/", boxPlot$data$setName[1], " (All Forecasters) - Stage ", stage, " - Box Plot.png")), boxPlot, width = 9.18, height = 5.78, units = c("in"))
}

# FUNCTIONS THAT GENERATE THE FIGURES

figureDataInit <- function() {
  #' @export

  statsEmpty <- data.frame(
    beliefSet = character(0),
    year = numeric(0),
    date = Date(0),
    group = character(0),
    n = numeric(0),
    mean = numeric(0),
    # mean_confint_lower = numeric(0),
    # mean_confint_upper = numeric(0),
    sd = numeric(0),
    median = numeric(0),
    # median_confint_lower = numeric(0),
    # median_confint_upper = numeric(0),
    geom_mean = numeric(0),
    # geom_mean_confint_lower = numeric(0),
    # geom_mean_confint_upper = numeric(0),
    hd_trim = numeric(0),
    # hd_trim_confint_lower = numeric(0),
    # hd_trim_confint_upper = numeric(0),
    simple_trim = numeric(0),
    # simple_trim_confint_lower = numeric(0),
    # simple_trim_confint_upper = numeric(0),
    neyman = numeric(0),
    # neyman_confint_lower = numeric(0),
    # neyman_confint_upper = numeric(0),
    geom_mean_of_odds = numeric(0)
    # geom_mean_of_odds_confint_lower = numeric(0),
    # geom_mean_of_odds_confint_upper = numeric(0)
  )

  return(statsEmpty)
}

figureDataBasics <- function(dateDataProcessed, year, beliefSet, setName, subsetUserName = NULL, subsetTeamId = NULL) {
  #' @export
  if (!is.null(subsetUserName)) {
    dateDataProcessed <- dateDataProcessed %>% filter(userName %in% subsetUserName)
  }
  if (!is.null(subsetTeamId)) {
    dateDataProcessed <- dateDataProcessed %>% filter(teamId %in% subsetTeamId)
  }
  # Create the summary stats (only compute confidence intervals for 2100 first 12 questions)
  if (year == 2100 && is.null(subsetTeamId) && (grepl("Extinction", setName) || grepl("Catastrophic", setName) || grepl("Human Births", setName) || grepl("Pathogen Risk", setName))) {
    dateData <- dateDataProcessed %>%
      summarize(
        n = n(),
        mean = mean(forecast),
        # mean_confint_lower = boot_results(forecast, statistic = "mean")$confint_lower,
        # mean_confint_upper = boot_results(forecast, statistic = "mean")$confint_upper,
        sd = sd(forecast),
        median = median(forecast),
        # median_confint_lower = boot_results(forecast, statistic = "median")$confint_lower,
        # median_confint_upper = boot_results(forecast, statistic = "median")$confint_upper,
        geom_mean = geoMeanCalc(forecast),
        # geom_mean_confint_lower = boot_results(forecast, statistic = "geoMeanCalc")$confint_lower,
        # geom_mean_confint_upper = boot_results(forecast, statistic = "geoMeanCalc")$confint_upper,
        hd_trim = hd_trim(forecast),
        # hd_trim_confint_lower = boot_results(forecast, statistic = "hd_trim")$confint_lower,
        # hd_trim_confint_upper = boot_results(forecast, statistic = "hd_trim")$confint_upper,
        simple_trim = trim(forecast),
        # simple_trim_confint_lower = boot_results(forecast, statistic = "trim")$confint_lower,
        # simple_trim_confint_upper = boot_results(forecast, statistic = "trim")$confint_upper,
        neyman = neymanAggCalc(forecast),
        # neyman_confint_lower = boot_results(forecast, statistic = "neymanAggCalc")$confint_lower,
        # neyman_confint_upper = boot_results(forecast, statistic = "neymanAggCalc")$confint_upper,
        geom_mean_of_odds = geoMeanOfOddsCalc(forecast)
        # geom_mean_of_odds_confint_lower = boot_results(forecast, statistic = "geoMeanOfOddsCalc")$confint_lower,
        # geom_mean_of_odds_confint_upper = boot_results(forecast, statistic = "geoMeanOfOddsCalc")$confint_upper
      )
  } else {
    dateData <- dateDataProcessed %>%
      summarize(
        n = n(),
        mean = mean(forecast),
        # mean_confint_lower = NA,
        # mean_confint_upper = NA,
        sd = sd(forecast),
        median = median(forecast),
        # median_confint_lower = NA,
        # median_confint_upper = NA,
        geom_mean = geoMeanCalc(forecast),
        # geom_mean_confint_lower = NA,
        # geom_mean_confint_upper = NA,
        hd_trim = hd_trim(forecast),
        # hd_trim_confint_lower = NA,
        # hd_trim_confint_upper = NA,
        simple_trim = trim(forecast),
        # simple_trim_confint_lower = NA,
        # simple_trim_confint_upper = NA,
        neyman = neymanAggCalc(forecast),
        # neyman_confint_lower = NA,
        # neyman_confint_upper = NA,
        geom_mean_of_odds = geoMeanOfOddsCalc(forecast)
        # geom_mean_of_odds_confint_lower = NA,
        # geom_mean_of_odds_confint_upper = NA
      )
  }

  return(dateData)
}

figureDataMetrics <- function(dateDataProcessed, beliefSet, year, date, qSpecialty, setName) {
  #' @importFrom gdata combine
  #' @export

  everyone <- figureDataBasics(dateDataProcessed, year, beliefSet, setName)

  g1 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, c(supers, expertsG1$userName))

  supers <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, supers)

  experts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, expertsG1$userName)

  generalUsers <- dateDataProcessed %>% filter(userName %in% filter(expertsG1, specialty1 == "General" | specialty2 == "General" | specialty3 == "General")$userName)
  general <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, generalUsers$userName)

  if (qSpecialty != "") {
    domainExpertsUsers <- expertsG1 %>% filter(specialty1 == qSpecialty | specialty2 == qSpecialty | specialty3 == qSpecialty)
    domainExperts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, domainExpertsUsers$userName)
  } else {
    # Create a dataframe with the same columns as the other dataframes
    domainExperts <- data.frame(
      n = NA,
      mean = NA,
      # mean_confint_lower = NA,
      # mean_confint_upper = NA,
      sd = NA,
      median = NA,
      # median_confint_lower = NA,
      # median_confint_upper = NA,
      geom_mean = NA,
      # geom_mean_confint_lower = NA,
      # geom_mean_confint_upper = NA,
      hd_trim = NA,
      # hd_trim_confint_lower = NA,
      # hd_trim_confint_upper = NA,
      simple_trim = NA,
      # simple_trim_confint_lower = NA,
      # simple_trim_confint_upper = NA,
      neyman = NA,
      # neyman_confint_lower = NA,
      # neyman_confint_upper = NA,
      geom_mean_of_odds = NA
      # geom_mean_of_odds_confint_lower = NA,
      # geom_mean_of_odds_confint_upper = NA
    )
  }

  if (qSpecialty != "") {
    nonDomainExpertsUsers <- expertsG1 %>%
      filter(specialty1 != qSpecialty & specialty2 != qSpecialty & specialty3 != qSpecialty) %>%
      filter(specialty1 != "General" & specialty2 != "General" & specialty3 != "General")
    nonDomainExperts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, nonDomainExpertsUsers$userName)
  } else {
    # Create a dataframe with the same columns as the other dataframes
    nonDomainExperts <- data.frame(
      n = NA,
      mean = NA,
      # mean_confint_lower = NA,
      # mean_confint_upper = NA,
      sd = NA,
      median = NA,
      # median_confint_lower = NA,
      # median_confint_upper = NA,
      geom_mean = NA,
      # geom_mean_confint_lower = NA,
      # geom_mean_confint_upper = NA,
      hd_trim = NA,
      # hd_trim_confint_lower = NA,
      # hd_trim_confint_upper = NA,
      simple_trim = NA,
      # simple_trim_confint_lower = NA,
      # simple_trim_confint_upper = NA,
      neyman = NA,
      # neyman_confint_lower = NA,
      # neyman_confint_upper = NA,
      geom_mean_of_odds = NA
      # geom_mean_of_odds_confint_lower = NA,
      # geom_mean_of_odds_confint_upper = NA
    )
  }

  t336 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 336)

  t336Supers <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetUserName = supers, subsetTeamId = 336)

  t336Experts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetUserName = expertsG1$userName, subsetTeamId = 336)

  t337 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 337)

  t337Supers <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 337, subsetUserName = supers)

  t337Experts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 337, subsetUserName = expertsG1$userName)

  t338 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 338)

  t338Supers <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 338, subsetUserName = supers)

  t338Experts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 338, subsetUserName = expertsG1$userName)

  t339 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 339)

  t339Supers <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 339, subsetUserName = supers)

  t339Experts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 339, subsetUserName = expertsG1$userName)

  t340 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 340)

  t340Supers <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 340, subsetUserName = supers)

  t340Experts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 340, subsetUserName = expertsG1$userName)

  t341 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 341)

  t341Supers <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 341, subsetUserName = supers)

  t341Experts <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 341, subsetUserName = expertsG1$userName)

  t342 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 342)

  t343 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 343)

  t344 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 344)

  t345 <- figureDataBasics(dateDataProcessed, year, beliefSet, setName, subsetTeamId = 345)

  rbound <- gdata::combine(
    everyone, g1, supers, experts, general,
    domainExperts, nonDomainExperts,
    t336, t336Supers, t336Experts,
    t337, t337Supers, t337Experts,
    t338, t338Supers, t338Experts,
    t339, t339Supers, t339Experts,
    t340, t340Supers, t340Experts,
    t341, t341Supers, t341Experts,
    t342, t343, t344, t345
  )

  rbound$beliefSet <- beliefSet
  rbound$year <- year
  rbound$currentDate <- date

  rbound <- rbound %>% rename(group = source)

  # If all mean entries for any given group are NA, DROP that group (drops domain and non-domain experts when there's no specialty)
  rbound <- rbound %>%
    group_by(group) %>%
    filter(!all(is.na(mean))) %>%
    ungroup()

  return(rbound)
}

multiYearReciprocalFigureData <- function(metaTable, data, phaseTwoMedian, timeline) {
  #' Multi-year Reciprocal Figure Data
  #'
  #' @export

  print("==GRAPHICS==")
  # minN = 12
  # for (i in 1:length(unique(metaTable$setName))){
  for (i in 1:length(unique(metaTable$setName))) {
    # for (i in 16:length(unique(metaTable$setName))){
    setNames <- unique(metaTable$setName)

    print(setNames[i])
    currentSetName <- setNames[i]

    years <- metaTable[i, ] %>% select(year1, year2, year3)
    years <- as.character(years)
    years <- years[years != ""]
    years <- sort(years, decreasing = FALSE)
    # year = years[1]

    beliefSets <- metaTable[i, ] %>% select(yourBeliefs, expertBeliefs, superBeliefs)
    beliefSets <- as.character(beliefSets)
    beliefSets <- beliefSets[beliefSets != ""]

    qSpecialty <- metaTable[i, ]$specialty

    defaultForecast <- metaTable[i, ]$defaultForecast50

    for (j in 1:length(beliefSets)) {
      # for(j in 1:1){
      print(beliefSets[j])

      # for(k in 1:length(years)){
      for (k in length(years):length(years)) {
        currentSetTimeSeries <- figureDataInit()

        print(years[k])
        currentQuestionName <- paste(years[k], beliefSets[j])

        questionDataRaw <- data %>%
          filter(setName == currentSetName) %>%
          filter(questionName == currentQuestionName) %>%
          filter(forecast != defaultForecast)

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
          rownames(currentSetTimeSeries) <- NULL
          currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = beliefSets[j], year = years[k], date = currentDate, qSpecialty))
        }

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))
        if ("source" %in% colnames(currentSetTimeSeries)) {
          currentSetTimeSeries <- currentSetTimeSeries %>%
            rename(group = source)
        }
        write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", currentQuestionName, ".csv"), row.names = FALSE)

        multiYearReciprocalGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName)
        multiYearReciprocalVarianceGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName)
      }
    }
  }
}

multiYearReciprocalGraphics <- function(title, subtitle, csv, currentSetName) {
  #' @export

  plotTable <- csv %>%
    select(group, year, currentDate, median, n) %>%
    mutate(
      currentDate = ymd(currentDate),
      group = case_when(
        group == "supers" ~ "Superforecasters",
        group == "experts" ~ "Experts",
        group == "domainExperts" ~ "Domain Experts",
        group == "nonDomainExperts" ~ "Non-domain Experts",
        group == "general" ~ "General X-risk Experts"
      )
    ) %>%
    mutate(
      group = factor(group, levels = unique(group), ordered = TRUE),
      median = replace(median, n < 10 | (group == "Non-domain Experts" & n < 4), NA)
    ) %>%
    filter(group %in% c(
      "Superforecasters", "Experts", "Domain Experts",
      "Non-domain Experts", "General X-risk Experts"
    ))

  fname <- paste0(currentSetName, " - Figure One (", csv$year[1], " ", csv$beliefSet[1], ")")
  plot <- plot_with_ribbons(plotTable, paste(title, "by", csv$year[1]), subtitle, phaseTwoMedian, fname)
}

multiYearReciprocalVarianceGraphics <- function(title, subtitle, csv, currentSetName) {
  #' Multi-year Reciprocal Variance Graphics
  #'
  #' @export

  # 1. VARIANCE
  plotTable <- mutate_figure_data_sd(csv)

  subtitle <- "Variance over Time"

  # Table for legend labels
  group_counts <- plotTable %>%
    group_by(group) %>%
    summarise(n = max(n))
  legend_labels <- paste0(group_counts$group, " (n = ", group_counts$n, ")")

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, color = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title, "by", csv$year[1]), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("VARIANCE - ", currentSetName, " - Figure One (", csv$year[1], " ", csv$beliefSet[1], ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
  ggsave(gsub("%", "%%", paste0("VARIANCE - ", currentSetName, " - Figure One (", csv$year[1], " ", csv$beliefSet[1], ")_vector.ps")), plot, width = 9.18, height = 5.78, units = c("in"))

  #####

  # 2. PERCENT VARIANCE

  # Create percent variance column
  plotTable <- plotTable %>%
    group_by(group) %>%
    mutate(
      first_sd = first(sd),
      percent_variance = 100 * (sd / first_sd)
    )

  subtitle <- "Variance over Time"

  plot <- ggplot(plotTable, aes(x = currentDate, y = percent_variance, group = group, color = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title, "by", csv$year[1]), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("PERCENT VARIANCE -", currentSetName, " - Figure One (", csv$year[1], " ", csv$beliefSet[1], ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
  ggsave(gsub("%", "%%", paste0("PERCENT VARIANCE -", currentSetName, " - Figure One (", csv$year[1], " ", csv$beliefSet[1], ")_vector.ps")), plot, width = 9.18, height = 5.78, units = c("in"))
}

pointDistribFigureData <- function(metaTable, data, phaseTwoMedian, timeline) {
  #' Point Distribution Figure Data
  #'
  #' @export

  print("==GRAPHICS==")
  # minN = 12
  for (i in 1:unique(metaTable$setName)) {
    setNames <- unique(metaTable$setName)

    print(setNames[i])
    currentSetName <- setNames[i]

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
      if (
        (currRow[currRow$answerText == "5th %", ]$forecast > currRow[currRow$answerText == "25th %", ]$forecast) |
          (currRow[currRow$answerText == "25th %", ]$forecast > currRow[currRow$answerText == "50th %", ]$forecast) |
          (currRow[currRow$answerText == "50th %", ]$forecast > currRow[currRow$answerText == "75th %", ]$forecast) |
          (currRow[currRow$answerText == "75th %", ]$forecast > currRow[currRow$answerText == "95th %", ]$forecast)
      ) {
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
      print(distrib[j])
      currentAnswerText <- distrib[j]
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

      setwd(paste0(yourHome, "automated analysis/Summary Data/", currentSetName, "/Figure Data"))
      if ("source" %in% colnames(currentSetTimeSeries)) {
        currentSetTimeSeries <- currentSetTimeSeries %>%
          rename(group = source)
      }
      write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", currentAnswerText, ".csv"), row.names = FALSE)

      pointDistribGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, distrib[j])
    }
  }
}

pointDistribGraphics <- function(title, subtitle, csv, currentSetName, distrib) {
  #' Point Distribution Graphics
  #'
  #' @export

  plotTable <- csv %>%
    select(group, year, currentDate, median, n) %>%
    mutate(
      currentDate = ymd(currentDate),
      group = case_when(
        group == "supers" ~ "Superforecasters",
        group == "experts" ~ "Experts",
        group == "domainExperts" ~ "Domain Experts",
        group == "nonDomainExperts" ~ "Non-domain Experts",
        group == "general" ~ "General X-risk Experts"
      )
    ) %>%
    mutate(
      group = factor(group, levels = unique(group), ordered = TRUE),
      median = replace(median, n < 10 | (group == "Non-domain Experts" & n < 4), NA)
    ) %>%
    filter(group %in% c(
      "Superforecasters", "Experts", "Domain Experts",
      "Non-domain Experts", "General X-risk Experts"
    ))

  if (grepl("%", currentSetName)) {
    fname <- paste0(currentSetName, "% - Figure One (", distrib, "%)")
  } else {
    fname <- paste0(currentSetName, " - Figure One (", distrib, "%)")
  }

  plot <- plot_with_ribbons(plotTable, paste(title, "-", distrib), subtitle, phaseTwoMedian, fname)
}

pointDistribVarianceGraphics <- function(title, subtitle, csv, currentSetName, currentDistrib) {
  #' Point Distribution Variance Graphics
  #'
  #' @export

  # 1. VARIANCE
  plotTable <- mutate_figure_data_sd(csv)

  subtitle <- "Variance over Time"

  # Table for legend labels
  group_counts <- plotTable %>%
    group_by(group) %>%
    summarise(n = max(n))
  legend_labels <- paste0(group_counts$group, " (n = ", group_counts$n, ")")

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, color = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/VARIANCE - ", currentSetName, " - Figure One (", currentDistrib, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  # 2. PERCENT VARIANCE

  # Create percent variance column
  plotTable <- plotTable %>%
    group_by(group) %>%
    mutate(
      first_sd = first(sd),
      percent_variance = 100 * (sd / first_sd)
    )

  subtitle <- "Variance over Time"

  plot <- ggplot(plotTable, aes(x = currentDate, y = percent_variance, group = group, color = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors))
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/PERCENT VARIANCE - ", currentSetName, " - Figure One (", currentDistrib, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiYearDistribFigureData <- function(metaTable, data, phaseTwoMedian, timeline) {
  #' Multi-year Distribution Figure Data
  #'
  #' @export

  print("==GRAPHICS==")
  # minN = 12
  for (i in 1:length(unique(metaTable$setName))) {
    # for (i in 22:length(unique(metaTable$setName))){

    setNames <- unique(metaTable$setName)

    print(setNames[i])
    currentSetName <- setNames[i]

    years <- metaTable[i, ] %>% select(year1, year2, year3)
    years <- as.character(years)
    years <- years[years != ""]
    years <- sort(years, decreasing = FALSE)
    # year = years[1]

    qSpecialty <- metaTable[i, ]$specialty

    distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

    preQRaw <- data %>% filter(setName == currentSetName)

    uniqueForecasts <- unique(data %>% filter(setName == currentSetName) %>% select(userName, timestamp))
    exclude <- data.frame(row.names = names(uniqueForecasts))

    # rm non monotonic
    for (j in 1:nrow(uniqueForecasts)) {
      # print(j)
      currRow <- preQRaw %>%
        filter(userName == uniqueForecasts[j, ]$userName) %>%
        filter(timestamp %in% seq(uniqueForecasts[j, ]$timestamp - 60, uniqueForecasts[j, ]$timestamp + 60, 1))
      if (
        (currRow[currRow$answerText == "5th %", ]$forecast > currRow[currRow$answerText == "25th %", ]$forecast) |
          (currRow[currRow$answerText == "25th %", ]$forecast > currRow[currRow$answerText == "50th %", ]$forecast) |
          (currRow[currRow$answerText == "50th %", ]$forecast > currRow[currRow$answerText == "75th %", ]$forecast) |
          (currRow[currRow$answerText == "75th %", ]$forecast > currRow[currRow$answerText == "95th %", ]$forecast)
      ) {
        exclude <- rbind(exclude, uniqueForecasts[j, ])
      }
    }

    if (ncol(exclude) > 0) {
      for (j in 1:nrow(exclude)) {
        removeRow <- preQRaw %>%
          filter(userName == exclude[j, ]$userName) %>%
          filter(timestamp == exclude[j, ]$timestamp)
        for (k in 1:nrow(removeRow)) {
          currRow <- removeRow[k, ]
          preQRaw <- preQRaw[!(preQRaw$userName == currRow$userName & preQRaw$timestamp == currRow$timestamp & preQRaw$answerText == currRow$answerText)]
        }
      }
    }

    for (j in 1:length(years)) {
      print(years[j])

      for (k in 1:length(distrib)) {
        print(distrib[k])
        currentAnswerText <- distrib[k]
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
          filter(questionName == years[j]) %>%
          filter(answerText == distrib[k]) %>%
          filter(forecast != defaultForecast)

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

          currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = years[j], date = currentDate, qSpecialty))
        }

        if (i != 22) {
          setwd(paste0(yourHome, "automated analysis/Summary Data/", currentSetName, "/Figure Data"))
        } else {
          setwd(paste0(yourHome, "automated analysis/Summary Data/40. Massive Multitask Language Understanding Benchmark/Figure Data/"))
        }
        if ("source" %in% colnames(currentSetTimeSeries)) {
          currentSetTimeSeries <- currentSetTimeSeries %>%
            rename(group = source)
        }
        write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", years[j], " - ", currentAnswerText, ".csv"), row.names = FALSE)

        multiYearDistribGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j], distrib = distrib[k])
      }
    }
  }
}

multiYearDistribGraphics <- function(title, subtitle, csv, currentSetName, year, currentDistrib) {
  #' Multi-Year Distribution Graphics
  #'
  #' @export

  plotTable <- mutate_figure_data_median(csv)
  file_path <- getwd()
  if (grepl("%", currentSetName)) {
    fname <- paste0(currentSetName, "% - Figure One (", year, " - ", currentDistrib, "%)")
  } else {
    fname <- paste0(currentSetName, " - Figure One (", year, "-", currentDistrib, "%)")
  }

  plot <- plot_with_ribbons(plotTable, paste(title, "-", year, "-", currentDistrib), subtitle, phaseTwoMedian, fname)
}

multiYearDistribVarianceGraphics <- function(title, subtitle, csv, currentSetName, year, currentDistrib) {
  #' Multi-Year Distribution Graphics
  #'
  #' @export

  # 1. VARIANCE
  plotTable <- mutate_figure_data_sd(csv)

  subtitle <- "Variance over Time"

  # Table for legend labels
  group_counts <- plotTable %>%
    group_by(group) %>%
    summarise(n = max(n))
  legend_labels <- paste0(group_counts$group, " (n = ", group_counts$n, ")")

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, color = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title, "-", year, "-", currentDistrib), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/VARIANCE - ", currentSetName, " - Figure One (", currentDistrib, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  # 2. PERCENT VARIANCE

  # Create percent variance column
  plotTable <- plotTable %>%
    group_by(group) %>%
    mutate(
      first_sd = first(sd),
      percent_variance = 100 * (sd / first_sd)
    )

  subtitle <- "Variance over Time"

  plot <- ggplot(plotTable, aes(x = currentDate, y = percent_variance, group = group, color = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title, "-", year, "-", currentDistrib), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/PERCENT VARIANCE - ", currentSetName, " - Figure One (", currentDistrib, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiYearBinaryFigureData <- function(metaTable, data, phaseTwoMedian, timeline) {
  #' Multi-year Binary Figure Data
  #'
  #' @export

  print("==GRAPHICS==")
  # minN = 12
  for (i in 1:length(unique(metaTable$setName))) {
    setNames <- unique(metaTable$setName)

    print(setNames[i])
    currentSetName <- setNames[i]

    years <- metaTable[i, ] %>% select(year1, year2, year3)
    years <- as.character(years)
    years <- years[years != ""]
    years <- sort(years, decreasing = FALSE)
    # year = years[1]

    qSpecialty <- metaTable[i, ]$specialty

    defaultForecast <- metaTable[i, ]$defaultForecast50

    for (j in 1:length(years)) {
      print(years[j])

      currentSetTimeSeries <- figureDataInit()

      questionDataRaw <- data %>%
        filter(setName == currentSetName) %>%
        filter(questionName == years[j]) %>%
        filter(forecast != defaultForecast)

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

        currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = years[j], date = currentDate, qSpecialty))
      }

      setwd(paste0(yourHome, "automated analysis/Summary Data/", currentSetName, "/Figure Data"))
      if ("source" %in% colnames(currentSetTimeSeries)) {
        currentSetTimeSeries <- currentSetTimeSeries %>%
          rename(group = source)
      }
      write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", years[j], ".csv"), row.names = FALSE)

      multiYearBinaryGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j])
    }
  }
}

multiYearBinaryGraphics <- function(title, subtitle, csv, currentSetName, year) {
  #' Multi-year Binary Graphics
  #'
  #' @export

  plotTable <- mutate_figure_data_median(csv)
  fname <- paste0(currentSetName, " - Figure One (", year, ")")
  plot <- plot_with_ribbons(plotTable, paste(title, "-", year), subtitle, phaseTwoMedian, fname)
}

multiYearBinaryVarianceGraphics <- function(title, subtitle, csv, currentSetName, year) {
  #' Multi-year Binary Graphics
  #'
  #' @export

  # 1. VARIANCE
  plotTable <- mutate_figure_data_sd(csv)

  subtitle <- "Variance over Time"

  # Table for legend labels
  group_counts <- plotTable %>%
    group_by(group) %>%
    summarise(n = max(n))
  legend_labels <- paste0(group_counts$group, " (n = ", group_counts$n, ")")

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, color = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/VARIANCE - ", currentSetName, " - Figure One (", year, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  # 2. PERCENT VARIANCE

  # Create percent variance column
  plotTable <- plotTable %>%
    group_by(group) %>%
    mutate(
      first_sd = first(sd),
      percent_variance = 100 * (sd / first_sd)
    )

  subtitle <- "Variance over Time"

  plot <- ggplot(plotTable, aes(x = currentDate, y = percent_variance, group = group, color = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/PERCENT VARIANCE - ", currentSetName, " - Figure One (", year, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiYearCountryDistribGraphics <- function(title, subtitle, csv, currentSetName, year, country) {
  #' Multi-Year Country Distribution Graphics
  #'
  #' @export

  plotTable <- mutate_figure_data_median(csv)
  fname <- paste0(currentSetName, " - Figure One (", year, ")")
  plot <- plot_with_ribbons(plotTable, paste(title, "-", country, "-", year), subtitle, phaseTwoMedian, fname)
}

multiYearCountryVarianceGraphics <- function(title, subtitle, csv, currentSetName, year, country) {
  #' Multi-Year Country Variance Graphics
  #'
  #' @export

  # 1. VARIANCE
  plotTable <- mutate_figure_data_sd(csv)

  subtitle <- "Variance over Time"

  # Table for legend labels
  group_counts <- plotTable %>%
    group_by(group) %>%
    summarise(n = max(n))
  legend_labels <- paste0(group_counts$group, " (n = ", group_counts$n, ")")

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, color = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title, "-", country, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/VARIANCE - ", currentSetName, " - Figure One (", year, " ", country, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  # 2. PERCENT VARIANCE

  # Create percent variance column
  plotTable <- plotTable %>%
    group_by(group) %>%
    mutate(
      first_sd = first(sd),
      percent_variance = 100 * (sd / first_sd)
    )

  subtitle <- "Variance over Time"

  plot <- ggplot(plotTable, aes(x = currentDate, y = percent_variance, group = group, color = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title, "-", country, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/PERCENT VARIANCE - ", currentSetName, " - Figure One (", year, country, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiCountryBinaryGraphics <- function(title, subtitle, csv, currentSetName, country) {
  #' Multi-Country Binary Graphics
  #'
  #' @export

  plotTable <- mutate_figure_data_median(csv)
  fname <- paste0(currentSetName, " - Figure One (", country, ")")
  plot <- plot_with_ribbons(plotTable, paste(title, "-", country), subtitle, phaseTwoMedian, fname)
}

pointBinaryFigureData <- function(metaTable, data, phaseTwoMedian, timeline) {
  #' Point Binary Figure Data
  #'
  #' @export

  print("==GRAPHICS==")

  for (i in 1:length(unique(metaTable$setName))) {
    setNames <- unique(metaTable$setName)

    print(setNames[i])
    currentSetName <- setNames[i]

    qSpecialty <- metaTable[i, ]$specialty

    defaultForecast <- metaTable[i, ]$defaultForecast50

    currentSetTimeSeries <- figureDataInit()

    questionDataRaw <- data %>%
      filter(setName == currentSetName) %>%
      filter(forecast != defaultForecast)

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

    setwd(paste0(yourHome, "automated analysis/Summary Data/", currentSetName, "/Figure Data"))
    if ("source" %in% colnames(currentSetTimeSeries)) {
      currentSetTimeSeries <- currentSetTimeSeries %>%
        rename(group = source)
    }
    write.csv(currentSetTimeSeries, paste0(currentSetName, ".csv"), row.names = FALSE)

    pointBinaryGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName)
  }
}

pointBinaryGraphics <- function(title, subtitle, csv, currentSetName) {
  #' Point Binary Graphics
  #'
  #' @export

  plotTable <- mutate_figure_data_median(csv)
  fname <- paste0(currentSetName, " - Figure One")
  plot <- plot_with_ribbons(plotTable, title, subtitle, phaseTwoMedian, fname)
}

pointBinaryVarianceGraphics <- function(title, subtitle, csv, currentSetName) {
  #' Point Binary Variance Graphics
  #'
  #' @export

  # 1. VARIANCE
  plotTable <- mutate_figure_data_sd(csv)

  subtitle <- "Variance over Time"

  # Table for legend labels
  group_counts <- plotTable %>%
    group_by(group) %>%
    summarise(n = max(n))
  legend_labels <- paste0(group_counts$group, " (n = ", group_counts$n, ")")

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, color = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/VARIANCE - ", currentSetName, " - Figure One.png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  # 2. PERCENT VARIANCE

  # Create percent variance column
  plotTable <- plotTable %>%
    group_by(group) %>%
    mutate(
      first_sd = first(sd),
      percent_variance = 100 * (sd / first_sd)
    )

  subtitle <- "Variance over Time"

  plot <- ggplot(plotTable, aes(x = currentDate, y = percent_variance, group = group, color = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = unlist(group_colors), labels = legend_labels)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/PERCENT VARIANCE - ", currentSetName, " - Figure One.png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

pointBinaryGraphics_custom <- function(title, csv, currentSetName) {
  plotTable <- mutate_figure_data_hd_trim(csv)
  plot <- ggplot(plotTable, aes(x = currentDate, y = hd_trim, group = group, color = group)) +
    geom_line() +
    ylab("HD Trim") +
    xlab("Date") +
    labs(title = paste(title)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")

  plot$labels$group <- ""
  plot$labels$color <- ""

  if (grepl("%", currentSetName)) {
    ggsave(paste0(currentSetName, "% - Figure One.png"), plot, width = 9.18, height = 5.78, units = c("in"))
  } else {
    ggsave(paste0(currentSetName, " - Figure One.png"), plot, width = 9.18, height = 5.78, units = c("in"))
  }
}

pointDistribGraphics_custom <- function(title, subtitle, csv, currentSetName, distrib) {
  plotTable <- mutate_figure_data_hd_trim(csv)
  plot <- ggplot(plotTable, aes(x = currentDate, y = hd_trim, group = group, color = group)) +
    geom_line() +
    ylab("HD Trim") +
    xlab("Date") +
    labs(title = paste(title)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")

  plot$labels$color <- ""

  if (grepl("%", currentSetName)) {
    ggsave(paste0(currentSetName, "% - Figure One (", distrib, "%).png"), plot, width = 9.18, height = 5.78, units = c("in"))
  } else {
    ggsave(paste0(currentSetName, " - Figure One (", distrib, "%).png"), plot, width = 9.18, height = 5.78, units = c("in"))
  }
}

multiYearReciprocal_teams <- function(metaTable, data) {
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

    if (dir.exists("Figure Data")) {
      setwd("Figure Data")
    } else {
      dir.create("Figure Data")
      setwd("Figure Data")
    }

    qSpecialty <- metaTable[i, ]$specialty

    for (j in 1:length(beliefSets)) {
      # for(j in 1:1){
      print(beliefSets[j])

      for (k in 1:length(years)) {
        print(years[k])
        currentQuestionName <- paste(years[k], beliefSets[j])

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[k], "/", beliefSets[j]))

        files <- list.files()
        csv <- read.csv(files[grepl(".csv", files)])

        if (dir.exists("Team Graphics")) {
          setwd("Team Graphics")
        } else {
          dir.create("Team Graphics")
          setwd("Team Graphics")
        }

        multiYearReciprocalTeamGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv, currentSetName)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[k], "/", beliefSets[j]))
      }
    }
  }
}

multiYearReciprocalTeamGraphics <- function(title, subtitle, csv, currentSetName) {
  #' @title Team Graphics: Multi-year Reciprocal Questions
  #'
  #' @description All members of all teams
  #'
  #' @export

  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )

  teams <- c(336, 337, 338, 339, 340, 341, 342, 343, 344, 345)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "Median"), paste0("t", teams[i], "N"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], "% | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  fname <- paste0(currentSetName, " - Teams [All] (", csv$year[1], " ", csv$beliefSet[1], ")")
  plot <- plot_with_ribbons(plotTable, paste(title, "by", csv$year[1]), subtitle, phaseTwoMedian, fname)

  # Supers
  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )
  teams <- c(336, 337, 338, 339, 340, 341)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "SupersMedian"), paste0("t", teams[i], "SupersN"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], "% | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  fname <- paste0(currentSetName, " - Teams [Supers] (", csv$year[1], " ", csv$beliefSet[1], ")")
  plot <- plot_with_ribbons(plotTable, paste(title, "by", csv$year[1], "(Supers)"), subtitle, phaseTwoMedian, fname)

  # Experts
  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )
  teams <- c(336, 337, 338, 339, 340, 341)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "ExpertsMedian"), paste0("t", teams[i], "ExpertsN"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], "% | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  fname <- paste0(currentSetName, " - Teams [Experts] (", csv$year[1], " ", csv$beliefSet[1], ")")
  plot <- plot_with_ribbons(plotTable, paste(title, "by", csv$year[1], "(Experts)"), subtitle, phaseTwoMedian, fname)
}

pointDistrib_teams <- function(metaTable, data) {
  distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

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

      files <- list.files()
      csv <- read.csv(files[grepl(".csv", files)])

      if (dir.exists("Team Graphics")) {
        setwd("Team Graphics")
      } else {
        dir.create("Team Graphics")
        setwd("Team Graphics")
      }

      pointDistribTeamGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv, currentSetName, distrib = distrib[j])

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))
    }
  }
}

pointDistribTeamGraphics <- function(title, subtitle, csv, currentSetName, distrib) {
  #' @title Team Graphics: Point Distribution Questions
  #'
  #' @description All members of all teams
  #'
  #' @export

  # All members of all teams
  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )

  teams <- c(336, 337, 338, 339, 340, 341, 342, 343, 344, 345)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "Median"), paste0("t", teams[i], "N"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], " | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  fname <- paste0(currentSetName, " - Teams [All] (", distrib, "%)")
  plot <- plot_with_ribbons(plotTable, paste(title, "(All)"), distrib, phaseTwoMedian, fname)

  # Supers
  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )
  teams <- c(336, 337, 338, 339, 340, 341)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "SupersMedian"), paste0("t", teams[i], "SupersN"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], " | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  fname <- paste0(currentSetName, " - Teams [Supers] (", distrib, "%)")
  plot <- plot_with_ribbons(plotTable, paste(title, "(Supers)"), distrib, phaseTwoMedian, fname)

  # Experts
  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )
  teams <- c(336, 337, 338, 339, 340, 341)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "ExpertsMedian"), paste0("t", teams[i], "ExpertsN"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], " | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  fname <- paste0(currentSetName, " - Teams [Experts] (", distrib, "%)")
  plot <- plot_with_ribbons(plotTable, paste(title, "(Experts)"), distrib, phaseTwoMedian, fname)
}

multiYearDistrib_teams <- function(metaTable, data) {
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

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    specialty <- metaTable[i, ]$specialty

    expectedRisk <- metaTable[i, ]$expectedRisk
    forecastMin <- metaTable[i, ]$forecastMin
    forecastMax <- metaTable[i, ]$forecastMax

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

        files <- list.files()
        csv <- read.csv(files[grepl(".csv", files)])

        if (dir.exists("Team Graphics")) {
          setwd("Team Graphics")
        } else {
          dir.create("Team Graphics")
          setwd("Team Graphics")
        }

        multiYearDistribTeamGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv, currentSetName, year = years[j], distrib = distrib[k])

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data/", years[j]))
      }
    }
  }
}

multiYearDistribTeamGraphics <- function(title, subtitle, csv, currentSetName, distrib, year) {
  #' @title Multi-Year Distribution Questions: Team Graphics
  #'
  #' @description All members of all teams
  #'
  #' @export

  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )

  teams <- c(336, 337, 338, 339, 340, 341, 342, 343, 344, 345)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "Median"), paste0("t", teams[i], "N"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], " | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plotTable <- plotTable %>%
    rename(confint_lower = contains("confint_lower"), confint_upper = contains("confint_upper"))

  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group, color = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2, color = "transparent") +
    ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(All)"), subtitle = paste(year, distrib)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = unlist(group_colors)) +
    scale_fill_manual(values = unlist(group_colors)) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    xlim(phaseTwoMedian, NA) +
    ylim(NA, as.numeric(quantile(plotTable$median, 0.95, na.rm = TRUE)))
  plot$labels$color <- ""

  ggsave(paste0(currentSetName, " - Teams [All] (", year, " ", distrib, "%).png"), plot, width = 9.18, height = 5.78, units = c("in"))

  # Supers
  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )
  teams <- c(336, 337, 338, 339, 340, 341)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "SupersMedian"), paste0("t", teams[i], "SupersN"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], " | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plotTable <- plotTable %>%
    rename(confint_lower = contains("confint_lower"), confint_upper = contains("confint_upper"))

  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group, color = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2, color = "transparent") +
    ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(Supers)"), subtitle = paste(year, distrib)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = unlist(group_colors)) +
    scale_fill_manual(values = unlist(group_colors)) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    xlim(phaseTwoMedian, NA) +
    ylim(NA, as.numeric(quantile(plotTable$median, 0.95, na.rm = TRUE)))
  plot$labels$color <- ""

  ggsave(paste0(currentSetName, " - Teams [Supers] (", year, " ", distrib, "%).png"), plot, width = 9.18, height = 5.78, units = c("in"))

  # Experts
  plotTable <- data.frame(
    currentDate = Date(0),
    median = numeric(0),
    n = numeric(0),
    group = character(0)
  )
  teams <- c(336, 337, 338, 339, 340, 341)
  for (i in 1:length(teams)) {
    currGroup <- csv %>% select(currentDate, paste0("t", teams[i], "ExpertsMedian"), paste0("t", teams[i], "ExpertsN"))
    currGroup <- currGroup %>% mutate(group = paste0("Team ", teams[i], " (", currGroup[nrow(currGroup), 2], " | n=", currGroup[nrow(currGroup), 3], ")"))
    names(currGroup) <- c("currentDate", "median", "n", "group")
    plotTable <- rbind(plotTable, currGroup)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plotTable <- plotTable %>%
    rename(confint_lower = contains("confint_lower"), confint_upper = contains("confint_upper"))

  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group, color = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2, color = "transparent") +
    ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(Experts)"), subtitle = paste(year, distrib)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = unlist(group_colors)) +
    scale_fill_manual(values = unlist(group_colors)) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    xlim(phaseTwoMedian, NA) +
    ylim(NA, as.numeric(quantile(plotTable$median, 0.95, na.rm = TRUE)))
  plot$labels$color <- ""

  ggsave(paste0(currentSetName, " - Teams [Experts] (", year, " ", distrib, "%).png"), plot, width = 9.18, height = 5.78, units = c("in"))
}

salienceGraphics <- function(salienceTbl, title, subtitle, specialty) {
  #' Salience Graphics
  #'
  #' @description Salience graphics for supers+experts+overall
  #'
  #' @export

  plotTable <- data.frame(
    date = Date(0),
    opt = numeric(0),
    group = character(0)
  )

  supersTable <- salienceTbl %>%
    select(date, supersOpt_g1) %>%
    mutate(group = "Superforecasters")
  names(supersTable) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, supersTable)

  expertsTable <- salienceTbl %>%
    select(date, expertsOpt_g1) %>%
    mutate(group = "Experts")
  names(expertsTable) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, expertsTable)

  overallTable <- salienceTbl %>%
    select(date, g1Opt) %>%
    mutate(group = "Overall")
  names(overallTable) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, overallTable)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = date, y = opt, group = group, fill = group)) +
    geom_line() +
    ylab("Optimism (vs end of Stage 1)") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    scale_color_manual(values = c(cb_pal[1:2], "black")) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    geom_hline(yintercept = 0)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/", title, " OVERALL SALIENCE.png")), plot, width = 9.18, height = 5.78, units = c("in"))

  if (specialty != "") {
    # Supers vs Specialty
    plotTable <- data.frame(
      date = Date(0),
      opt = numeric(0),
      group = character(0)
    )

    supersTable <- salienceTbl %>%
      select(date, supersOpt_g1) %>%
      mutate(group = "Superforecasters")
    names(supersTable) <- c("date", "opt", "group")
    plotTable <- rbind(plotTable, supersTable)

    dExpertsTable <- salienceTbl %>%
      select(date, dExpertsOpt_g1) %>%
      mutate(group = "Domain Experts")
    names(dExpertsTable) <- c("date", "opt", "group")
    plotTable <- rbind(plotTable, dExpertsTable)

    ndExpertsTable <- salienceTbl %>%
      select(date, ndExpertsOpt_g1) %>%
      mutate(group = "Generalists")
    names(ndExpertsTable) <- c("date", "opt", "group")
    plotTable <- rbind(plotTable, ndExpertsTable)

    plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

    plot <- ggplot(plotTable, aes(x = date, y = opt, group = group, fill = group)) +
      geom_line() +
      ylab("Optimism (vs end of Stage 1)") +
      xlab("Date") +
      labs(title = paste(title), subtitle = subtitle) +
      theme_bw() +
      scale_color_manual(values = c(cb_pal)) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank()
      ) +
      geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
      geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
      geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
      geom_hline(yintercept = 0)
    plot$labels$color <- ""

    file_path <- getwd()
    ggsave(gsub("%", "%%", paste0(file_path, "/", title, " SUPERS VS DOMAIN EXP VS GENERALISTS SALIENCE.png")), plot, width = 9.18, height = 5.78, units = c("in"))
  }

  # Teams
  plotTable <- data.frame(
    date = Date(0),
    opt = numeric(0),
    group = character(0)
  )

  t336Table <- salienceTbl %>%
    select(date, t336Opt) %>%
    mutate(group = "Team 336")
  names(t336Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t336Table)

  t337Table <- salienceTbl %>%
    select(date, t337Opt) %>%
    mutate(group = "Team 337")
  names(t337Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t337Table)

  t338Table <- salienceTbl %>%
    select(date, t338Opt) %>%
    mutate(group = "Team 338")
  names(t338Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t338Table)

  t339Table <- salienceTbl %>%
    select(date, t339Opt) %>%
    mutate(group = "Team 339")
  names(t339Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t339Table)

  t340Table <- salienceTbl %>%
    select(date, t340Opt) %>%
    mutate(group = "Team 340")
  names(t340Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t340Table)

  t341Table <- salienceTbl %>%
    select(date, t341Opt) %>%
    mutate(group = "Team 341")
  names(t341Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t341Table)

  t342Table <- salienceTbl %>%
    select(date, t342Opt) %>%
    mutate(group = "Team 342")
  names(t342Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t342Table)

  t343Table <- salienceTbl %>%
    select(date, t343Opt) %>%
    mutate(group = "Team 343")
  names(t343Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t343Table)

  t344Table <- salienceTbl %>%
    select(date, t344Opt) %>%
    mutate(group = "Team 344")
  names(t344Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t344Table)

  t345Table <- salienceTbl %>%
    select(date, t345Opt) %>%
    mutate(group = "Team 345")
  names(t345Table) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, t345Table)

  overallTable <- salienceTbl %>%
    select(date, overallOpt) %>%
    mutate(group = "Overall")
  names(overallTable) <- c("date", "opt", "group")
  plotTable <- rbind(plotTable, overallTable)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = date, y = opt, group = group, fill = group)) +
    geom_line() +
    ylab("Optimism (vs end of Stage 1)") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    scale_color_manual(values = c(cb_pal(10), "black")) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    geom_hline(yintercept = 0)
  plot$labels$color <- ""

  file_path <- getwd()
  ggsave(gsub("%", "%%", paste0(file_path, "/", title, " TEAMS SALIENCE.png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

rs_quintile_plot <- function(tbl, title, subtitle) {
  #' Boxplot for RS quintiles
  #'
  #' @importFrom ncar Round
  #' @export

  # plot <- ggplot(tbl, aes(x = quintile, y = forecast, group = quintile)) +
  plot <- ggplot(tbl, aes(x = quintile, y = forecast, color = userType)) +
    geom_boxplot(outlier.shape = NA, coef = 0) +
    ylab("Forecast") +
    xlab("Quintile") +
    labs(title = title, subtitle = subtitle) +
    theme_bw() +
    coord_trans(y = pseudo_log_trans(base = 10), ylim = c(0, 100)) +
    scale_y_continuous(breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100)) +
    scale_color_manual(values = unlist(group_colors)) +
    # scale_fill_manual(values = unlist(group_colors)) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    geom_point(position = position_jitterdodge(), aes(x = quintile, y = forecast, group = userType)) +
    stat_summary(
      fun.y = median, geom = "label", aes(label = Round(..y.., 4)),
      position = position_dodge2(width = 0.75, preserve = "single"),
      vjust = 0.5,
      size = 3,
      fill = "white",
      show.legend = FALSE
    )

  plot$labels$colour <- ""
  plot$labels$fill <- ""

  return(plot)
}
