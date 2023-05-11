library(dplyr)
library(docstring)
library(ggthemes)
library(lubridate)
library(boot)
library(ggplot2)
library(scales)

# Generate a colorblind-friendly palette with six colors
cb_pal <- colorblind_pal()(6)

# Exclude black from the palette
cb_pal <- tail(cb_pal, -1)


boot_results <- function(plotTable, statistic = median, width = 0.95) {
  #' Get bootstrapped confidence intervals
  #' 
  #' `do` function applies the `boot` function to each combination of (group,
  #' currentDate) and creates a new dataframe with a row for each combination...
  #' Assumes this is being called from within figureDataMetrics (one question,
  #' one group at a time)
  #'
  #'  @export
  
  set.seed(123)
  interval <- plotTable %>%
    do({
      x <- .$forecast
      res <- boot(x, statistic = function(x,i) statistic(x[i]), R = 1000)
      a <- boot.ci(res, conf = width, type = "perc")
      data.frame(confint_lower = a$percent[4], confint_upper = a$percent[5])
    })
  return(interval)
}

histogram <- function(questionDataProcessed, filenameStart, title, stage,
                      specialty, expectedRisk, forecastMin, forecastMax) {
  #' Histogram
  #'
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
      legend.title = element_text(size = 9, vjust = -37)
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
      legend.title = element_text(size = 9, vjust = -37)
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
      legend.title = element_text(size = 9, vjust = -37)
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
        legend.title = element_text(size = 9, vjust = -37)
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
                    expectedRisk, forecastMin, forecastMax) {
  #' Basic boxplot function
  #'
  #' @export

  tbl <- read.csv(files[1])
  if (type == "distrib") {
    # FOR LOOP TO ADD TO TBL AND REST
  }

  if (type == "regGroups") {
    boxData_supers <- tbl %>% filter(userName %in% supers)
    boxData <- boxData_supers %>% mutate(group = paste0("Superforecasters (n=", nrow(boxData_supers), ")"))
    boxData_experts <- tbl %>% filter(userName %in% expertsG1$userName)
    if (specialty != "") {
      field <- specialty
      specialists <- expertsG1 %>% filter(field == specialty1 | field == specialty2 | field == specialty3)
      boxData_special <- tbl %>% filter(userName %in% specialists$userName)
      boxData <- rbind(boxData, boxData_special %>% mutate(group = paste0(field, " Experts (n=", nrow(boxData_special), ")")))
    }
    boxData <- rbind(boxData, boxData_experts %>% mutate(group = paste0("Non-domain Experts (n=", nrow(boxData_experts), ")")))
    boxData_general <- tbl %>% filter(userName %in% filter(expertsG1, specialty1 == "General" | specialty2 == "General" | specialty3 == "General")$userName)
    boxData <- rbind(boxData, boxData_general %>% mutate(group = paste0("General X-risk Experts (n=", nrow(boxData_general), ")")))

    boxData$group <- factor(boxData$group, levels = unique(boxData$group), ordered = TRUE)

    boxPlot <- ggplot(boxData, aes(x = group, y = forecast, fill = group)) +
      geom_boxplot(outlier.shape = NA) +
      ylab("Forecast") +
      xlab("Group") +
      labs(title = title, subtitle = subtitle) +
      theme_bw() +
      scale_fill_manual(values = cb_pal) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        axis.title.x = element_blank()) +
      geom_point(position = position_jitterdodge())
    # If there are domain experts, stick them in between supers and general x-risk experts
    if (specialty != "") {
      boxPlot <- boxPlot +
        geom_label(aes(x = 1, y = median(boxData_supers$forecast), label = median(boxData_supers$forecast), fill = "white")) +
        geom_label(aes(x = 2, y = median(boxData_special$forecast), label = median(boxData_special$forecast), fill = "white")) +
        geom_label(aes(x = 3, y = median(boxData_experts$forecast), label = median(boxData_experts$forecast), fill = "white")) +
        geom_label(aes(x = 4, y = median(boxData_general$forecast), label = median(boxData_general$forecast), fill = "white"))
    } else {
      boxPlot <- boxPlot +
        geom_label(aes(x = 1, y = median(boxData_supers$forecast), label = median(boxData_supers$forecast), fill = "white")) +
        geom_label(aes(x = 2, y = median(boxData_experts$forecast), label = median(boxData_experts$forecast), fill = "white")) +
        geom_label(aes(x = 3, y = median(boxData_general$forecast), label = median(boxData_general$forecast), fill = "white"))
    }
    boxPlot$labels$color <- ""
    if (expectedRisk == "low" & forecastMin == 0 && forecastMax == 100) {
      boxPlot <- boxPlot +
        scale_y_continuous(trans = pseudo_log_trans(base = 10), breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100), limits = c(0, 100))
    }
  }

  if (dir.exists("BoxPlots")) {
    setwd("BoxPlots")
  } else {
    dir.create("BoxPlots")
    setwd("BoxPlots")
  }

  ggsave(gsub("%", "%%", paste0(filenameStart, ".png")), boxPlot, width = 9.18, height = 5.78, units = c("in"))
}

boxPlot_distrib <- function(tbl, specialty, title, forecastMin, forecastMax,
                            stage, year) {
  #' Box Plot for Distribution Questions
  #'
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
    geom_boxplot(outlier.shape = NA) +
    ylab("Forecast") +
    xlab("Percentile") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "All Forecasters (n=", length(unique(boxData$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = cb_pal) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
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
    geom_boxplot(outlier.shape = NA) +
    ylab("Forecast") +
    xlab("Percentile") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "Superforecasters (n=", length(unique(boxData_supers$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = cb_pal) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
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
    geom_boxplot(outlier.shape = NA) +
    ylab("Forecast") +
    xlab("Percentile") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "Experts (n=", length(unique(boxData_experts$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = cb_pal) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
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
        geom_boxplot(outlier.shape = NA) +
        ylab("Forecast") +
        xlab("Percentile") +
        labs(title = title, subtitle = paste0("Stage ", stage, " | ", specialty, " Specialists (n=", length(unique(boxData_special$userName)), ")")) +
        theme_bw() +
        scale_color_manual(values = cb_pal) +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_text(size = 9, vjust = -37)
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
  #' @export

  boxData <- tbl %>% filter(userName %in% c(supers, expertsG1$userName))
  boxData$questionName <- factor(boxData$questionName, levels = unique(boxData$questionName), ordered = TRUE)

  boxPlot <- ggplot(boxData, aes(x = questionName, y = forecast, color = questionName)) +
    geom_boxplot(outlier.shape = NA) +
    ylab("Forecast") +
    xlab("Country") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "All Forecasters (n=", length(unique(boxData$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = cb_pal(length(unique(boxData$questionName)))) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_point(position = position_jitterdodge())
  # for(i in 1:length(unique(boxData$questionName))){
  #   boxPlot = boxPlot +
  #     geom_label(aes(x=i, y=median((boxData %>% filter(questionName == unique(boxData$questionName)[i]))$forecast), label=median((boxData %>% filter(questionName == unique(boxData$questionName)[i]))$forecast)), color=cb_pal(length(unique(boxData$questionName)))[i])
  # }
  boxPlot$labels$color <- ""
  if (year != "") {
    boxPlot$labels$subtitle <- paste0("Stage ", stage, " | ", year, " | All Forecasters (n=", length(unique(boxData$userName)), ")")
  }

  ggsave(gsub("%", "%%", paste0(boxPlot$data$setName[1], " (All Forecasters) - Stage ", stage, " - ", year, " - Box Plot.png")), boxPlot, width = 9.18, height = 5.78, units = c("in"))
}

boxPlot_country <- function(tbl, specialty, title,
                            forecastMin, forecastMax, stage) {
  #' Box Plot for Country Questions
  #'
  #' @export

  boxData <- tbl %>% filter(userName %in% c(supers, expertsG1$userName))
  boxData$answerText <- factor(boxData$answerText, levels = unique(boxData$answerText), ordered = TRUE)

  boxPlot <- ggplot(boxData, aes(x = answerText, y = forecast, color = answerText)) +
    geom_boxplot(outlier.shape = NA) +
    ylab("Forecast") +
    xlab("Country") +
    labs(title = title, subtitle = paste0("Stage ", stage, " | ", "All Forecasters (n=", length(unique(boxData$userName)), ")")) +
    theme_bw() +
    scale_color_manual(values = cb_pal(length(unique(boxData$answerText)))) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_point(position = position_jitterdodge())
  # for(i in 1:length(unique(boxData$questionName))){
  #   boxPlot = boxPlot +
  #     geom_label(aes(x=i, y=median((boxData %>% filter(questionName == unique(boxData$questionName)[i]))$forecast), label=median((boxData %>% filter(questionName == unique(boxData$questionName)[i]))$forecast)), color=cb_pal(length(unique(boxData$questionName)))[i])
  # }
  boxPlot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0(boxPlot$data$setName[1], " (All Forecasters) - Stage ", stage, " - Box Plot.png")), boxPlot, width = 9.18, height = 5.78, units = c("in"))
}

# FUNCTIONS THAT GENERATE THE FIGURES

figureDataInit <- function() {
  return(data.frame(
    beliefSet = character(0),
    year = numeric(0),
    date = Date(0),
    mean = numeric(0),
    sd = numeric(0),
    geoMean = numeric(0),
    median = numeric(0),
    hdTrim = numeric(0),
    neymanAgg = numeric(0),
    n = numeric(0),
    g1Mean = numeric(0),
    g1Sd = numeric(0),
    g1GeoMean = numeric(0),
    g1Median = numeric(0),
    g1HdTrim = numeric(0),
    g1NeymanAgg = numeric(0),
    g1N = numeric(0),
    supersMean = numeric(0),
    supersSd = numeric(0),
    supersGeoMean = numeric(0),
    supersMedian = numeric(0),
    supersHdTrim = numeric(0),
    supersNeymanAgg = numeric(0),
    supersN = numeric(0),
    expertsMean = numeric(0),
    expertsSd = numeric(0),
    expertsGeoMean = numeric(0),
    expertsMedian = numeric(0),
    expertsHdTrim = numeric(0),
    expertsNeymanAgg = numeric(0),
    expertsN = numeric(0),
    domainExpertsMean = numeric(0),
    domainExpertsSd = numeric(0),
    domainExpertsGeoMean = numeric(0),
    domainExpertsMedian = numeric(0),
    domainExpertsHdTrim = numeric(0),
    domainExpertsNeymanAgg = numeric(0),
    domainExpertsN = numeric(0),
    nonDomainExpertsMean = numeric(0),
    nonDomainExpertsSd = numeric(0),
    nonDomainExpertsGeoMean = numeric(0),
    nonDomainExpertsMedian = numeric(0),
    nonDomainExpertsHdTrim = numeric(0),
    nonDomainExpertsNeymanAgg = numeric(0),
    nonDomainExpertsN = numeric(0),
    t336Mean = numeric(0),
    t336Sd = numeric(0),
    t336GeoMean = numeric(0),
    t336Median = numeric(0),
    t336HdTrim = numeric(0),
    t336NeymanAgg = numeric(0),
    t336N = numeric(0),
    t336SupersMean = numeric(0),
    t336SupersSd = numeric(0),
    t336SupersGeoMean = numeric(0),
    t336SupersMedian = numeric(0),
    t336SupersHdTrim = numeric(0),
    t336SupersNeymanAgg = numeric(0),
    t336SupersN = numeric(0),
    t336ExpertsMean = numeric(0),
    t336ExpertsSd = numeric(0),
    t336ExpertsGeoMean = numeric(0),
    t336ExpertsMedian = numeric(0),
    t336ExpertsHdTrim = numeric(0),
    t336ExpertsNeymanAgg = numeric(0),
    t336ExpertsN = numeric(0),
    t337Mean = numeric(0),
    t337Sd = numeric(0),
    t337GeoMean = numeric(0),
    t337Median = numeric(0),
    t337HdTrim = numeric(0),
    t337NeymanAgg = numeric(0),
    t337N = numeric(0),
    t337SupersMean = numeric(0),
    t337SupersSd = numeric(0),
    t337SupersGeoMean = numeric(0),
    t337SupersMedian = numeric(0),
    t337SupersHdTrim = numeric(0),
    t337SupersNeymanAgg = numeric(0),
    t337SupersN = numeric(0),
    t337ExpertsMean = numeric(0),
    t337ExpertsSd = numeric(0),
    t337ExpertsGeoMean = numeric(0),
    t337ExpertsMedian = numeric(0),
    t337ExpertsHdTrim = numeric(0),
    t337ExpertsNeymanAgg = numeric(0),
    t337ExpertsN = numeric(0),
    t338Mean = numeric(0),
    t338Sd = numeric(0),
    t338GeoMean = numeric(0),
    t338Median = numeric(0),
    t338HdTrim = numeric(0),
    t338NeymanAgg = numeric(0),
    t338N = numeric(0),
    t338SupersMean = numeric(0),
    t338SupersSd = numeric(0),
    t338SupersGeoMean = numeric(0),
    t338SupersMedian = numeric(0),
    t338SupersHdTrim = numeric(0),
    t338SupersNeymanAgg = numeric(0),
    t338SupersN = numeric(0),
    t338ExpertsMean = numeric(0),
    t338ExpertsSd = numeric(0),
    t338ExpertsGeoMean = numeric(0),
    t338ExpertsMedian = numeric(0),
    t338ExpertsHdTrim = numeric(0),
    t338ExpertsNeymanAgg = numeric(0),
    t338ExpertsN = numeric(0),
    t339Mean = numeric(0),
    t339Sd = numeric(0),
    t339GeoMean = numeric(0),
    t339Median = numeric(0),
    t339HdTrim = numeric(0),
    t339NeymanAgg = numeric(0),
    t339N = numeric(0),
    t339SupersMean = numeric(0),
    t339SupersSd = numeric(0),
    t339SupersGeoMean = numeric(0),
    t339SupersMedian = numeric(0),
    t339SupersHdTrim = numeric(0),
    t339SupersNeymanAgg = numeric(0),
    t339SupersN = numeric(0),
    t339ExpertsMean = numeric(0),
    t339ExpertsSd = numeric(0),
    t339ExpertsGeoMean = numeric(0),
    t339ExpertsMedian = numeric(0),
    t339ExpertsHdTrim = numeric(0),
    t339ExpertsNeymanAgg = numeric(0),
    t339ExpertsN = numeric(0),
    t340Mean = numeric(0),
    t340Sd = numeric(0),
    t340GeoMean = numeric(0),
    t340Median = numeric(0),
    t340HdTrim = numeric(0),
    t340NeymanAgg = numeric(0),
    t340N = numeric(0),
    t340SupersMean = numeric(0),
    t340SupersSd = numeric(0),
    t340SupersGeoMean = numeric(0),
    t340SupersMedian = numeric(0),
    t340SupersHdTrim = numeric(0),
    t340SupersNeymanAgg = numeric(0),
    t340SupersN = numeric(0),
    t340ExpertsMean = numeric(0),
    t340ExpertsSd = numeric(0),
    t340ExpertsGeoMean = numeric(0),
    t340ExpertsMedian = numeric(0),
    t340ExpertsHdTrim = numeric(0),
    t340ExpertsNeymanAgg = numeric(0),
    t340ExpertsN = numeric(0),
    t341Mean = numeric(0),
    t341Sd = numeric(0),
    t341GeoMean = numeric(0),
    t341Median = numeric(0),
    t341HdTrim = numeric(0),
    t341NeymanAgg = numeric(0),
    t341N = numeric(0),
    t341SupersMean = numeric(0),
    t341SupersSd = numeric(0),
    t341SupersGeoMean = numeric(0),
    t341SupersMedian = numeric(0),
    t341SupersHdTrim = numeric(0),
    t341SupersNeymanAgg = numeric(0),
    t341SupersN = numeric(0),
    t341ExpertsMean = numeric(0),
    t341ExpertsSd = numeric(0),
    t341ExpertsGeoMean = numeric(0),
    t341ExpertsMedian = numeric(0),
    t341ExpertsHdTrim = numeric(0),
    t341ExpertsNeymanAgg = numeric(0),
    t331ExpertsN = numeric(0),
    t342Mean = numeric(0),
    t342Sd = numeric(0),
    t342GeoMean = numeric(0),
    t342Median = numeric(0),
    t342HdTrim = numeric(0),
    t342NeymanAgg = numeric(0),
    t342N = numeric(0),
    t343Mean = numeric(0),
    t343Sd = numeric(0),
    t343GeoMean = numeric(0),
    t343Median = numeric(0),
    t343HdTrim = numeric(0),
    t343NeymanAgg = numeric(0),
    t343N = numeric(0),
    t344Mean = numeric(0),
    t344Sd = numeric(0),
    t344GeoMean = numeric(0),
    t344Median = numeric(0),
    t344HdTrim = numeric(0),
    t344NeymanAgg = numeric(0),
    t344N = numeric(0),
    t345Mean = numeric(0),
    t345Sd = numeric(0),
    t345GeoMean = numeric(0),
    t345Median = numeric(0),
    t345HdTrim = numeric(0),
    t345NeymanAgg = numeric(0),
    t345N = numeric(0)
  ))
}

figureDataMetrics <- function(dateDataProcessed, beliefSet, year, date, qSpecialty) {
  dateData <- dateDataProcessed
  mean <- mean(dateData$forecast)
  sd <- sd(dateData$forecast)
  geoMean <- geoMeanCalc(dateData$forecast)
  median <- median(dateData$forecast)
  # Get bootstrapped CI's (for median only)
  boot_ci <- boot_results(dateData)
  median_confint_lower <- boot_ci$confint_lower
  median_confint_upper <- boot_ci$confint_upper
  if (length(dateData$forecast) > 0) {
    hdTrim <- hd_trim(dateData$forecast)
  } else {
    hdTrim <- NA
  }
  neymanAgg <- neymanAggCalc(dateData$forecast)
  n <- nrow(dateData)

  g1DateData <- filter(dateDataProcessed, userName %in% c(supers, expertsG1$userName))
  g1Mean <- mean(g1DateData$forecast)
  g1Sd <- sd(g1DateData$forecast)
  g1GeoMean <- geoMeanCalc(g1DateData$forecast)
  g1Median <- median(g1DateData$forecast)
  # Get bootstrapped CI's (for median only)
  boot_ci <- boot_results(g1DateData)
  g1Median_confint_lower <- boot_ci$confint_lower
  g1Median_confint_upper <- boot_ci$confint_upper
  if (length(g1DateData$forecast) > 0) {
    g1HdTrim <- hd_trim(g1DateData$forecast)
  } else {
    g1HdTrim <- NA
  }
  g1NeymanAgg <- neymanAggCalc(g1DateData$forecast)
  g1N <- nrow(g1DateData)

  supersDateData <- filter(dateDataProcessed, userName %in% supers)
  supersMean <- mean(supersDateData$forecast)
  supersSd <- sd(supersDateData$forecast)
  supersGeoMean <- geoMeanCalc(supersDateData$forecast)
  supersMedian <- median(supersDateData$forecast)
  # Get bootstrapped CI's (for median only)
  boot_ci <- boot_results(supersDateData)
  supersMedian_confint_lower <- boot_ci$confint_lower
  supersMedian_confint_upper <- boot_ci$confint_upper
  if (length(supersDateData$forecast) > 0) {
    supersHdTrim <- hd_trim(supersDateData$forecast)
  } else {
    supersHdTrim <- NA
  }
  supersNeymanAgg <- neymanAggCalc(supersDateData$forecast)
  supersN <- nrow(supersDateData)

  expertsDateData <- filter(dateDataProcessed, userName %in% expertsG1$userName)
  expertsMean <- mean(expertsDateData$forecast)
  expertsSd <- sd(expertsDateData$forecast)
  expertsGeoMean <- geoMeanCalc(expertsDateData$forecast)
  expertsMedian <- median(expertsDateData$forecast)
  # Get bootstrapped CI's (for median only)
  boot_ci <- boot_results(expertsDateData)
  expertsMedian_confint_lower <- boot_ci$confint_lower
  expertsMedian_confint_upper <- boot_ci$confint_upper
  if (length(expertsDateData$forecast) > 0) {
    expertsHdTrim <- hd_trim(expertsDateData$forecast)
  } else {
    expertsHdTrim <- NA
  }
  expertsNeymanAgg <- neymanAggCalc(expertsDateData$forecast)
  expertsN <- nrow(expertsDateData)

  if (qSpecialty != "") {
    domainExperts <- expertsG1 %>% filter(specialty1 == qSpecialty | specialty2 == qSpecialty | specialty3 == qSpecialty)
    domainExpertsDateData <- filter(dateDataProcessed, userName %in% domainExperts$userName)
    domainExpertsMean <- mean(domainExpertsDateData$forecast)
    domainExpertsSd <- sd(domainExpertsDateData$forecast)
    domainExpertsGeoMean <- geoMeanCalc(domainExpertsDateData$forecast)
    domainExpertsMedian <- median(domainExpertsDateData$forecast)
    # Get bootstrapped CI's (for median only)
    boot_ci <- boot_results(domainExpertsDateData)
    domainExpertsMedian_confint_lower <- boot_ci$confint_lower
    domainExpertsMedian_confint_upper <- boot_ci$confint_upper
    domainExpertsMedian <- median(domainExpertsDateData$forecast)
    if (length(domainExpertsDateData$forecast) > 0) {
      domainExpertsHdTrim <- hd_trim(domainExpertsDateData$forecast)
    } else {
      domainExpertsHdTrim <- NA
    }
    domainExpertsNeymanAgg <- neymanAggCalc(domainExpertsDateData$forecast)
    domainExpertsN <- nrow(domainExpertsDateData)
  } else {
    domainExpertsMean <- NA
    domainExpertsSd <- NA
    domainExpertsGeoMean <- NA
    domainExpertsMedian <- NA
    domainExpertsMedian_confint_lower <- NA
    domainExpertsMedian_confint_upper <- NA
    domainExpertsHdTrim <- NA
    domainExpertsNeymanAgg <- NA
    domainExpertsN <- NA
  }

  if (qSpecialty != "") {
    nonDomainExperts <- expertsG1 %>% filter(specialty1 != qSpecialty & specialty2 != qSpecialty & specialty3 != qSpecialty)
    nonDomainExpertsDateData <- filter(dateDataProcessed, userName %in% nonDomainExperts$userName)
    nonDomainExpertsMean <- mean(nonDomainExpertsDateData$forecast)
    nonDomainExpertsSd <- sd(nonDomainExpertsDateData$forecast)
    nonDomainExpertsGeoMean <- geoMeanCalc(nonDomainExpertsDateData$forecast)
    nonDomainExpertsMedian <- median(nonDomainExpertsDateData$forecast)
    # Get bootstrapped CI's (for median only)
    boot_ci <- boot_results(nonDomainExpertsDateData)
    nonDomainExpertsMedian_confint_lower <- boot_ci$confint_lower
    nonDomainExpertsMedian_confint_upper <- boot_ci$confint_upper
    if (length(nonDomainExpertsDateData$forecast) > 0) {
      nonDomainExpertsHdTrim <- hd_trim(nonDomainExpertsDateData$forecast)
    } else {
      nonDomainExpertsHdTrim <- NA
    }
    nonDomainExpertsNeymanAgg <- neymanAggCalc(nonDomainExpertsDateData$forecast)
    nonDomainExpertsN <- nrow(nonDomainExpertsDateData)
  } else {
    nonDomainExpertsMean <- NA
    nonDomainExpertsSd <- NA
    nonDomainExpertsGeoMean <- NA
    nonDomainExpertsMedian <- NA
    domainExpertsMedian_confint_lower <- NA
    domainExpertsMedian_confint_upper <- NA
    nonDomainExpertsHdTrim <- NA
    nonDomainExpertsNeymanAgg <- NA
    nonDomainExpertsN <- NA
  }

  t336DateData <- filter(dateDataProcessed, teamId == 336)
  t336Mean <- mean(t336DateData$forecast)
  t336Sd <- sd(t336DateData$forecast)
  t336GeoMean <- geoMeanCalc(t336DateData$forecast)
  t336Median <- median(t336DateData$forecast)
  if (length(t336DateData$forecast) > 0) {
    t336HdTrim <- hd_trim(t336DateData$forecast)
  } else {
    t336HdTrim <- NA
  }
  t336NeymanAgg <- neymanAggCalc(t336DateData$forecast)
  t336N <- nrow(t336DateData)

  t336SupersDateData <- dateDataProcessed %>%
    filter(teamId == 336) %>%
    filter(userName %in% supers)
  t336SupersMean <- mean(t336SupersDateData$forecast)
  t336SupersSd <- sd(t336SupersDateData$forecast)
  t336SupersGeoMean <- geoMeanCalc(t336SupersDateData$forecast)
  t336SupersMedian <- median(t336SupersDateData$forecast)
  if (length(t336SupersDateData$forecast) > 0) {
    t336SupersHdTrim <- hd_trim(t336SupersDateData$forecast)
  } else {
    t336SupersHdTrim <- NA
  }
  t336SupersNeymanAgg <- neymanAggCalc(t336SupersDateData$forecast)
  t336SupersN <- nrow(t336SupersDateData)

  t336ExpertsDateData <- dateDataProcessed %>%
    filter(teamId == 336) %>%
    filter(userName %in% expertsG1$userName)
  t336ExpertsMean <- mean(t336ExpertsDateData$forecast)
  t336ExpertsSd <- sd(t336ExpertsDateData$forecast)
  t336ExpertsGeoMean <- geoMeanCalc(t336ExpertsDateData$forecast)
  t336ExpertsMedian <- median(t336ExpertsDateData$forecast)
  if (length(t336ExpertsDateData$forecast) > 0) {
    t336ExpertsHdTrim <- hd_trim(t336ExpertsDateData$forecast)
  } else {
    t336ExpertsHdTrim <- NA
  }
  t336ExpertsNeymanAgg <- neymanAggCalc(t336ExpertsDateData$forecast)
  t336ExpertsN <- nrow(t336ExpertsDateData)

  t337DateData <- filter(dateDataProcessed, teamId == 337)
  t337Mean <- mean(t337DateData$forecast)
  t337Sd <- sd(t337DateData$forecast)
  t337GeoMean <- geoMeanCalc(t337DateData$forecast)
  t337Median <- median(t337DateData$forecast)
  if (length(t337DateData$forecast) > 0) {
    t337HdTrim <- hd_trim(t337DateData$forecast)
  } else {
    t337HdTrim <- NA
  }
  t337NeymanAgg <- neymanAggCalc(t337DateData$forecast)
  t337N <- nrow(t337DateData)

  t337SupersDateData <- dateDataProcessed %>%
    filter(teamId == 337) %>%
    filter(userName %in% supers)
  t337SupersMean <- mean(t337SupersDateData$forecast)
  t337SupersSd <- sd(t337SupersDateData$forecast)
  t337SupersGeoMean <- geoMeanCalc(t337SupersDateData$forecast)
  t337SupersMedian <- median(t337SupersDateData$forecast)
  if (length(t337SupersDateData$forecast) > 0) {
    t337SupersHdTrim <- hd_trim(t337SupersDateData$forecast)
  } else {
    t337SupersHdTrim <- NA
  }
  t337SupersNeymanAgg <- neymanAggCalc(t337SupersDateData$forecast)
  t337SupersN <- nrow(t337SupersDateData)

  t337ExpertsDateData <- dateDataProcessed %>%
    filter(teamId == 337) %>%
    filter(userName %in% expertsG1$userName)
  t337ExpertsMean <- mean(t337ExpertsDateData$forecast)
  t337ExpertsSd <- sd(t337ExpertsDateData$forecast)
  t337ExpertsGeoMean <- geoMeanCalc(t337ExpertsDateData$forecast)
  t337ExpertsMedian <- median(t337ExpertsDateData$forecast)
  if (length(t337ExpertsDateData$forecast) > 0) {
    t337ExpertsHdTrim <- hd_trim(t337ExpertsDateData$forecast)
  } else {
    t337ExpertsHdTrim <- NA
  }
  t337ExpertsNeymanAgg <- neymanAggCalc(t337ExpertsDateData$forecast)
  t337ExpertsN <- nrow(t337ExpertsDateData)

  t338DateData <- filter(dateDataProcessed, teamId == 338)
  t338Mean <- mean(t338DateData$forecast)
  t338Sd <- sd(t338DateData$forecast)
  t338GeoMean <- geoMeanCalc(t338DateData$forecast)
  t338Median <- median(t338DateData$forecast)
  if (length(t338DateData$forecast) > 0) {
    t338HdTrim <- hd_trim(t338DateData$forecast)
  } else {
    t338HdTrim <- NA
  }
  t338NeymanAgg <- neymanAggCalc(t338DateData$forecast)
  t338N <- nrow(t338DateData)

  t338SupersDateData <- dateDataProcessed %>%
    filter(teamId == 338) %>%
    filter(userName %in% supers)
  t338SupersMean <- mean(t338SupersDateData$forecast)
  t338SupersSd <- sd(t338SupersDateData$forecast)
  t338SupersGeoMean <- geoMeanCalc(t338SupersDateData$forecast)
  t338SupersMedian <- median(t338SupersDateData$forecast)
  if (length(t338SupersDateData$forecast) > 0) {
    t338SupersHdTrim <- hd_trim(t338SupersDateData$forecast)
  } else {
    t338SupersHdTrim <- NA
  }
  t338SupersNeymanAgg <- neymanAggCalc(t338SupersDateData$forecast)
  t338SupersN <- nrow(t338SupersDateData)

  t338ExpertsDateData <- dateDataProcessed %>%
    filter(teamId == 338) %>%
    filter(userName %in% expertsG1$userName)
  t338ExpertsMean <- mean(t338ExpertsDateData$forecast)
  t338ExpertsSd <- sd(t338ExpertsDateData$forecast)
  t338ExpertsGeoMean <- geoMeanCalc(t338ExpertsDateData$forecast)
  t338ExpertsMedian <- median(t338ExpertsDateData$forecast)
  if (length(t338ExpertsDateData$forecast) > 0) {
    t338ExpertsHdTrim <- hd_trim(t338ExpertsDateData$forecast)
  } else {
    t338ExpertsHdTrim <- NA
  }
  t338ExpertsNeymanAgg <- neymanAggCalc(t338ExpertsDateData$forecast)
  t338ExpertsN <- nrow(t338ExpertsDateData)

  t339DateData <- filter(dateDataProcessed, teamId == 339)
  t339Mean <- mean(t339DateData$forecast)
  t339Sd <- sd(t339DateData$forecast)
  t339GeoMean <- geoMeanCalc(t339DateData$forecast)
  t339Median <- median(t339DateData$forecast)
  if (length(t339DateData$forecast) > 0) {
    t339HdTrim <- hd_trim(t339DateData$forecast)
  } else {
    t339HdTrim <- NA
  }
  t339NeymanAgg <- neymanAggCalc(t339DateData$forecast)
  t339N <- nrow(t339DateData)

  t339SupersDateData <- dateDataProcessed %>%
    filter(teamId == 339) %>%
    filter(userName %in% supers)
  t339SupersMean <- mean(t339SupersDateData$forecast)
  t339SupersSd <- sd(t339SupersDateData$forecast)
  t339SupersGeoMean <- geoMeanCalc(t339SupersDateData$forecast)
  t339SupersMedian <- median(t339SupersDateData$forecast)
  if (length(t339SupersDateData$forecast) > 0) {
    t339SupersHdTrim <- hd_trim(t339SupersDateData$forecast)
  } else {
    t339SupersHdTrim <- NA
  }
  t339SupersNeymanAgg <- neymanAggCalc(t339SupersDateData$forecast)
  t339SupersN <- nrow(t339SupersDateData)

  t339ExpertsDateData <- dateDataProcessed %>%
    filter(teamId == 339) %>%
    filter(userName %in% expertsG1$userName)
  t339ExpertsMean <- mean(t339ExpertsDateData$forecast)
  t339ExpertsSd <- sd(t339ExpertsDateData$forecast)
  t339ExpertsGeoMean <- geoMeanCalc(t339ExpertsDateData$forecast)
  t339ExpertsMedian <- median(t339ExpertsDateData$forecast)
  if (length(t339ExpertsDateData$forecast) > 0) {
    t339ExpertsHdTrim <- hd_trim(t339ExpertsDateData$forecast)
  } else {
    t339ExpertsHdTrim <- NA
  }
  t339ExpertsNeymanAgg <- neymanAggCalc(t339ExpertsDateData$forecast)
  t339ExpertsN <- nrow(t339ExpertsDateData)

  t340DateData <- filter(dateDataProcessed, teamId == 340)
  t340Mean <- mean(t340DateData$forecast)
  t340Sd <- sd(t340DateData$forecast)
  t340GeoMean <- geoMeanCalc(t340DateData$forecast)
  t340Median <- median(t340DateData$forecast)
  if (length(t340DateData$forecast) > 0) {
    t340HdTrim <- hd_trim(t340DateData$forecast)
  } else {
    t340HdTrim <- NA
  }
  t340NeymanAgg <- neymanAggCalc(t340DateData$forecast)
  t340N <- nrow(t340DateData)

  t340SupersDateData <- dateDataProcessed %>%
    filter(teamId == 340) %>%
    filter(userName %in% supers)
  t340SupersMean <- mean(t340SupersDateData$forecast)
  t340SupersSd <- sd(t340SupersDateData$forecast)
  t340SupersGeoMean <- geoMeanCalc(t340SupersDateData$forecast)
  t340SupersMedian <- median(t340SupersDateData$forecast)
  if (length(t340SupersDateData$forecast) > 0) {
    t340SupersHdTrim <- hd_trim(t340SupersDateData$forecast)
  } else {
    t340SupersHdTrim <- NA
  }
  t340SupersNeymanAgg <- neymanAggCalc(t340SupersDateData$forecast)
  t340SupersN <- nrow(t340SupersDateData)

  t340ExpertsDateData <- dateDataProcessed %>%
    filter(teamId == 340) %>%
    filter(userName %in% expertsG1$userName)
  t340ExpertsMean <- mean(t340ExpertsDateData$forecast)
  t340ExpertsSd <- sd(t340ExpertsDateData$forecast)
  t340ExpertsGeoMean <- geoMeanCalc(t340ExpertsDateData$forecast)
  t340ExpertsMedian <- median(t340ExpertsDateData$forecast)
  if (length(t340ExpertsDateData$forecast) > 0) {
    t340ExpertsHdTrim <- hd_trim(t340ExpertsDateData$forecast)
  } else {
    t340ExpertsHdTrim <- NA
  }
  t340ExpertsNeymanAgg <- neymanAggCalc(t340ExpertsDateData$forecast)
  t340ExpertsN <- nrow(t340ExpertsDateData)

  t341DateData <- filter(dateDataProcessed, teamId == 341)
  t341Mean <- mean(t341DateData$forecast)
  t341Sd <- sd(t341DateData$forecast)
  t341GeoMean <- geoMeanCalc(t341DateData$forecast)
  t341Median <- median(t341DateData$forecast)
  if (length(t341DateData$forecast) > 0) {
    t341HdTrim <- hd_trim(t341DateData$forecast)
  } else {
    t341HdTrim <- NA
  }
  t341NeymanAgg <- neymanAggCalc(t341DateData$forecast)
  t341N <- nrow(t341DateData)

  t341SupersDateData <- dateDataProcessed %>%
    filter(teamId == 341) %>%
    filter(userName %in% supers)
  t341SupersMean <- mean(t341SupersDateData$forecast)
  t341SupersSd <- sd(t341SupersDateData$forecast)
  t341SupersGeoMean <- geoMeanCalc(t341SupersDateData$forecast)
  t341SupersMedian <- median(t341SupersDateData$forecast)
  if (length(t341SupersDateData$forecast) > 0) {
    t341SupersHdTrim <- hd_trim(t341SupersDateData$forecast)
  } else {
    t341SupersHdTrim <- NA
  }
  t341SupersNeymanAgg <- neymanAggCalc(t341SupersDateData$forecast)
  t341SupersN <- nrow(t341SupersDateData)

  t341ExpertsDateData <- dateDataProcessed %>%
    filter(teamId == 341) %>%
    filter(userName %in% expertsG1$userName)
  t341ExpertsMean <- mean(t341ExpertsDateData$forecast)
  t341ExpertsSd <- sd(t341ExpertsDateData$forecast)
  t341ExpertsGeoMean <- geoMeanCalc(t341ExpertsDateData$forecast)
  t341ExpertsMedian <- median(t341ExpertsDateData$forecast)
  if (length(t341ExpertsDateData$forecast) > 0) {
    t341ExpertsHdTrim <- hd_trim(t341ExpertsDateData$forecast)
  } else {
    t341ExpertsHdTrim <- NA
  }
  t341ExpertsNeymanAgg <- neymanAggCalc(t341ExpertsDateData$forecast)
  t341ExpertsN <- nrow(t341ExpertsDateData)

  t342DateData <- filter(dateDataProcessed, teamId == 342)
  t342Mean <- mean(t342DateData$forecast)
  t342Sd <- sd(t342DateData$forecast)
  t342GeoMean <- geoMeanCalc(t342DateData$forecast)
  t342Median <- median(t342DateData$forecast)
  if (length(t342DateData$forecast) > 0) {
    t342HdTrim <- hd_trim(t342DateData$forecast)
  } else {
    t342HdTrim <- NA
  }
  t342NeymanAgg <- neymanAggCalc(t342DateData$forecast)
  t342N <- nrow(t342DateData)

  t343DateData <- filter(dateDataProcessed, teamId == 343)
  t343Mean <- mean(t343DateData$forecast)
  t343Sd <- sd(t343DateData$forecast)
  t343GeoMean <- geoMeanCalc(t343DateData$forecast)
  t343Median <- median(t343DateData$forecast)
  if (length(t343DateData$forecast) > 0) {
    t343HdTrim <- hd_trim(t343DateData$forecast)
  } else {
    t343HdTrim <- NA
  }
  t343NeymanAgg <- neymanAggCalc(t343DateData$forecast)
  t343N <- nrow(t343DateData)

  t344DateData <- filter(dateDataProcessed, teamId == 344)
  t344Mean <- mean(t344DateData$forecast)
  t344Sd <- sd(t344DateData$forecast)
  t344GeoMean <- geoMeanCalc(t344DateData$forecast)
  t344Median <- median(t344DateData$forecast)
  if (length(t344DateData$forecast) > 0) {
    t344HdTrim <- hd_trim(t344DateData$forecast)
  } else {
    t344HdTrim <- NA
  }
  t344NeymanAgg <- neymanAggCalc(t344DateData$forecast)
  t344N <- nrow(t344DateData)

  t345DateData <- filter(dateDataProcessed, teamId == 345)
  t345Mean <- mean(t345DateData$forecast)
  t345Sd <- sd(t345DateData$forecast)
  t345GeoMean <- geoMeanCalc(t345DateData$forecast)
  t345Median <- median(t345DateData$forecast)
  if (length(t345DateData$forecast) > 0) {
    t345HdTrim <- hd_trim(t345DateData$forecast)
  } else {
    t345HdTrim <- NA
  }
  t345NeymanAgg <- neymanAggCalc(t345DateData$forecast)
  t345N <- nrow(t345DateData)

  return(data.frame(beliefSet, year, currentDate = date, mean, sd, geoMean, median, hdTrim, neymanAgg, n,
                    g1Mean, g1Sd, g1GeoMean, g1Median, g1Median_confint_lower, g1Median_confint_upper, g1HdTrim, g1NeymanAgg, g1N,
                    supersMean, supersSd, supersGeoMean, supersMedian, supersMedian_confint_lower, supersMedian_confint_upper, expertsMedian_confint_lower, expertsMedian_confint_upper, supersHdTrim, supersNeymanAgg, supersN,
                    expertsMean, expertsSd, expertsGeoMean, expertsMedian, expertsMedian_confint_lower, expertsMedian_confint_upper, expertsHdTrim, expertsNeymanAgg, expertsN,
                    domainExpertsMean, domainExpertsSd, domainExpertsGeoMean, domainExpertsMedian, domainExpertsMedian_confint_lower, domainExpertsMedian_confint_upper, domainExpertsHdTrim, domainExpertsNeymanAgg, domainExpertsN,
                    nonDomainExpertsMean, nonDomainExpertsSd, nonDomainExpertsGeoMean, nonDomainExpertsMedian, nonDomainExpertsMedian_confint_lower, nonDomainExpertsMedian_confint_upper, nonDomainExpertsHdTrim, nonDomainExpertsNeymanAgg, nonDomainExpertsN,
                    t336Mean, t336Sd, t336GeoMean, t336Median, t336HdTrim, t336NeymanAgg, t336N,
                    t336SupersMean, t336SupersSd, t336SupersGeoMean, t336SupersMedian, t336SupersHdTrim, t336SupersNeymanAgg, t336SupersN,
                    t336ExpertsMean, t336ExpertsSd, t336ExpertsGeoMean, t336ExpertsMedian, t336ExpertsHdTrim, t336ExpertsNeymanAgg, t336ExpertsN,
                    t337Mean, t337Sd, t337GeoMean, t337Median, t337HdTrim, t337NeymanAgg, t337N,
                    t337SupersMean, t337SupersSd, t337SupersGeoMean, t337SupersMedian, t337SupersHdTrim, t337SupersNeymanAgg, t337SupersN,
                    t337ExpertsMean, t337ExpertsSd, t337ExpertsGeoMean, t337ExpertsMedian, t337ExpertsHdTrim, t337ExpertsNeymanAgg, t337ExpertsN,
                    t338Mean, t338Sd, t338GeoMean, t338Median, t338HdTrim, t338NeymanAgg, t338N,
                    t338SupersMean, t338SupersSd, t338SupersGeoMean, t338SupersMedian, t338SupersHdTrim, t338SupersNeymanAgg, t338SupersN,
                    t338ExpertsMean, t338ExpertsSd, t338ExpertsGeoMean, t338ExpertsMedian, t338ExpertsHdTrim, t338ExpertsNeymanAgg, t338ExpertsN,
                    t339Mean, t339Sd, t339GeoMean, t339Median, t339HdTrim, t339NeymanAgg, t339N,
                    t339SupersMean, t339SupersSd, t339SupersGeoMean, t339SupersMedian, t339SupersHdTrim, t339SupersNeymanAgg, t339SupersN,
                    t339ExpertsMean, t339ExpertsSd, t339ExpertsGeoMean, t339ExpertsMedian, t339ExpertsHdTrim, t339ExpertsNeymanAgg, t339ExpertsN,
                    t340Mean, t340Sd, t340GeoMean, t340Median, t340HdTrim, t340NeymanAgg, t340N,
                    t340SupersMean, t340SupersSd, t340SupersGeoMean, t340SupersMedian, t340SupersHdTrim, t340SupersNeymanAgg, t340SupersN,
                    t340ExpertsMean, t340ExpertsSd, t340ExpertsGeoMean, t340ExpertsMedian, t340ExpertsHdTrim, t340ExpertsNeymanAgg, t340ExpertsN,
                    t341Mean, t341Sd, t341GeoMean, t341Median, t341HdTrim, t341NeymanAgg, t341N,
                    t341SupersMean, t341SupersSd, t341SupersGeoMean, t341SupersMedian, t341SupersHdTrim, t341SupersNeymanAgg, t341SupersN,
                    t341ExpertsMean, t341ExpertsSd, t341ExpertsGeoMean, t341ExpertsMedian, t341ExpertsHdTrim, t341ExpertsNeymanAgg, t341ExpertsN,
                    t342Mean, t342Sd, t342GeoMean, t342Median, t342HdTrim, t342NeymanAgg, t342N,
                    t343Mean, t343Sd, t343GeoMean, t343Median, t343HdTrim, t343NeymanAgg, t343N,
                    t344Mean, t344Sd, t344GeoMean, t344Median, t344HdTrim, t344NeymanAgg, t344N,
                    t345Mean, t345Sd, t345GeoMean, t345Median, t345HdTrim, t345NeymanAgg, t345N))
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

          currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = beliefSets[j], year = years[k], date = currentDate, qSpecialty))
        }

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Figure Data"))
        write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", currentQuestionName, ".csv"), row.names = FALSE)

        multiYearReciprocalGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName)
        multiYearReciprocalVarianceGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName)
      }
    }
  }
}

multiYearReciprocalGraphics <- function(title, subtitle, csv, currentSetName) {
  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    median = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersMedian) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- subtitle

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsMedian) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsMedian) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$median))) {
    domainExpertsTimeSeries$median[is.na(domainExpertsTimeSeries$median)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$median <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "by", csv$year[1]), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(paste0(currentSetName, " - Figure One (", csv$year[1], " ", csv$beliefSet[1], ").png"), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiYearReciprocalVarianceGraphics <- function(title, subtitle, csv, currentSetName) {
  #' Multi-year Reciprocal Variance Graphics
  #'
  #' @export

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title, "by", csv$year[1]), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(paste0("VARIANCE - ", currentSetName, " - Figure One (", csv$year[1], " ", csv$beliefSet[1], ").png"), plot, width = 9.18, height = 5.78, units = c("in"))

  #####

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries$supersSd <- 100 * (supersTimeSeries$supersSd / supersTimeSeries$supersSd[1])
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries$expertsSd <- 100 * (expertsTimeSeries$expertsSd / expertsTimeSeries$expertsSd[1])
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries$domainExpertsSd <- 100 * (domainExpertsTimeSeries$domainExpertsSd / domainExpertsTimeSeries$domainExpertsSd[1])
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title, "by", csv$year[1]), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(paste0("PERCENT VARIANCE -", currentSetName, " - Figure One (", csv$year[1], " ", csv$beliefSet[1], ").png"), plot, width = 9.18, height = 5.78, units = c("in"))
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

      totalSupers <- nrow(unique(questionDataRaw %>% filter(userName %in% supers) %>% select(userName)))
      totalExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName) %>% select(userName)))
      totalDomainExperts <- nrow(unique(questionDataRaw %>% filter(userName %in% expertsG1$userName[expertsG1$specialty == qSpecialty]) %>% select(userName)))

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
      write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", currentAnswerText, ".csv"), row.names = FALSE)

      pointDistribGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, distrib[j])
    }
  }
}

pointDistribGraphics <- function(title, subtitle, csv, currentSetName, distrib) {
  #' Point Distribution Graphics
  #'
  #' @export

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    median = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersMedian) %>%
    mutate(group = "Superforecasters")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsMedian) %>%
    mutate(group = "Experts")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsMedian) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$median)) & any(domainExpertsTimeSeries$n > 4)) {
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$median <- NA
      }
    }
    if (any(!is.na(domainExpertsTimeSeries$n))) {
      plotTable <- rbind(plotTable, domainExpertsTimeSeries)
    }
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "-", distrib), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")

  if (grepl("%", currentSetName)) {
    ggsave(paste0(currentSetName, "% - Figure One (", distrib, "%).png"), plot, width = 9.18, height = 5.78, units = c("in"))
  } else {
    ggsave(paste0(currentSetName, " - Figure One (", distrib, "%).png"), plot, width = 9.18, height = 5.78, units = c("in"))
  }
}

pointDistribVarianceGraphics <- function(title, subtitle, csv, currentSetName, currentDistrib) {
  #' Point Distribution Variance Graphics
  #'
  #' @export

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("VARIANCE - ", currentSetName, " - Figure One (", currentDistrib, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries$supersSd <- 100 * (supersTimeSeries$supersSd / supersTimeSeries$supersSd[1])
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries$expertsSd <- 100 * (expertsTimeSeries$expertsSd / expertsTimeSeries$expertsSd[1])
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries$domainExpertsSd <- 100 * (domainExpertsTimeSeries$domainExpertsSd / domainExpertsTimeSeries$domainExpertsSd[1])
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("PERCENT VARIANCE - ", currentSetName, " - Figure One (", currentDistrib, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
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

          currentSetTimeSeries <- rbind(currentSetTimeSeries, figureDataMetrics(dateDataProcessed, beliefSet = "", year = years[j], date = currentDate, qSpecialty))
        }

        if (i != 22) {
          setwd(paste0(yourHome, "automated analysis/Summary Data/", currentSetName, "/Figure Data"))
        } else {
          setwd(paste0(yourHome, "automated analysis/Summary Data/40. Massive Multitask Language Understanding Benchmark/Figure Data/"))
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

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    median = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersMedian) %>%
    mutate(group = "Superforecasters")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {  # note to self to ask about this
      supersTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsMedian) %>%
    mutate(group = "Experts")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsMedian) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$median)) & any(domainExpertsTimeSeries$n > 4)) {
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$median <- NA
      }
    }
    if (any(!is.na(domainExpertsTimeSeries$n))) {
      plotTable <- rbind(plotTable, domainExpertsTimeSeries)
    }
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "-", year, "-", currentDistrib), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")

  ggsave(gsub("%", "%%", paste0(currentSetName, " - Figure One (", year, " - ", currentDistrib, ").png")), plot, device = "png", width = 9.18, height = 5.78, units = c("in"))
}

multiYearDistribVarianceGraphics <- function(title, subtitle, csv, currentSetName, year, currentDistrib) {
  #' Multi-Year Distribution Graphics
  #'
  #' @export

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title, "-", year, "-", currentDistrib), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("VARIANCE - ", currentSetName, " - Figure One (", currentDistrib, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries$supersSd <- 100 * (supersTimeSeries$supersSd / supersTimeSeries$supersSd[1])
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries$expertsSd <- 100 * (expertsTimeSeries$expertsSd / expertsTimeSeries$expertsSd[1])
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries$domainExpertsSd <- 100 * (domainExpertsTimeSeries$domainExpertsSd / domainExpertsTimeSeries$domainExpertsSd[1])
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title, "-", year, "-", currentDistrib), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("PERCENT VARIANCE - ", currentSetName, " - Figure One (", currentDistrib, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
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
      write.csv(currentSetTimeSeries, paste0(currentSetName, " - ", years[j], ".csv"), row.names = FALSE)

      multiYearBinaryGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName, year = years[j])
    }
  }
}

multiYearBinaryGraphics <- function(title, subtitle, csv, currentSetName, year) {
  #' Multi-year Binary Graphics
  #'
  #' @export

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    median = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersMedian) %>%
    mutate(group = "Superforecasters")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsMedian) %>%
    mutate(group = "Experts")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsMedian) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$median)) & any(domainExpertsTimeSeries$n > 4)) {
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$median <- NA
      }
    }
    if (any(!is.na(domainExpertsTimeSeries$n))) {
      plotTable <- rbind(plotTable, domainExpertsTimeSeries)
    }
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")

  ggsave(gsub("%", "%%", paste0(currentSetName, " - Figure One (", year, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiYearBinaryVarianceGraphics <- function(title, subtitle, csv, currentSetName, year) {
  #' Multi-year Binary Graphics
  #'
  #' @export

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("VARIANCE - ", currentSetName, " - Figure One (", year, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries$supersSd <- 100 * (supersTimeSeries$supersSd / supersTimeSeries$supersSd[1])
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries$expertsSd <- 100 * (expertsTimeSeries$expertsSd / expertsTimeSeries$expertsSd[1])
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries$domainExpertsSd <- 100 * (domainExpertsTimeSeries$domainExpertsSd / domainExpertsTimeSeries$domainExpertsSd[1])
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("PERCENT VARIANCE - ", currentSetName, " - Figure One (", year, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiYearCountryDistribGraphics <- function(title, subtitle, csv, currentSetName, year, country) {
  #' Multi-Year Country Distribution Graphics
  #'
  #' @export

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    median = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersMedian) %>%
    mutate(group = "Superforecasters")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsMedian) %>%
    mutate(group = "Experts")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsMedian) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$median)) & any(domainExpertsTimeSeries$n > 4)) {
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$median <- NA
      }
    }
    if (any(!is.na(domainExpertsTimeSeries$n))) {
      plotTable <- rbind(plotTable, domainExpertsTimeSeries)
    }
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "-", country, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0(currentSetName, " - Figure One (", year, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiYearCountryVarianceGraphics <- function(title, subtitle, csv, currentSetName, year, country) {
  #' Multi-Year Country Variance Graphics
  #'
  #' @export

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title, "-", country, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("VARIANCE - ", currentSetName, " - Figure One (", year, " ", country, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries$supersSd <- 100 * (supersTimeSeries$supersSd / supersTimeSeries$supersSd[1])
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries$expertsSd <- 100 * (expertsTimeSeries$expertsSd / expertsTimeSeries$expertsSd[1])
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries$domainExpertsSd <- 100 * (domainExpertsTimeSeries$domainExpertsSd / domainExpertsTimeSeries$domainExpertsSd[1])
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title, "-", country, "-", year), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("PERCENT VARIANCE - ", currentSetName, " - Figure One (", year, country, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

multiCountryBinaryGraphics <- function(title, subtitle, csv, currentSetName, country) {
  #' Multi-Country Binary Graphics
  #'
  #' @export

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    median = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersMedian) %>%
    mutate(group = "Superforecasters")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsMedian) %>%
    mutate(group = "Experts")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsMedian) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$median)) & any(domainExpertsTimeSeries$n > 4)) {
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$median <- NA
      }
    }
    if (any(!is.na(domainExpertsTimeSeries$n))) {
      plotTable <- rbind(plotTable, domainExpertsTimeSeries)
    }
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "-", country), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0(currentSetName, " - Figure One (", country, ").png")), plot, width = 9.18, height = 5.78, units = c("in"))
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
    write.csv(currentSetTimeSeries, paste0(currentSetName, ".csv"), row.names = FALSE)

    pointBinaryGraphics(title = metaTable[i, ]$title, subtitle = metaTable[i, ]$subtitle, csv = currentSetTimeSeries, currentSetName)
  }
}

pointBinaryGraphics <- function(title, subtitle, csv, currentSetName) {
  #' Point Binary Graphics
  #'
  #' @export

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    median = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersMedian) %>%
    mutate(group = "Superforecasters")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsMedian) %>%
    mutate(group = "Experts")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$median <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsMedian) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "median", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$median)) & any(domainExpertsTimeSeries$n > 4)) {
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$median <- NA
      }
    }
    if (any(!is.na(domainExpertsTimeSeries$n))) {
      plotTable <- rbind(plotTable, domainExpertsTimeSeries)
    }
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")

  ggsave(gsub("%", "%%", paste0(currentSetName, " - Figure One.png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

pointBinaryVarianceGraphics <- function(title, subtitle, csv, currentSetName) {
  #' Point Binary Variance Graphics
  #'
  #' @export

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("SD") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("VARIANCE - ", currentSetName, " - Figure One.png")), plot, width = 9.18, height = 5.78, units = c("in"))


  #####

  csv <- csv %>% filter(currentDate > ymd("2022 07 14"))

  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    sd = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersSd) %>%
    mutate(group = paste("Superforecasters"))
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (n=", csv$supersN[nrow(csv)], ")")
  supersTimeSeries$supersSd <- 100 * (supersTimeSeries$supersSd / supersTimeSeries$supersSd[1])
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, supersTimeSeries)

  title <- title
  subtitle <- "Variance over Time"

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsSd) %>%
    mutate(group = "Experts")
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (n=", csv$expertsN[nrow(csv)], ")")
  expertsTimeSeries$expertsSd <- 100 * (expertsTimeSeries$expertsSd / expertsTimeSeries$expertsSd[1])
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$sd <- NA
    }
  }
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsSd) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (n=", csv$domainExpertsN[nrow(csv)], ")")
  domainExpertsTimeSeries$domainExpertsSd <- 100 * (domainExpertsTimeSeries$domainExpertsSd / domainExpertsTimeSeries$domainExpertsSd[1])
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "sd", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$sd))) {
    domainExpertsTimeSeries$sd[is.na(domainExpertsTimeSeries$sd)] <- 0
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$sd <- NA
      }
    }
    plotTable <- rbind(plotTable, domainExpertsTimeSeries)
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plotTable$group <- factor(plotTable$group, levels = unique(plotTable$group), ordered = TRUE)

  plot <- ggplot(plotTable, aes(x = currentDate, y = sd, group = group, fill = group)) +
    geom_line() +
    ylab("% of Initial SD") +
    xlab("Date") +
    labs(title = paste(title), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed")
  if (length(unique(plotTable$group)) == 2) {
    plot <- plot +
      scale_color_manual(values = cb_pal[1:2])
  } else {
    plot <- plot +
      scale_color_manual(values = cb_pal)
  }
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0("PERCENT VARIANCE - ", currentSetName, " - Figure One.png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

pointBinaryGraphics_custom <- function(title, csv, currentSetName) {
  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    hdTrim = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersHdTrim) %>%
    mutate(group = "Superforecasters")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "hdTrim", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$hdTrim <- NA
    }
  }
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (N = ", supersTimeSeries[nrow(supersTimeSeries), ]$n, ")")
  plotTable <- rbind(plotTable, supersTimeSeries)

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsHdTrim) %>%
    mutate(group = "Experts")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "hdTrim", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$hdTrim <- NA
    }
  }
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (N = ", expertsTimeSeries[nrow(expertsTimeSeries), ]$n, ")")
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsHdTrim) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "hdTrim", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$hdTrim)) & any(domainExpertsTimeSeries$n >= 4)) {
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$hdTrim <- NA
      }
    }
    if (any(!is.na(domainExpertsTimeSeries$n))) {
      domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (N = ", domainExpertsTimeSeries[nrow(domainExpertsTimeSeries), ]$n, ")")
      plotTable <- rbind(plotTable, domainExpertsTimeSeries)
    }
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plot <- ggplot(plotTable, aes(x = currentDate, y = hdTrim, group = group, fill = group)) +
    geom_line() +
    ylab("HD Trim") +
    xlab("Date") +
    labs(title = paste(title)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
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
  plotTable <- data.frame(
    year = numeric(0),
    currentDate = Date(0),
    hdTrim = numeric(0),
    group = character(0),
    n = numeric(0)
  )

  supersTimeSeries <- csv %>%
    select(year, currentDate, supersHdTrim) %>%
    mutate(group = "Superforecasters")
  supersTimeSeries <- cbind(supersTimeSeries, csv$supersN)
  names(supersTimeSeries) <- c("year", "currentDate", "hdTrim", "group", "n")
  for (i in 1:nrow(supersTimeSeries)) {
    if (supersTimeSeries[i, ]$n < 10) {
      supersTimeSeries[i, ]$hdTrim <- NA
    }
  }
  supersTimeSeries$group <- paste0(supersTimeSeries$group[1], " (N = ", supersTimeSeries$n[length(supersTimeSeries$n)], ")")
  plotTable <- rbind(plotTable, supersTimeSeries)

  expertsTimeSeries <- csv %>%
    select(year, currentDate, expertsHdTrim) %>%
    mutate(group = "Experts")
  expertsTimeSeries <- cbind(expertsTimeSeries, csv$expertsN)
  names(expertsTimeSeries) <- c("year", "currentDate", "hdTrim", "group", "n")
  for (i in 1:nrow(expertsTimeSeries)) {
    if (expertsTimeSeries[i, ]$n < 10) {
      expertsTimeSeries[i, ]$hdTrim <- NA
    }
  }
  expertsTimeSeries$group <- paste0(expertsTimeSeries$group[1], " (N = ", expertsTimeSeries$n[length(expertsTimeSeries$n)], ")")
  plotTable <- rbind(plotTable, expertsTimeSeries)

  domainExpertsTimeSeries <- csv %>%
    select(year, currentDate, domainExpertsHdTrim) %>%
    mutate(group = "Domain Experts")
  domainExpertsTimeSeries <- cbind(domainExpertsTimeSeries, csv$domainExpertsN)
  names(domainExpertsTimeSeries) <- c("year", "currentDate", "hdTrim", "group", "n")
  if (!all(is.na(domainExpertsTimeSeries$median)) & any(domainExpertsTimeSeries$n >= 4)) {
    for (i in 1:nrow(domainExpertsTimeSeries)) {
      if (domainExpertsTimeSeries[i, ]$n < 4) {
        domainExpertsTimeSeries[i, ]$hdTrim <- NA
      }
    }
    if (any(!is.na(domainExpertsTimeSeries$n))) {
      domainExpertsTimeSeries$group <- paste0(domainExpertsTimeSeries$group[1], " (N = ", domainExpertsTimeSeries$n[length(domainExpertsTimeSeries$n)], ")")
      plotTable <- rbind(plotTable, domainExpertsTimeSeries)
    }
  }

  plotTable$currentDate <- ymd(plotTable$currentDate)

  plot <- ggplot(plotTable, aes(x = currentDate, y = hdTrim, group = group, fill = group)) +
    geom_line() +
    ylab("HD Trim") +
    xlab("Date") +
    labs(title = paste(title)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "by", csv$year[1], "(All)"), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group))))
  plot$labels$color <- ""

  ggsave(paste0(currentSetName, " - Teams [All] (", csv$year[1], " ", csv$beliefSet[1], ").png"), plot, width = 9.18, height = 5.78, units = c("in"))

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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "by", csv$year[1], "(Supers)"), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group))))
  plot$labels$color <- ""

  ggsave(paste0(currentSetName, " - Teams [Supers] (", csv$year[1], " ", csv$beliefSet[1], ").png"), plot, width = 9.18, height = 5.78, units = c("in"))

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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "by", csv$year[1], "(Experts)"), subtitle = subtitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group))))
  plot$labels$color <- ""

  ggsave(paste0(currentSetName, " - Teams [Experts] (", csv$year[1], " ", csv$beliefSet[1], ").png"), plot, width = 9.18, height = 5.78, units = c("in"))
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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(All)"), subtitle = distrib) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group)))) +
    ylim(NA, as.numeric(quantile(plotTable$median, 0.95, na.rm = TRUE)))
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0(currentSetName, " - Teams [All] (", distrib, "%).png")), plot, width = 9.18, height = 5.78, units = c("in"))

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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(Supers)"), subtitle = distrib) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group)))) +
    ylim(NA, as.numeric(quantile(plotTable$median, 0.95, na.rm = TRUE)))
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0(currentSetName, " - Teams [Supers] (", distrib, "%).png")), plot, width = 9.18, height = 5.78, units = c("in"))

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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(Experts)"), subtitle = distrib) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group)))) +
    ylim(NA, as.numeric(quantile(plotTable$median, 0.95, na.rm = TRUE)))
  plot$labels$color <- ""

  ggsave(gsub("%", "%%", paste0(currentSetName, " - Teams [Experts] (", distrib, "%).png")), plot, width = 9.18, height = 5.78, units = c("in"))
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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(All)"), subtitle = paste(year, distrib)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group)))) +
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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(Supers)"), subtitle = paste(year, distrib)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group)))) +
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

  plotTable <- boot_results(plotTable) 
  plot <- ggplot(plotTable, aes(x = currentDate, y = median, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = confint_lower, ymax = confint_upper, fill = group), alpha = 0.2) + ylab("Median") +
    xlab("Date") +
    labs(title = paste(title, "(Experts)"), subtitle = paste(year, distrib)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    scale_color_manual(values = cb_pal(length(unique(plotTable$group)))) +
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
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    geom_hline(yintercept = 0)
  plot$labels$color <- ""
  ggsave(gsub("%", "%%", paste0(title, " OVERALL SALIENCE.png")), plot, width = 9.18, height = 5.78, units = c("in"))

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
        legend.title = element_text(size = 9, vjust = -37)
      ) +
      geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
      geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
      geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
      geom_hline(yintercept = 0)
    plot$labels$color <- ""
    ggsave(gsub("%", "%%", paste0(title, " SUPERS VS DOMAIN EXP VS GENERALISTS SALIENCE.png")), plot, width = 9.18, height = 5.78, units = c("in"))
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
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_vline(xintercept = phaseTwoMedian, linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 8 25"), linetype = "dashed") +
    geom_vline(xintercept = ymd("2022 10 3"), linetype = "dashed") +
    geom_hline(yintercept = 0)
  plot$labels$color <- ""
  ggsave(gsub("%", "%%", paste0(title, " TEAMS SALIENCE.png")), plot, width = 9.18, height = 5.78, units = c("in"))
}

rs_quintile_plot <- function(tbl, title, subtitle) {
  #' Boxplot for RS quintiles
  #' 
  #' @export

  #plot <- ggplot(tbl, aes(x = quintile, y = forecast, group = quintile)) +
  plot <-  ggplot(tbl, aes(x=quintile, y=forecast, fill = userType)) +
    geom_boxplot(outlier.shape = NA) +
    ylab("Forecast") +
    xlab("Quintile") +
    labs(title = title, subtitle = subtitle) +
    theme_bw() +
    coord_trans(y = pseudo_log_trans(base = 10), ylim = c(0, 100)) +
    scale_y_continuous(breaks = c(0, 0.5, 1, 10, 25, 50, 75, 100)) +
    #scale_color_manual(values = cb_pal) +
    scale_fill_manual(values = cb_pal) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 9, vjust = -37)
    ) +
    geom_point(position=position_jitterdodge(), aes(x=quintile, y=forecast, group=userType)) +

  #if (length(unique(tbl$userType)) == 2) {
  #  plot <- plot +
  #    geom_label(aes(x=0.815, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #    geom_label(aes(x=1.195, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="experts") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="experts") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2])  +
  #    geom_label(aes(x=1.815, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #    geom_label(aes(x=2.195, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="experts") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="experts") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2])  +
  #    geom_label(aes(x=2.815, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #    geom_label(aes(x=3.195, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="experts") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="experts") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2])  +
  #    geom_label(aes(x=3.815, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #    geom_label(aes(x=4.195, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="experts") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="experts") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2])  +
  #    geom_label(aes(x=4.815, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #    geom_label(aes(x=5.195, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="experts") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="experts") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2])
  #}
  #else if (length(unique(tbl$userType)) == 3) {
  #  users <- c("supers", "x-risk general experts", "other experts")
  #  offsets <- c(-.25, 0, .25)
  #  for (i in 1:5) {
  #    for (j in 1:length(users)) {
  #      med <- median((tbl %>% filter(quintile == paste0("Q", i)) %>% filter(userType==users[j]) %>% select(forecast))$forecast)
  #      plot <- plot +
  #        geom_label(aes(x=i + offsets[j], y=med, label=med, group=userType), fill=cb_pal[j])
  #    }
  #  }
    geom_label(aes(x=0.75, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
    geom_label(aes(x=1, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
    geom_label(aes(x=1.25, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[3]) +
    geom_label(aes(x=1.75, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
    geom_label(aes(x=2, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
    geom_label(aes(x=2.25, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[3]) +
    geom_label(aes(x=2.75, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
    geom_label(aes(x=3, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
    geom_label(aes(x=3.25, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[3]) +
    geom_label(aes(x=3.75, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
    geom_label(aes(x=4, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
    geom_label(aes(x=4.25, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[3]) +
    geom_label(aes(x=4.75, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
    geom_label(aes(x=5, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
    geom_label(aes(x=5.25, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[3])
  
    #geom_point(position = position_jitterdodge(), aes(x = quintile, y = forecast, color = userType)) +
    #geom_label(aes(x = 1, y = median((tbl %>% filter(quintile == "Q1") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q1") %>% select(forecast))$forecast), group = userType)) +
    #geom_label(aes(x = 2, y = median((tbl %>% filter(quintile == "Q2") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q2") %>% select(forecast))$forecast), group = userType)) +
    #geom_label(aes(x = 3, y = median((tbl %>% filter(quintile == "Q3") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q3") %>% select(forecast))$forecast), group = userType)) +
    #geom_label(aes(x = 4, y = median((tbl %>% filter(quintile == "Q4") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q4") %>% select(forecast))$forecast), group = userType)) +
    #geom_label(aes(x = 5, y = median((tbl %>% filter(quintile == "Q5") %>% select(forecast))$forecast), label = median((tbl %>% filter(quintile == "Q5") %>% select(forecast))$forecast), group = userType))
  # geom_label(aes(x=0.717, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[1]) +
  #   geom_label(aes(x=0.905, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[2]) +
  #   geom_label(aes(x=1.095, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[3]) +
  #   geom_label(aes(x=1.28, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[4]) +
  #   geom_label(aes(x=1.717, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[1]) +
  #   geom_label(aes(x=1.905, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[2]) +
  #   geom_label(aes(x=2.095, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[3]) +
  #   geom_label(aes(x=2.28, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[4]) +
  #   geom_label(aes(x=2.717, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[1]) +
  #   geom_label(aes(x=2.905, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[2]) +
  #   geom_label(aes(x=3.095, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[3]) +
  #   geom_label(aes(x=3.28, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[4]) +
  #   geom_label(aes(x=3.717, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[1]) +
  #   geom_label(aes(x=3.905, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[2]) +
  #   geom_label(aes(x=4.095, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[3]) +
  #   geom_label(aes(x=4.28, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[4]) +
  #   geom_label(aes(x=4.717, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[1]) +
  #   geom_label(aes(x=4.905, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("eneral", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[2]) +
  #   geom_label(aes(x=5.095, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("ther", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[3]) +
  #   geom_label(aes(x=5.28, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(4)[4])
  # geom_label(aes(x=0.69, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[1]) +
  #   geom_label(aes(x=0.82, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="ai") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="ai") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[2]) +
  #   geom_label(aes(x=0.94, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[3]) +
  #   geom_label(aes(x=1.06, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="climate") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="climate") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[4]) +
  #   geom_label(aes(x=1.18, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[5]) +
  #   geom_label(aes(x=1.31, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[6]) +
  #   geom_label(aes(x=1.69, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[1]) +
  #   geom_label(aes(x=1.82, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="ai") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="ai") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[2]) +
  #   geom_label(aes(x=1.94, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[3]) +
  #   geom_label(aes(x=2.06, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="climate") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="climate") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[4]) +
  #   geom_label(aes(x=2.18, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[5]) +
  #   geom_label(aes(x=2.31, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[6]) +
  #   geom_label(aes(x=2.69, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[1]) +
  #   geom_label(aes(x=2.82, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="ai") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="ai") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[2]) +
  #   geom_label(aes(x=2.94, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[3]) +
  #   geom_label(aes(x=3.06, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="climate") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="climate") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[4]) +
  #   geom_label(aes(x=3.18, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[5]) +
  #   geom_label(aes(x=3.31, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[6]) +
  #   geom_label(aes(x=3.69, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[1]) +
  #   geom_label(aes(x=3.82, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="ai") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="ai") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[2]) +
  #   geom_label(aes(x=3.94, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[3]) +
  #   geom_label(aes(x=4.06, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="climate") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="climate") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[4]) +
  #   geom_label(aes(x=4.18, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[5]) +
  #   geom_label(aes(x=4.31, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[6]) +
  #   geom_label(aes(x=4.69, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[1]) +
  #   geom_label(aes(x=4.82, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="ai") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="ai") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[2]) +
  #   geom_label(aes(x=4.94, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="biorisk") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[3]) +
  #   geom_label(aes(x=5.06, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="climate") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="climate") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[4]) +
  #   geom_label(aes(x=5.18, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="nuclear") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[5]) +
  #   geom_label(aes(x=5.31, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=hue_pal()(6)[6])
  # geom_label(aes(x=0.75, y=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #   geom_label(aes(x=1, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
  #   geom_label(aes(x=1.25, y=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q1") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=cb_pal[3]) +
  #   geom_label(aes(x=1.75, y=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #   geom_label(aes(x=2, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
  #   geom_label(aes(x=2.25, y=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q2") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=cb_pal[3]) +
  #   geom_label(aes(x=2.75, y=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #   geom_label(aes(x=3, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
  #   geom_label(aes(x=3.25, y=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q3") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=cb_pal[3]) +
  #   geom_label(aes(x=3.75, y=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  #   geom_label(aes(x=4, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
  #   geom_label(aes(x=4.25, y=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q4") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=cb_pal[3]) +
  # geom_label(aes(x=4.75, y=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(grepl("uper", userType)) %>% select(forecast))$forecast), group=userType), fill=cb_pal[1]) +
  # geom_label(aes(x=5, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="SMEs") %>% select(forecast))$forecast), group=userType), fill=cb_pal[2]) +
  # geom_label(aes(x=5.25, y=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), label=median((tbl %>% filter(quintile == "Q5") %>% filter(userType=="x-risk generalists") %>% select(forecast))$forecast), group=userType), fill=cb_pal[3])

  plot$labels$colour <- ""
  plot$labels$fill <- ""

  return(plot)
}
