library(dplyr)
library(docstring)

convergenceInit <- function() {
  #' Initialize empty convergence table.
  #'
  #' How much did teams converge (within-team)?
  #'
  #' @return Empty table with convergence columns.

  return(data.frame(
    setName = character(0),
    questionName = character(0),
    answerText = character(0),
    t336_init1_stage1_sd = numeric(0),
    t336_init1_stage2_sd = numeric(0),
    t336_init1_stage3_sd = numeric(0),
    t336_init1_stage4_sd = numeric(0),
    t336_init2_stage2_sd = numeric(0),
    t336_init2_stage3_sd = numeric(0),
    t336_init2_stage4_sd = numeric(0),
    t336_init3_stage3_sd = numeric(0),
    t336_init3_stage4_sd = numeric(0),
    t336_init1_stage1_sd_super = numeric(0),
    t336_init1_stage2_sd_super = numeric(0),
    t336_init1_stage3_sd_super = numeric(0),
    t336_init1_stage4_sd_super = numeric(0),
    t336_init2_stage2_sd_super = numeric(0),
    t336_init2_stage3_sd_super = numeric(0),
    t336_init2_stage4_sd_super = numeric(0),
    t336_init3_stage3_sd_super = numeric(0),
    t336_init3_stage4_sd_super = numeric(0),
    t336_init1_stage1_sd_expert = numeric(0),
    t336_init1_stage2_sd_expert = numeric(0),
    t336_init1_stage3_sd_expert = numeric(0),
    t336_init1_stage4_sd_expert = numeric(0),
    t336_init2_stage2_sd_expert = numeric(0),
    t336_init2_stage3_sd_expert = numeric(0),
    t336_init2_stage4_sd_expert = numeric(0),
    t336_init3_stage3_sd_expert = numeric(0),
    t336_init3_stage4_sd_expert = numeric(0),
    t337_init1_stage1_sd = numeric(0),
    t337_init1_stage2_sd = numeric(0),
    t337_init1_stage3_sd = numeric(0),
    t337_init1_stage4_sd = numeric(0),
    t337_init2_stage2_sd = numeric(0),
    t337_init2_stage3_sd = numeric(0),
    t337_init2_stage4_sd = numeric(0),
    t337_init3_stage3_sd = numeric(0),
    t337_init3_stage4_sd = numeric(0),
    t337_init1_stage1_sd_super = numeric(0),
    t337_init1_stage2_sd_super = numeric(0),
    t337_init1_stage3_sd_super = numeric(0),
    t337_init1_stage4_sd_super = numeric(0),
    t337_init2_stage2_sd_super = numeric(0),
    t337_init2_stage3_sd_super = numeric(0),
    t337_init2_stage4_sd_super = numeric(0),
    t337_init3_stage3_sd_super = numeric(0),
    t337_init3_stage4_sd_super = numeric(0),
    t337_init1_stage1_sd_expert = numeric(0),
    t337_init1_stage2_sd_expert = numeric(0),
    t337_init1_stage3_sd_expert = numeric(0),
    t337_init1_stage4_sd_expert = numeric(0),
    t337_init2_stage2_sd_expert = numeric(0),
    t337_init2_stage3_sd_expert = numeric(0),
    t337_init2_stage4_sd_expert = numeric(0),
    t337_init3_stage3_sd_expert = numeric(0),
    t337_init3_stage4_sd_expert = numeric(0),
    t338_init1_stage1_sd = numeric(0),
    t338_init1_stage2_sd = numeric(0),
    t338_init1_stage3_sd = numeric(0),
    t338_init1_stage4_sd = numeric(0),
    t338_init2_stage2_sd = numeric(0),
    t338_init2_stage3_sd = numeric(0),
    t338_init2_stage4_sd = numeric(0),
    t338_init3_stage3_sd = numeric(0),
    t338_init3_stage4_sd = numeric(0),
    t338_init1_stage1_sd_super = numeric(0),
    t338_init1_stage2_sd_super = numeric(0),
    t338_init1_stage3_sd_super = numeric(0),
    t338_init1_stage4_sd_super = numeric(0),
    t338_init2_stage2_sd_super = numeric(0),
    t338_init2_stage3_sd_super = numeric(0),
    t338_init2_stage4_sd_super = numeric(0),
    t338_init3_stage3_sd_super = numeric(0),
    t338_init3_stage4_sd_super = numeric(0),
    t338_init1_stage1_sd_expert = numeric(0),
    t338_init1_stage2_sd_expert = numeric(0),
    t338_init1_stage3_sd_expert = numeric(0),
    t338_init1_stage4_sd_expert = numeric(0),
    t338_init2_stage2_sd_expert = numeric(0),
    t338_init2_stage3_sd_expert = numeric(0),
    t338_init2_stage4_sd_expert = numeric(0),
    t338_init3_stage3_sd_expert = numeric(0),
    t338_init3_stage4_sd_expert = numeric(0),
    t339_init1_stage1_sd = numeric(0),
    t339_init1_stage2_sd = numeric(0),
    t339_init1_stage3_sd = numeric(0),
    t339_init1_stage4_sd = numeric(0),
    t339_init2_stage2_sd = numeric(0),
    t339_init2_stage3_sd = numeric(0),
    t339_init2_stage4_sd = numeric(0),
    t339_init3_stage3_sd = numeric(0),
    t339_init3_stage4_sd = numeric(0),
    t339_init1_stage1_sd_super = numeric(0),
    t339_init1_stage2_sd_super = numeric(0),
    t339_init1_stage3_sd_super = numeric(0),
    t339_init1_stage4_sd_super = numeric(0),
    t339_init2_stage2_sd_super = numeric(0),
    t339_init2_stage3_sd_super = numeric(0),
    t339_init2_stage4_sd_super = numeric(0),
    t339_init3_stage3_sd_super = numeric(0),
    t339_init3_stage4_sd_super = numeric(0),
    t339_init1_stage1_sd_expert = numeric(0),
    t339_init1_stage2_sd_expert = numeric(0),
    t339_init1_stage3_sd_expert = numeric(0),
    t339_init1_stage4_sd_expert = numeric(0),
    t339_init2_stage2_sd_expert = numeric(0),
    t339_init2_stage3_sd_expert = numeric(0),
    t339_init2_stage4_sd_expert = numeric(0),
    t339_init3_stage3_sd_expert = numeric(0),
    t339_init3_stage4_sd_expert = numeric(0),
    t340_init1_stage1_sd = numeric(0),
    t340_init1_stage2_sd = numeric(0),
    t340_init1_stage3_sd = numeric(0),
    t340_init1_stage4_sd = numeric(0),
    t340_init2_stage2_sd = numeric(0),
    t340_init2_stage3_sd = numeric(0),
    t340_init2_stage4_sd = numeric(0),
    t340_init3_stage3_sd = numeric(0),
    t340_init3_stage4_sd = numeric(0),
    t340_init1_stage1_sd_super = numeric(0),
    t340_init1_stage2_sd_super = numeric(0),
    t340_init1_stage3_sd_super = numeric(0),
    t340_init1_stage4_sd_super = numeric(0),
    t340_init2_stage2_sd_super = numeric(0),
    t340_init2_stage3_sd_super = numeric(0),
    t340_init2_stage4_sd_super = numeric(0),
    t340_init3_stage3_sd_super = numeric(0),
    t340_init3_stage4_sd_super = numeric(0),
    t340_init1_stage1_sd_expert = numeric(0),
    t340_init1_stage2_sd_expert = numeric(0),
    t340_init1_stage3_sd_expert = numeric(0),
    t340_init1_stage4_sd_expert = numeric(0),
    t340_init2_stage2_sd_expert = numeric(0),
    t340_init2_stage3_sd_expert = numeric(0),
    t340_init2_stage4_sd_expert = numeric(0),
    t340_init3_stage3_sd_expert = numeric(0),
    t340_init3_stage4_sd_expert = numeric(0),
    t341_init1_stage1_sd = numeric(0),
    t341_init1_stage2_sd = numeric(0),
    t341_init1_stage3_sd = numeric(0),
    t341_init1_stage4_sd = numeric(0),
    t341_init2_stage2_sd = numeric(0),
    t341_init2_stage3_sd = numeric(0),
    t341_init2_stage4_sd = numeric(0),
    t341_init3_stage3_sd = numeric(0),
    t341_init3_stage4_sd = numeric(0),
    t341_init1_stage1_sd_super = numeric(0),
    t341_init1_stage2_sd_super = numeric(0),
    t341_init1_stage3_sd_super = numeric(0),
    t341_init1_stage4_sd_super = numeric(0),
    t341_init2_stage2_sd_super = numeric(0),
    t341_init2_stage3_sd_super = numeric(0),
    t341_init2_stage4_sd_super = numeric(0),
    t341_init3_stage3_sd_super = numeric(0),
    t341_init3_stage4_sd_super = numeric(0),
    t341_init1_stage1_sd_expert = numeric(0),
    t341_init1_stage2_sd_expert = numeric(0),
    t341_init1_stage3_sd_expert = numeric(0),
    t341_init1_stage4_sd_expert = numeric(0),
    t341_init2_stage2_sd_expert = numeric(0),
    t341_init2_stage3_sd_expert = numeric(0),
    t341_init2_stage4_sd_expert = numeric(0),
    t341_init3_stage3_sd_expert = numeric(0),
    t341_init3_stage4_sd_expert = numeric(0),
    t342_init1_stage1_sd = numeric(0),
    t342_init1_stage2_sd = numeric(0),
    t342_init1_stage3_sd = numeric(0),
    t342_init1_stage4_sd = numeric(0),
    t342_init2_stage2_sd = numeric(0),
    t342_init2_stage3_sd = numeric(0),
    t342_init2_stage4_sd = numeric(0),
    t342_init3_stage3_sd = numeric(0),
    t342_init3_stage4_sd = numeric(0),
    t343_init1_stage1_sd = numeric(0),
    t343_init1_stage2_sd = numeric(0),
    t343_init1_stage3_sd = numeric(0),
    t343_init1_stage4_sd = numeric(0),
    t343_init2_stage2_sd = numeric(0),
    t343_init2_stage3_sd = numeric(0),
    t343_init2_stage4_sd = numeric(0),
    t343_init3_stage3_sd = numeric(0),
    t343_init3_stage4_sd = numeric(0),
    t344_init1_stage1_sd = numeric(0),
    t344_init1_stage2_sd = numeric(0),
    t344_init1_stage3_sd = numeric(0),
    t344_init1_stage4_sd = numeric(0),
    t344_init2_stage2_sd = numeric(0),
    t344_init2_stage3_sd = numeric(0),
    t344_init2_stage4_sd = numeric(0),
    t344_init3_stage3_sd = numeric(0),
    t344_init3_stage4_sd = numeric(0),
    t345_init1_stage1_sd = numeric(0),
    t345_init1_stage2_sd = numeric(0),
    t345_init1_stage3_sd = numeric(0),
    t345_init1_stage4_sd = numeric(0),
    t345_init2_stage2_sd = numeric(0),
    t345_init2_stage3_sd = numeric(0),
    t345_init2_stage4_sd = numeric(0),
    t345_init3_stage3_sd = numeric(0),
    t345_init3_stage4_sd = numeric(0)
  ))
}

convergenceAdd <- function(phase1, phase2, phase3, phase4, convergenceTable) {
  #' Within-team Convergence
  #'
  #' @description How much did SD shrink over time (within-team) from stage to stage?
  #'
  #' So, (roughly) each of the variables in the convergence functions are structured as follows:
  #' t###_init#_stage#
  #' where
  #' t### refers to team
  #' init# refers to the stage in which the forecaster first submitted a
  #'    forecast (i.e. init1 means we’re subsetting just to forecasters who
  #'    submitted a forecast during stage 1; init2 is specifically excluding those
  #'    who submitted during stage 1, and just focusing on people who first
  #'    submitted a forecast on that question in stage 2)
  #' stage# refers to the end-of-stage snapshot we’re collecting data from
  #'
  #' @param phase1 End of stage/phase 1, where was everyone
  #' @param phase2 ditto
  #'
  #' @note This code has not been scrutinized
  #' @note This is NOT what we refer to as "convergence" in the XPT report.

  t336_init1_stage1 <- phase1 %>% filter(teamId == 336)
  t336_init1_stage1_sd <- sd(t336_init1_stage1$forecast, na.rm = TRUE)

  t336_init1_stage2 <- phase2 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId)
  t336_init1_stage2_sd <- sd(t336_init1_stage2$forecast, na.rm = TRUE)

  t336_init1_stage3 <- phase3 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId)
  t336_init1_stage3_sd <- sd(t336_init1_stage3$forecast, na.rm = TRUE)

  t336_init1_stage4 <- phase4 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId)
  t336_init1_stage4_sd <- sd(t336_init1_stage4$forecast, na.rm = TRUE)

  t336_init2_stage2 <- phase2 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId))
  t336_init2_stage2_sd <- sd(t336_init2_stage2$forecast, na.rm = TRUE)

  t336_init2_stage3 <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t336_init2_stage3_sd <- sd(t336_init2_stage3$forecast, na.rm = TRUE)

  t336_init2_stage4 <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t336_init2_stage4_sd <- sd(t336_init2_stage4$forecast, na.rm = TRUE)

  t336_init3_stage3 <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t336_init3_stage3_sd <- sd(t336_init3_stage3$forecast, na.rm = TRUE)

  t336_init3_stage4 <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t336_init3_stage4_sd <- sd(t336_init3_stage4$forecast, na.rm = TRUE)

  t336_init1_stage1_super <- phase1 %>%
    filter(teamId == 336) %>%
    filter(userName %in% supers)
  t336_init1_stage1_sd_super <- sd(t336_init1_stage1_super$forecast, na.rm = TRUE)

  t336_init1_stage2_super <- phase2 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t336_init1_stage2_sd_super <- sd(t336_init1_stage2_super$forecast, na.rm = TRUE)

  t336_init1_stage3_super <- phase3 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t336_init1_stage3_sd_super <- sd(t336_init1_stage3_super$forecast, na.rm = TRUE)

  t336_init1_stage4_super <- phase4 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t336_init1_stage4_sd_super <- sd(t336_init1_stage4_super$forecast, na.rm = TRUE)

  t336_init2_stage2_super <- phase2 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t336_init2_stage2_sd_super <- sd(t336_init2_stage2_super$forecast, na.rm = TRUE)

  t336_init2_stage3_super <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t336_init2_stage3_sd_super <- sd(t336_init2_stage3_super$forecast, na.rm = TRUE)

  t336_init2_stage4_super <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t336_init2_stage4_sd_super <- sd(t336_init2_stage4_super$forecast, na.rm = TRUE)

  t336_init3_stage3_super <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t336_init3_stage3_sd_super <- sd(t336_init3_stage3_super$forecast, na.rm = TRUE)

  t336_init3_stage4_super <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t336_init3_stage4_sd_super <- sd(t336_init3_stage4_super$forecast, na.rm = TRUE)

  t336_init1_stage1_expert <- phase1 %>%
    filter(teamId == 336) %>%
    filter(userName %in% expertsG1$userName)
  t336_init1_stage1_sd_expert <- sd(t336_init1_stage1_expert$forecast, na.rm = TRUE)

  t336_init1_stage2_expert <- phase2 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init1_stage2_sd_expert <- sd(t336_init1_stage2_expert$forecast, na.rm = TRUE)

  t336_init1_stage3_expert <- phase3 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init1_stage3_sd_expert <- sd(t336_init1_stage3_expert$forecast, na.rm = TRUE)

  t336_init1_stage4_expert <- phase4 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init1_stage4_sd_expert <- sd(t336_init1_stage4_expert$forecast, na.rm = TRUE)

  t336_init2_stage2_expert <- phase2 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t336_init2_stage2_sd_expert <- sd(t336_init2_stage2_expert$forecast, na.rm = TRUE)

  t336_init2_stage3_expert <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init2_stage3_sd_expert <- sd(t336_init2_stage3_expert$forecast, na.rm = TRUE)

  t336_init2_stage4_expert <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init2_stage4_sd_expert <- sd(t336_init2_stage4_expert$forecast, na.rm = TRUE)

  t336_init3_stage3_expert <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t336_init3_stage3_sd_expert <- sd(t336_init3_stage3_expert$forecast, na.rm = TRUE)

  t336_init3_stage4_expert <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init3_stage4_sd_expert <- sd(t336_init3_stage4_expert$forecast, na.rm = TRUE)

  t337_init1_stage1 <- phase1 %>% filter(teamId == 337)
  t337_init1_stage1_sd <- sd(t337_init1_stage1$forecast)

  t337_init1_stage2 <- phase2 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId)
  t337_init1_stage2_sd <- sd(t337_init1_stage2$forecast, na.rm = TRUE)

  t337_init1_stage3 <- phase3 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId)
  t337_init1_stage3_sd <- sd(t337_init1_stage3$forecast, na.rm = TRUE)

  t337_init1_stage4 <- phase4 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId)
  t337_init1_stage4_sd <- sd(t337_init1_stage4$forecast, na.rm = TRUE)

  t337_init2_stage2 <- phase2 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId))
  t337_init2_stage2_sd <- sd(t337_init2_stage2$forecast, na.rm = TRUE)

  t337_init2_stage3 <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t337_init2_stage3_sd <- sd(t337_init2_stage3$forecast, na.rm = TRUE)

  t337_init2_stage4 <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t337_init2_stage4_sd <- sd(t337_init2_stage4$forecast, na.rm = TRUE)

  t337_init3_stage3 <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t337_init3_stage3_sd <- sd(t337_init3_stage3$forecast, na.rm = TRUE)

  t337_init3_stage4 <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t337_init3_stage4_sd <- sd(t337_init3_stage4$forecast, na.rm = TRUE)

  t337_init1_stage1_super <- phase1 %>%
    filter(teamId == 337) %>%
    filter(userName %in% supers)
  t337_init1_stage1_sd_super <- sd(t337_init1_stage1_super$forecast, na.rm = TRUE)

  t337_init1_stage2_super <- phase2 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t337_init1_stage2_sd_super <- sd(t337_init1_stage2_super$forecast, na.rm = TRUE)

  t337_init1_stage3_super <- phase3 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t337_init1_stage3_sd_super <- sd(t337_init1_stage3_super$forecast, na.rm = TRUE)

  t337_init1_stage4_super <- phase4 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t337_init1_stage4_sd_super <- sd(t337_init1_stage4_super$forecast, na.rm = TRUE)

  t337_init2_stage2_super <- phase2 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t337_init2_stage2_sd_super <- sd(t337_init2_stage2_super$forecast, na.rm = TRUE)

  t337_init2_stage3_super <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t337_init2_stage3_sd_super <- sd(t337_init2_stage3_super$forecast, na.rm = TRUE)

  t337_init2_stage4_super <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t337_init2_stage4_sd_super <- sd(t337_init2_stage4_super$forecast, na.rm = TRUE)

  t337_init3_stage3_super <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t337_init3_stage3_sd_super <- sd(t337_init3_stage3_super$forecast, na.rm = TRUE)

  t337_init3_stage4_super <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t337_init3_stage4_sd_super <- sd(t337_init3_stage4_super$forecast, na.rm = TRUE)

  t337_init1_stage1_expert <- phase1 %>%
    filter(teamId == 337) %>%
    filter(userName %in% expertsG1$userName)
  t337_init1_stage1_sd_expert <- sd(t337_init1_stage1_expert$forecast, na.rm = TRUE)

  t337_init1_stage2_expert <- phase2 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init1_stage2_sd_expert <- sd(t337_init1_stage2_expert$forecast, na.rm = TRUE)

  t337_init1_stage3_expert <- phase3 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init1_stage3_sd_expert <- sd(t337_init1_stage3_expert$forecast, na.rm = TRUE)

  t337_init1_stage4_expert <- phase4 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init1_stage4_sd_expert <- sd(t337_init1_stage4_expert$forecast, na.rm = TRUE)

  t337_init2_stage2_expert <- phase2 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t337_init2_stage2_sd_expert <- sd(t337_init2_stage2_expert$forecast, na.rm = TRUE)

  t337_init2_stage3_expert <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init2_stage3_sd_expert <- sd(t337_init2_stage3_expert$forecast, na.rm = TRUE)

  t337_init2_stage4_expert <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init2_stage4_sd_expert <- sd(t337_init2_stage4_expert$forecast, na.rm = TRUE)

  t337_init3_stage3_expert <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t337_init3_stage3_sd_expert <- sd(t337_init3_stage3_expert$forecast, na.rm = TRUE)

  t337_init3_stage4_expert <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init3_stage4_sd_expert <- sd(t337_init3_stage4_expert$forecast, na.rm = TRUE)

  t338_init1_stage1 <- phase1 %>% filter(teamId == 338)
  t338_init1_stage1_sd <- sd(t338_init1_stage1$forecast, na.rm = TRUE)

  t338_init1_stage2 <- phase2 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId)
  t338_init1_stage2_sd <- sd(t338_init1_stage2$forecast, na.rm = TRUE)

  t338_init1_stage3 <- phase3 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId)
  t338_init1_stage3_sd <- sd(t338_init1_stage3$forecast, na.rm = TRUE)

  t338_init1_stage4 <- phase4 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId)
  t338_init1_stage4_sd <- sd(t338_init1_stage4$forecast, na.rm = TRUE)

  t338_init2_stage2 <- phase2 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId))
  t338_init2_stage2_sd <- sd(t338_init2_stage2$forecast, na.rm = TRUE)

  t338_init2_stage3 <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t338_init2_stage3_sd <- sd(t338_init2_stage3$forecast, na.rm = TRUE)

  t338_init2_stage4 <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t338_init2_stage4_sd <- sd(t338_init2_stage4$forecast, na.rm = TRUE)

  t338_init3_stage3 <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t338_init3_stage3_sd <- sd(t338_init3_stage3$forecast, na.rm = TRUE)

  t338_init3_stage4 <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t338_init3_stage4_sd <- sd(t338_init3_stage4$forecast, na.rm = TRUE)

  t338_init1_stage1_super <- phase1 %>%
    filter(teamId == 338) %>%
    filter(userName %in% supers)
  t338_init1_stage1_sd_super <- sd(t338_init1_stage1_super$forecast, na.rm = TRUE)

  t338_init1_stage2_super <- phase2 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t338_init1_stage2_sd_super <- sd(t338_init1_stage2_super$forecast, na.rm = TRUE)

  t338_init1_stage3_super <- phase3 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t338_init1_stage3_sd_super <- sd(t338_init1_stage3_super$forecast, na.rm = TRUE)

  t338_init1_stage4_super <- phase4 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t338_init1_stage4_sd_super <- sd(t338_init1_stage4_super$forecast, na.rm = TRUE)

  t338_init2_stage2_super <- phase2 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t338_init2_stage2_sd_super <- sd(t338_init2_stage2_super$forecast, na.rm = TRUE)

  t338_init2_stage3_super <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t338_init2_stage3_sd_super <- sd(t338_init2_stage3_super$forecast, na.rm = TRUE)

  t338_init2_stage4_super <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t338_init2_stage4_sd_super <- sd(t338_init2_stage4_super$forecast, na.rm = TRUE)

  t338_init3_stage3_super <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t338_init3_stage3_sd_super <- sd(t338_init3_stage3_super$forecast, na.rm = TRUE)

  t338_init3_stage4_super <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t338_init3_stage4_sd_super <- sd(t338_init3_stage4_super$forecast, na.rm = TRUE)

  t338_init1_stage1_expert <- phase1 %>%
    filter(teamId == 338) %>%
    filter(userName %in% expertsG1$userName)
  t338_init1_stage1_sd_expert <- sd(t338_init1_stage1_expert$forecast, na.rm = TRUE)

  t338_init1_stage2_expert <- phase2 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init1_stage2_sd_expert <- sd(t338_init1_stage2_expert$forecast, na.rm = TRUE)

  t338_init1_stage3_expert <- phase3 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init1_stage3_sd_expert <- sd(t338_init1_stage3_expert$forecast, na.rm = TRUE)

  t338_init1_stage4_expert <- phase4 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init1_stage4_sd_expert <- sd(t338_init1_stage4_expert$forecast, na.rm = TRUE)

  t338_init2_stage2_expert <- phase2 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t338_init2_stage2_sd_expert <- sd(t338_init2_stage2_expert$forecast, na.rm = TRUE)

  t338_init2_stage3_expert <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init2_stage3_sd_expert <- sd(t338_init2_stage3_expert$forecast, na.rm = TRUE)

  t338_init2_stage4_expert <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init2_stage4_sd_expert <- sd(t338_init2_stage4_expert$forecast, na.rm = TRUE)

  t338_init3_stage3_expert <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t338_init3_stage3_sd_expert <- sd(t338_init3_stage3_expert$forecast, na.rm = TRUE)

  t338_init3_stage4_expert <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init3_stage4_sd_expert <- sd(t338_init3_stage4_expert$forecast, na.rm = TRUE)

  t339_init1_stage1 <- phase1 %>% filter(teamId == 339)
  t339_init1_stage1_sd <- sd(t339_init1_stage1$forecast, na.rm = TRUE)

  t339_init1_stage2 <- phase2 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId)
  t339_init1_stage2_sd <- sd(t339_init1_stage2$forecast, na.rm = TRUE)

  t339_init1_stage3 <- phase3 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId)
  t339_init1_stage3_sd <- sd(t339_init1_stage3$forecast, na.rm = TRUE)

  t339_init1_stage4 <- phase4 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId)
  t339_init1_stage4_sd <- sd(t339_init1_stage4$forecast, na.rm = TRUE)

  t339_init2_stage2 <- phase2 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId))
  t339_init2_stage2_sd <- sd(t339_init2_stage2$forecast, na.rm = TRUE)

  t339_init2_stage3 <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t339_init2_stage3_sd <- sd(t339_init2_stage3$forecast, na.rm = TRUE)

  t339_init2_stage4 <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t339_init2_stage4_sd <- sd(t339_init2_stage4$forecast, na.rm = TRUE)

  t339_init3_stage3 <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t339_init3_stage3_sd <- sd(t339_init3_stage3$forecast, na.rm = TRUE)

  t339_init3_stage4 <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t339_init3_stage4_sd <- sd(t339_init3_stage4$forecast, na.rm = TRUE)

  t339_init1_stage1_super <- phase1 %>%
    filter(teamId == 339) %>%
    filter(userName %in% supers)
  t339_init1_stage1_sd_super <- sd(t339_init1_stage1_super$forecast, na.rm = TRUE)

  t339_init1_stage2_super <- phase2 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t339_init1_stage2_sd_super <- sd(t339_init1_stage2_super$forecast, na.rm = TRUE)

  t339_init1_stage3_super <- phase3 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t339_init1_stage3_sd_super <- sd(t339_init1_stage3_super$forecast, na.rm = TRUE)

  t339_init1_stage4_super <- phase4 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t339_init1_stage4_sd_super <- sd(t339_init1_stage4_super$forecast, na.rm = TRUE)

  t339_init2_stage2_super <- phase2 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t339_init2_stage2_sd_super <- sd(t339_init2_stage2_super$forecast, na.rm = TRUE)

  t339_init2_stage3_super <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t339_init2_stage3_sd_super <- sd(t339_init2_stage3_super$forecast, na.rm = TRUE)

  t339_init2_stage4_super <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t339_init2_stage4_sd_super <- sd(t339_init2_stage4_super$forecast, na.rm = TRUE)

  t339_init3_stage3_super <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t339_init3_stage3_sd_super <- sd(t339_init3_stage3_super$forecast, na.rm = TRUE)

  t339_init3_stage4_super <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t339_init3_stage4_sd_super <- sd(t339_init3_stage4_super$forecast, na.rm = TRUE)

  t339_init1_stage1_expert <- phase1 %>%
    filter(teamId == 339) %>%
    filter(userName %in% expertsG1$userName)
  t339_init1_stage1_sd_expert <- sd(t339_init1_stage1_expert$forecast, na.rm = TRUE)

  t339_init1_stage2_expert <- phase2 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init1_stage2_sd_expert <- sd(t339_init1_stage2_expert$forecast, na.rm = TRUE)

  t339_init1_stage3_expert <- phase3 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init1_stage3_sd_expert <- sd(t339_init1_stage3_expert$forecast, na.rm = TRUE)

  t339_init1_stage4_expert <- phase4 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init1_stage4_sd_expert <- sd(t339_init1_stage4_expert$forecast, na.rm = TRUE)

  t339_init2_stage2_expert <- phase2 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t339_init2_stage2_sd_expert <- sd(t339_init2_stage2_expert$forecast, na.rm = TRUE)

  t339_init2_stage3_expert <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init2_stage3_sd_expert <- sd(t339_init2_stage3_expert$forecast, na.rm = TRUE)

  t339_init2_stage4_expert <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init2_stage4_sd_expert <- sd(t339_init2_stage4_expert$forecast, na.rm = TRUE)

  t339_init3_stage3_expert <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t339_init3_stage3_sd_expert <- sd(t339_init3_stage3_expert$forecast, na.rm = TRUE)

  t339_init3_stage4_expert <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init3_stage4_sd_expert <- sd(t339_init3_stage4_expert$forecast, na.rm = TRUE)

  t340_init1_stage1 <- phase1 %>% filter(teamId == 340)
  t340_init1_stage1_sd <- sd(t340_init1_stage1$forecast, na.rm = TRUE)

  t340_init1_stage2 <- phase2 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId)
  t340_init1_stage2_sd <- sd(t340_init1_stage2$forecast, na.rm = TRUE)

  t340_init1_stage3 <- phase3 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId)
  t340_init1_stage3_sd <- sd(t340_init1_stage3$forecast, na.rm = TRUE)

  t340_init1_stage4 <- phase4 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId)
  t340_init1_stage4_sd <- sd(t340_init1_stage4$forecast, na.rm = TRUE)

  t340_init2_stage2 <- phase2 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId))
  t340_init2_stage2_sd <- sd(t340_init2_stage2$forecast, na.rm = TRUE)

  t340_init2_stage3 <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t340_init2_stage3_sd <- sd(t340_init2_stage3$forecast, na.rm = TRUE)

  t340_init2_stage4 <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t340_init2_stage4_sd <- sd(t340_init2_stage4$forecast, na.rm = TRUE)

  t340_init3_stage3 <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t340_init3_stage3_sd <- sd(t340_init3_stage3$forecast, na.rm = TRUE)

  t340_init3_stage4 <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t340_init3_stage4_sd <- sd(t340_init3_stage4$forecast, na.rm = TRUE)

  t340_init1_stage1_super <- phase1 %>%
    filter(teamId == 340) %>%
    filter(userName %in% supers)
  t340_init1_stage1_sd_super <- sd(t340_init1_stage1_super$forecast, na.rm = TRUE)

  t340_init1_stage2_super <- phase2 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t340_init1_stage2_sd_super <- sd(t340_init1_stage2_super$forecast, na.rm = TRUE)

  t340_init1_stage3_super <- phase3 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t340_init1_stage3_sd_super <- sd(t340_init1_stage3_super$forecast, na.rm = TRUE)

  t340_init1_stage4_super <- phase4 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t340_init1_stage4_sd_super <- sd(t340_init1_stage4_super$forecast, na.rm = TRUE)

  t340_init2_stage2_super <- phase2 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t340_init2_stage2_sd_super <- sd(t340_init2_stage2_super$forecast, na.rm = TRUE)

  t340_init2_stage3_super <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t340_init2_stage3_sd_super <- sd(t340_init2_stage3_super$forecast, na.rm = TRUE)

  t340_init2_stage4_super <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t340_init2_stage4_sd_super <- sd(t340_init2_stage4_super$forecast, na.rm = TRUE)

  t340_init3_stage3_super <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t340_init3_stage3_sd_super <- sd(t340_init3_stage3_super$forecast, na.rm = TRUE)

  t340_init3_stage4_super <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t340_init3_stage4_sd_super <- sd(t340_init3_stage4_super$forecast, na.rm = TRUE)

  t340_init1_stage1_expert <- phase1 %>%
    filter(teamId == 340) %>%
    filter(userName %in% expertsG1$userName)
  t340_init1_stage1_sd_expert <- sd(t340_init1_stage1_expert$forecast, na.rm = TRUE)

  t340_init1_stage2_expert <- phase2 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init1_stage2_sd_expert <- sd(t340_init1_stage2_expert$forecast, na.rm = TRUE)

  t340_init1_stage3_expert <- phase3 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init1_stage3_sd_expert <- sd(t340_init1_stage3_expert$forecast, na.rm = TRUE)

  t340_init1_stage4_expert <- phase4 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init1_stage4_sd_expert <- sd(t340_init1_stage4_expert$forecast, na.rm = TRUE)

  t340_init2_stage2_expert <- phase2 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t340_init2_stage2_sd_expert <- sd(t340_init2_stage2_expert$forecast, na.rm = TRUE)

  t340_init2_stage3_expert <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init2_stage3_sd_expert <- sd(t340_init2_stage3_expert$forecast, na.rm = TRUE)

  t340_init2_stage4_expert <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init2_stage4_sd_expert <- sd(t340_init2_stage4_expert$forecast, na.rm = TRUE)

  t340_init3_stage3_expert <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t340_init3_stage3_sd_expert <- sd(t340_init3_stage3_expert$forecast, na.rm = TRUE)

  t340_init3_stage4_expert <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init3_stage4_sd_expert <- sd(t340_init3_stage4_expert$forecast, na.rm = TRUE)

  t341_init1_stage1 <- phase1 %>% filter(teamId == 341)
  t341_init1_stage1_sd <- sd(t341_init1_stage1$forecast)

  t341_init1_stage2 <- phase2 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId)
  t341_init1_stage2_sd <- sd(t341_init1_stage2$forecast, na.rm = TRUE)

  t341_init1_stage3 <- phase3 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId)
  t341_init1_stage3_sd <- sd(t341_init1_stage3$forecast, na.rm = TRUE)

  t341_init1_stage4 <- phase4 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId)
  t341_init1_stage4_sd <- sd(t341_init1_stage4$forecast, na.rm = TRUE)

  t341_init2_stage2 <- phase2 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId))
  t341_init2_stage2_sd <- sd(t341_init2_stage2$forecast, na.rm = TRUE)

  t341_init2_stage3 <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t341_init2_stage3_sd <- sd(t341_init2_stage3$forecast, na.rm = TRUE)

  t341_init2_stage4 <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t341_init2_stage4_sd <- sd(t341_init2_stage4$forecast, na.rm = TRUE)

  t341_init3_stage3 <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t341_init3_stage3_sd <- sd(t341_init3_stage3$forecast, na.rm = TRUE)

  t341_init3_stage4 <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t341_init3_stage4_sd <- sd(t341_init3_stage4$forecast, na.rm = TRUE)

  t341_init1_stage1_super <- phase1 %>%
    filter(teamId == 341) %>%
    filter(userName %in% supers)
  t341_init1_stage1_sd_super <- sd(t341_init1_stage1_super$forecast, na.rm = TRUE)

  t341_init1_stage2_super <- phase2 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t341_init1_stage2_sd_super <- sd(t341_init1_stage2_super$forecast, na.rm = TRUE)

  t341_init1_stage3_super <- phase3 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t341_init1_stage3_sd_super <- sd(t341_init1_stage3_super$forecast, na.rm = TRUE)

  t341_init1_stage4_super <- phase4 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t341_init1_stage4_sd_super <- sd(t341_init1_stage4_super$forecast, na.rm = TRUE)

  t341_init2_stage2_super <- phase2 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t341_init2_stage2_sd_super <- sd(t341_init2_stage2_super$forecast, na.rm = TRUE)

  t341_init2_stage3_super <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t341_init2_stage3_sd_super <- sd(t341_init2_stage3_super$forecast, na.rm = TRUE)

  t341_init2_stage4_super <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t341_init2_stage4_sd_super <- sd(t341_init2_stage4_super$forecast, na.rm = TRUE)

  t341_init3_stage3_super <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t341_init3_stage3_sd_super <- sd(t341_init3_stage3_super$forecast, na.rm = TRUE)

  t341_init3_stage4_super <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t341_init3_stage4_sd_super <- sd(t341_init3_stage4_super$forecast, na.rm = TRUE)

  t341_init1_stage1_expert <- phase1 %>%
    filter(teamId == 341) %>%
    filter(userName %in% expertsG1$userName)
  t341_init1_stage1_sd_expert <- sd(t341_init1_stage1_expert$forecast, na.rm = TRUE)

  t341_init1_stage2_expert <- phase2 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init1_stage2_sd_expert <- sd(t341_init1_stage2_expert$forecast, na.rm = TRUE)

  t341_init1_stage3_expert <- phase3 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init1_stage3_sd_expert <- sd(t341_init1_stage3_expert$forecast, na.rm = TRUE)

  t341_init1_stage4_expert <- phase4 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init1_stage4_sd_expert <- sd(t341_init1_stage4_expert$forecast, na.rm = TRUE)

  t341_init2_stage2_expert <- phase2 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t341_init2_stage2_sd_expert <- sd(t341_init2_stage2_expert$forecast, na.rm = TRUE)

  t341_init2_stage3_expert <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init2_stage3_sd_expert <- sd(t341_init2_stage3_expert$forecast, na.rm = TRUE)

  t341_init2_stage4_expert <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init2_stage4_sd_expert <- sd(t341_init2_stage4_expert$forecast, na.rm = TRUE)

  t341_init3_stage3_expert <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t341_init3_stage3_sd_expert <- sd(t341_init3_stage3_expert$forecast, na.rm = TRUE)

  t341_init3_stage4_expert <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init3_stage4_sd_expert <- sd(t341_init3_stage4_expert$forecast, na.rm = TRUE)

  #####

  t342_init1_stage1 <- phase1 %>% filter(teamId == 342)
  t342_init1_stage1_sd <- sd(t342_init1_stage1$forecast, na.rm = TRUE)

  t342_init1_stage2 <- phase2 %>%
    filter(teamId == 342) %>%
    filter(userId %in% phase1$userId)
  t342_init1_stage2_sd <- sd(t342_init1_stage2$forecast, na.rm = TRUE)

  t342_init1_stage3 <- phase3 %>%
    filter(teamId == 342) %>%
    filter(userId %in% phase1$userId)
  t342_init1_stage3_sd <- sd(t342_init1_stage3$forecast, na.rm = TRUE)

  t342_init1_stage4 <- phase4 %>%
    filter(teamId == 342) %>%
    filter(userId %in% phase1$userId)
  t342_init1_stage4_sd <- sd(t342_init1_stage4$forecast, na.rm = TRUE)

  t342_init2_stage2 <- phase2 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId))
  t342_init2_stage2_sd <- sd(t342_init2_stage2$forecast, na.rm = TRUE)

  t342_init2_stage3 <- phase3 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t342_init2_stage3_sd <- sd(t342_init2_stage3$forecast, na.rm = TRUE)

  t342_init2_stage4 <- phase4 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t342_init2_stage4_sd <- sd(t342_init2_stage4$forecast, na.rm = TRUE)

  t342_init3_stage3 <- phase3 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t342_init3_stage3_sd <- sd(t342_init3_stage3$forecast, na.rm = TRUE)

  t342_init3_stage4 <- phase4 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t342_init3_stage4_sd <- sd(t342_init3_stage4$forecast, na.rm = TRUE)

  t343_init1_stage1 <- phase1 %>% filter(teamId == 343)
  t343_init1_stage1_sd <- sd(t343_init1_stage1$forecast, na.rm = TRUE)

  t343_init1_stage2 <- phase2 %>%
    filter(teamId == 343) %>%
    filter(userId %in% phase1$userId)
  t343_init1_stage2_sd <- sd(t343_init1_stage2$forecast, na.rm = TRUE)

  t343_init1_stage3 <- phase3 %>%
    filter(teamId == 343) %>%
    filter(userId %in% phase1$userId)
  t343_init1_stage3_sd <- sd(t343_init1_stage3$forecast, na.rm = TRUE)

  t343_init1_stage4 <- phase4 %>%
    filter(teamId == 343) %>%
    filter(userId %in% phase1$userId)
  t343_init1_stage4_sd <- sd(t343_init1_stage4$forecast, na.rm = TRUE)

  t343_init2_stage2 <- phase2 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId))
  t343_init2_stage2_sd <- sd(t343_init2_stage2$forecast, na.rm = TRUE)

  t343_init2_stage3 <- phase3 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t343_init2_stage3_sd <- sd(t343_init2_stage3$forecast, na.rm = TRUE)

  t343_init2_stage4 <- phase4 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t343_init2_stage4_sd <- sd(t343_init2_stage4$forecast, na.rm = TRUE)

  t343_init3_stage3 <- phase3 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t343_init3_stage3_sd <- sd(t343_init3_stage3$forecast, na.rm = TRUE)

  t343_init3_stage4 <- phase4 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t343_init3_stage4_sd <- sd(t343_init3_stage4$forecast, na.rm = TRUE)

  t344_init1_stage1 <- phase1 %>% filter(teamId == 344)
  t344_init1_stage1_sd <- sd(t344_init1_stage1$forecast, na.rm = TRUE)

  t344_init1_stage2 <- phase2 %>%
    filter(teamId == 344) %>%
    filter(userId %in% phase1$userId)
  t344_init1_stage2_sd <- sd(t344_init1_stage2$forecast, na.rm = TRUE)

  t344_init1_stage3 <- phase3 %>%
    filter(teamId == 344) %>%
    filter(userId %in% phase1$userId)
  t344_init1_stage3_sd <- sd(t344_init1_stage3$forecast, na.rm = TRUE)

  t344_init1_stage4 <- phase4 %>%
    filter(teamId == 344) %>%
    filter(userId %in% phase1$userId)
  t344_init1_stage4_sd <- sd(t344_init1_stage4$forecast, na.rm = TRUE)

  t344_init2_stage2 <- phase2 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId))
  t344_init2_stage2_sd <- sd(t344_init2_stage2$forecast, na.rm = TRUE)

  t344_init2_stage3 <- phase3 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t344_init2_stage3_sd <- sd(t344_init2_stage3$forecast, na.rm = TRUE)

  t344_init2_stage4 <- phase4 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t344_init2_stage4_sd <- sd(t344_init2_stage4$forecast, na.rm = TRUE)

  t344_init3_stage3 <- phase3 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t344_init3_stage3_sd <- sd(t344_init3_stage3$forecast, na.rm = TRUE)

  t344_init3_stage4 <- phase4 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t344_init3_stage4_sd <- sd(t344_init3_stage4$forecast, na.rm = TRUE)

  t345_init1_stage1 <- phase1 %>% filter(teamId == 345)
  t345_init1_stage1_sd <- sd(t345_init1_stage1$forecast, na.rm = TRUE)

  t345_init1_stage2 <- phase2 %>%
    filter(teamId == 345) %>%
    filter(userId %in% phase1$userId)
  t345_init1_stage2_sd <- sd(t345_init1_stage2$forecast, na.rm = TRUE)

  t345_init1_stage3 <- phase3 %>%
    filter(teamId == 345) %>%
    filter(userId %in% phase1$userId)
  t345_init1_stage3_sd <- sd(t345_init1_stage3$forecast, na.rm = TRUE)

  t345_init1_stage4 <- phase4 %>%
    filter(teamId == 345) %>%
    filter(userId %in% phase1$userId)
  t345_init1_stage4_sd <- sd(t345_init1_stage4$forecast, na.rm = TRUE)

  t345_init2_stage2 <- phase2 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId))
  t345_init2_stage2_sd <- sd(t345_init2_stage2$forecast, na.rm = TRUE)

  t345_init2_stage3 <- phase3 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t345_init2_stage3_sd <- sd(t345_init2_stage3$forecast, na.rm = TRUE)

  t345_init2_stage4 <- phase4 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t345_init2_stage4_sd <- sd(t345_init2_stage4$forecast, na.rm = TRUE)

  t345_init3_stage3 <- phase3 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t345_init3_stage3_sd <- sd(t345_init3_stage3$forecast, na.rm = TRUE)

  t345_init3_stage4 <- phase4 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t345_init3_stage4_sd <- sd(t345_init3_stage4$forecast, na.rm = TRUE)

  return(data.frame(
    setName = phase1$setName[1],
    questionName = phase1$questionName[1],
    answerText = phase1$answerText[1],
    t336_init1_stage1_sd,
    t336_init1_stage2_sd,
    t336_init1_stage3_sd,
    t336_init1_stage4_sd,
    t336_init2_stage2_sd,
    t336_init2_stage3_sd,
    t336_init2_stage4_sd,
    t336_init3_stage3_sd,
    t336_init3_stage4_sd,
    t336_init1_stage1_sd_super,
    t336_init1_stage2_sd_super,
    t336_init1_stage3_sd_super,
    t336_init1_stage4_sd_super,
    t336_init2_stage2_sd_super,
    t336_init2_stage3_sd_super,
    t336_init2_stage4_sd_super,
    t336_init3_stage3_sd_super,
    t336_init3_stage4_sd_super,
    t336_init1_stage1_sd_expert,
    t336_init1_stage2_sd_expert,
    t336_init1_stage3_sd_expert,
    t336_init1_stage4_sd_expert,
    t336_init2_stage2_sd_expert,
    t336_init2_stage3_sd_expert,
    t336_init2_stage4_sd_expert,
    t336_init3_stage3_sd_expert,
    t336_init3_stage4_sd_expert,
    t337_init1_stage1_sd,
    t337_init1_stage2_sd,
    t337_init1_stage3_sd,
    t337_init1_stage4_sd,
    t337_init2_stage2_sd,
    t337_init2_stage3_sd,
    t337_init2_stage4_sd,
    t337_init3_stage3_sd,
    t337_init3_stage4_sd,
    t337_init1_stage1_sd_super,
    t337_init1_stage2_sd_super,
    t337_init1_stage3_sd_super,
    t337_init1_stage4_sd_super,
    t337_init2_stage2_sd_super,
    t337_init2_stage3_sd_super,
    t337_init2_stage4_sd_super,
    t337_init3_stage3_sd_super,
    t337_init3_stage4_sd_super,
    t337_init1_stage1_sd_expert,
    t337_init1_stage2_sd_expert,
    t337_init1_stage3_sd_expert,
    t337_init1_stage4_sd_expert,
    t337_init2_stage2_sd_expert,
    t337_init2_stage3_sd_expert,
    t337_init2_stage4_sd_expert,
    t337_init3_stage3_sd_expert,
    t337_init3_stage4_sd_expert,
    t338_init1_stage1_sd,
    t338_init1_stage2_sd,
    t338_init1_stage3_sd,
    t338_init1_stage4_sd,
    t338_init2_stage2_sd,
    t338_init2_stage3_sd,
    t338_init2_stage4_sd,
    t338_init3_stage3_sd,
    t338_init3_stage4_sd,
    t338_init1_stage1_sd_super,
    t338_init1_stage2_sd_super,
    t338_init1_stage3_sd_super,
    t338_init1_stage4_sd_super,
    t338_init2_stage2_sd_super,
    t338_init2_stage3_sd_super,
    t338_init2_stage4_sd_super,
    t338_init3_stage3_sd_super,
    t338_init3_stage4_sd_super,
    t338_init1_stage1_sd_expert,
    t338_init1_stage2_sd_expert,
    t338_init1_stage3_sd_expert,
    t338_init1_stage4_sd_expert,
    t338_init2_stage2_sd_expert,
    t338_init2_stage3_sd_expert,
    t338_init2_stage4_sd_expert,
    t338_init3_stage3_sd_expert,
    t338_init3_stage4_sd_expert,
    t339_init1_stage1_sd,
    t339_init1_stage2_sd,
    t339_init1_stage3_sd,
    t339_init1_stage4_sd,
    t339_init2_stage2_sd,
    t339_init2_stage3_sd,
    t339_init2_stage4_sd,
    t339_init3_stage3_sd,
    t339_init3_stage4_sd,
    t339_init1_stage1_sd_super,
    t339_init1_stage2_sd_super,
    t339_init1_stage3_sd_super,
    t339_init1_stage4_sd_super,
    t339_init2_stage2_sd_super,
    t339_init2_stage3_sd_super,
    t339_init2_stage4_sd_super,
    t339_init3_stage3_sd_super,
    t339_init3_stage4_sd_super,
    t339_init1_stage1_sd_expert,
    t339_init1_stage2_sd_expert,
    t339_init1_stage3_sd_expert,
    t339_init1_stage4_sd_expert,
    t339_init2_stage2_sd_expert,
    t339_init2_stage3_sd_expert,
    t339_init2_stage4_sd_expert,
    t339_init3_stage3_sd_expert,
    t339_init3_stage4_sd_expert,
    t340_init1_stage1_sd,
    t340_init1_stage2_sd,
    t340_init1_stage3_sd,
    t340_init1_stage4_sd,
    t340_init2_stage2_sd,
    t340_init2_stage3_sd,
    t340_init2_stage4_sd,
    t340_init3_stage3_sd,
    t340_init3_stage4_sd,
    t340_init1_stage1_sd_super,
    t340_init1_stage2_sd_super,
    t340_init1_stage3_sd_super,
    t340_init1_stage4_sd_super,
    t340_init2_stage2_sd_super,
    t340_init2_stage3_sd_super,
    t340_init2_stage4_sd_super,
    t340_init3_stage3_sd_super,
    t340_init3_stage4_sd_super,
    t340_init1_stage1_sd_expert,
    t340_init1_stage2_sd_expert,
    t340_init1_stage3_sd_expert,
    t340_init1_stage4_sd_expert,
    t340_init2_stage2_sd_expert,
    t340_init2_stage3_sd_expert,
    t340_init2_stage4_sd_expert,
    t340_init3_stage3_sd_expert,
    t340_init3_stage4_sd_expert,
    t341_init1_stage1_sd,
    t341_init1_stage2_sd,
    t341_init1_stage3_sd,
    t341_init1_stage4_sd,
    t341_init2_stage2_sd,
    t341_init2_stage3_sd,
    t341_init2_stage4_sd,
    t341_init3_stage3_sd,
    t341_init3_stage4_sd,
    t341_init1_stage1_sd_super,
    t341_init1_stage2_sd_super,
    t341_init1_stage3_sd_super,
    t341_init1_stage4_sd_super,
    t341_init2_stage2_sd_super,
    t341_init2_stage3_sd_super,
    t341_init2_stage4_sd_super,
    t341_init3_stage3_sd_super,
    t341_init3_stage4_sd_super,
    t341_init1_stage1_sd_expert,
    t341_init1_stage2_sd_expert,
    t341_init1_stage3_sd_expert,
    t341_init1_stage4_sd_expert,
    t341_init2_stage2_sd_expert,
    t341_init2_stage3_sd_expert,
    t341_init2_stage4_sd_expert,
    t341_init3_stage3_sd_expert,
    t341_init3_stage4_sd_expert,
    t342_init1_stage1_sd,
    t342_init1_stage2_sd,
    t342_init1_stage3_sd,
    t342_init1_stage4_sd,
    t342_init2_stage2_sd,
    t342_init2_stage3_sd,
    t342_init2_stage4_sd,
    t342_init3_stage3_sd,
    t342_init3_stage4_sd,
    t343_init1_stage1_sd,
    t343_init1_stage2_sd,
    t343_init1_stage3_sd,
    t343_init1_stage4_sd,
    t343_init2_stage2_sd,
    t343_init2_stage3_sd,
    t343_init2_stage4_sd,
    t343_init3_stage3_sd,
    t343_init3_stage4_sd,
    t344_init1_stage1_sd,
    t344_init1_stage2_sd,
    t344_init1_stage3_sd,
    t344_init1_stage4_sd,
    t344_init2_stage2_sd,
    t344_init2_stage3_sd,
    t344_init2_stage4_sd,
    t344_init3_stage3_sd,
    t344_init3_stage4_sd,
    t345_init1_stage1_sd,
    t345_init1_stage2_sd,
    t345_init1_stage3_sd,
    t345_init1_stage4_sd,
    t345_init2_stage2_sd,
    t345_init2_stage3_sd,
    t345_init2_stage4_sd,
    t345_init3_stage3_sd,
    t345_init3_stage4_sd
  ))
}

convergenceAdd_active <- function(phase1, phase2, phase3, phase4, convergenceTable) {
  #' Subset to forecasters who were really committed, still active in Phase 4.
  #'
  #' @param phase1 TODO
  #' @param phase2 TODO
  #' @param phase3 TODO
  #' @param phase4 TODO
  #' @param convergenceTable TODO
  #' 
  #' @export

  active_s4_forecasters <- phase4 %>%
    filter(timestamp > ymd("2022 10 02")) %>%
    select(userId)
  active_s4_forecasters <- active_s4_forecasters[, 1]

  phase1 <- phase1 %>% filter(userId %in% active_s4_forecasters)
  phase2 <- phase2 %>% filter(userId %in% active_s4_forecasters)
  phase3 <- phase3 %>% filter(userId %in% active_s4_forecasters)
  phase4 <- phase4 %>% filter(userId %in% active_s4_forecasters)

  t336_init1_stage1 <- phase1 %>% filter(teamId == 336)
  t336_init1_stage1_sd <- sd(t336_init1_stage1$forecast, na.rm = TRUE)

  t336_init1_stage2 <- phase2 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId)
  t336_init1_stage2_sd <- sd(t336_init1_stage2$forecast, na.rm = TRUE)

  t336_init1_stage3 <- phase3 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId)
  t336_init1_stage3_sd <- sd(t336_init1_stage3$forecast, na.rm = TRUE)

  t336_init1_stage4 <- phase4 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId)
  t336_init1_stage4_sd <- sd(t336_init1_stage4$forecast, na.rm = TRUE)

  t336_init2_stage2 <- phase2 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId))
  t336_init2_stage2_sd <- sd(t336_init2_stage2$forecast, na.rm = TRUE)

  t336_init2_stage3 <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t336_init2_stage3_sd <- sd(t336_init2_stage3$forecast, na.rm = TRUE)

  t336_init2_stage4 <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t336_init2_stage4_sd <- sd(t336_init2_stage4$forecast, na.rm = TRUE)

  t336_init3_stage3 <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t336_init3_stage3_sd <- sd(t336_init3_stage3$forecast, na.rm = TRUE)

  t336_init3_stage4 <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t336_init3_stage4_sd <- sd(t336_init3_stage4$forecast, na.rm = TRUE)

  t336_init1_stage1_super <- phase1 %>%
    filter(teamId == 336) %>%
    filter(userName %in% supers)
  t336_init1_stage1_sd_super <- sd(t336_init1_stage1_super$forecast, na.rm = TRUE)

  t336_init1_stage2_super <- phase2 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t336_init1_stage2_sd_super <- sd(t336_init1_stage2_super$forecast, na.rm = TRUE)

  t336_init1_stage3_super <- phase3 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t336_init1_stage3_sd_super <- sd(t336_init1_stage3_super$forecast, na.rm = TRUE)

  t336_init1_stage4_super <- phase4 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t336_init1_stage4_sd_super <- sd(t336_init1_stage4_super$forecast, na.rm = TRUE)

  t336_init2_stage2_super <- phase2 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t336_init2_stage2_sd_super <- sd(t336_init2_stage2_super$forecast, na.rm = TRUE)

  t336_init2_stage3_super <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t336_init2_stage3_sd_super <- sd(t336_init2_stage3_super$forecast, na.rm = TRUE)

  t336_init2_stage4_super <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t336_init2_stage4_sd_super <- sd(t336_init2_stage4_super$forecast, na.rm = TRUE)

  t336_init3_stage3_super <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t336_init3_stage3_sd_super <- sd(t336_init3_stage3_super$forecast, na.rm = TRUE)

  t336_init3_stage4_super <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t336_init3_stage4_sd_super <- sd(t336_init3_stage4_super$forecast, na.rm = TRUE)

  t336_init1_stage1_expert <- phase1 %>%
    filter(teamId == 336) %>%
    filter(userName %in% expertsG1$userName)
  t336_init1_stage1_sd_expert <- sd(t336_init1_stage1_expert$forecast, na.rm = TRUE)

  t336_init1_stage2_expert <- phase2 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init1_stage2_sd_expert <- sd(t336_init1_stage2_expert$forecast, na.rm = TRUE)

  t336_init1_stage3_expert <- phase3 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init1_stage3_sd_expert <- sd(t336_init1_stage3_expert$forecast, na.rm = TRUE)

  t336_init1_stage4_expert <- phase4 %>%
    filter(teamId == 336) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init1_stage4_sd_expert <- sd(t336_init1_stage4_expert$forecast, na.rm = TRUE)

  t336_init2_stage2_expert <- phase2 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t336_init2_stage2_sd_expert <- sd(t336_init2_stage2_expert$forecast, na.rm = TRUE)

  t336_init2_stage3_expert <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init2_stage3_sd_expert <- sd(t336_init2_stage3_expert$forecast, na.rm = TRUE)

  t336_init2_stage4_expert <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init2_stage4_sd_expert <- sd(t336_init2_stage4_expert$forecast, na.rm = TRUE)

  t336_init3_stage3_expert <- phase3 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t336_init3_stage3_sd_expert <- sd(t336_init3_stage3_expert$forecast, na.rm = TRUE)

  t336_init3_stage4_expert <- phase4 %>%
    filter(teamId == 336) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t336_init3_stage4_sd_expert <- sd(t336_init3_stage4_expert$forecast, na.rm = TRUE)

  t337_init1_stage1 <- phase1 %>% filter(teamId == 337)
  t337_init1_stage1_sd <- sd(t337_init1_stage1$forecast)

  t337_init1_stage2 <- phase2 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId)
  t337_init1_stage2_sd <- sd(t337_init1_stage2$forecast, na.rm = TRUE)

  t337_init1_stage3 <- phase3 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId)
  t337_init1_stage3_sd <- sd(t337_init1_stage3$forecast, na.rm = TRUE)

  t337_init1_stage4 <- phase4 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId)
  t337_init1_stage4_sd <- sd(t337_init1_stage4$forecast, na.rm = TRUE)

  t337_init2_stage2 <- phase2 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId))
  t337_init2_stage2_sd <- sd(t337_init2_stage2$forecast, na.rm = TRUE)

  t337_init2_stage3 <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t337_init2_stage3_sd <- sd(t337_init2_stage3$forecast, na.rm = TRUE)

  t337_init2_stage4 <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t337_init2_stage4_sd <- sd(t337_init2_stage4$forecast, na.rm = TRUE)

  t337_init3_stage3 <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t337_init3_stage3_sd <- sd(t337_init3_stage3$forecast, na.rm = TRUE)

  t337_init3_stage4 <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t337_init3_stage4_sd <- sd(t337_init3_stage4$forecast, na.rm = TRUE)

  t337_init1_stage1_super <- phase1 %>%
    filter(teamId == 337) %>%
    filter(userName %in% supers)
  t337_init1_stage1_sd_super <- sd(t337_init1_stage1_super$forecast, na.rm = TRUE)

  t337_init1_stage2_super <- phase2 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t337_init1_stage2_sd_super <- sd(t337_init1_stage2_super$forecast, na.rm = TRUE)

  t337_init1_stage3_super <- phase3 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t337_init1_stage3_sd_super <- sd(t337_init1_stage3_super$forecast, na.rm = TRUE)

  t337_init1_stage4_super <- phase4 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t337_init1_stage4_sd_super <- sd(t337_init1_stage4_super$forecast, na.rm = TRUE)

  t337_init2_stage2_super <- phase2 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t337_init2_stage2_sd_super <- sd(t337_init2_stage2_super$forecast, na.rm = TRUE)

  t337_init2_stage3_super <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t337_init2_stage3_sd_super <- sd(t337_init2_stage3_super$forecast, na.rm = TRUE)

  t337_init2_stage4_super <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t337_init2_stage4_sd_super <- sd(t337_init2_stage4_super$forecast, na.rm = TRUE)

  t337_init3_stage3_super <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t337_init3_stage3_sd_super <- sd(t337_init3_stage3_super$forecast, na.rm = TRUE)

  t337_init3_stage4_super <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t337_init3_stage4_sd_super <- sd(t337_init3_stage4_super$forecast, na.rm = TRUE)

  t337_init1_stage1_expert <- phase1 %>%
    filter(teamId == 337) %>%
    filter(userName %in% expertsG1$userName)
  t337_init1_stage1_sd_expert <- sd(t337_init1_stage1_expert$forecast, na.rm = TRUE)

  t337_init1_stage2_expert <- phase2 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init1_stage2_sd_expert <- sd(t337_init1_stage2_expert$forecast, na.rm = TRUE)

  t337_init1_stage3_expert <- phase3 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init1_stage3_sd_expert <- sd(t337_init1_stage3_expert$forecast, na.rm = TRUE)

  t337_init1_stage4_expert <- phase4 %>%
    filter(teamId == 337) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init1_stage4_sd_expert <- sd(t337_init1_stage4_expert$forecast, na.rm = TRUE)

  t337_init2_stage2_expert <- phase2 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t337_init2_stage2_sd_expert <- sd(t337_init2_stage2_expert$forecast, na.rm = TRUE)

  t337_init2_stage3_expert <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init2_stage3_sd_expert <- sd(t337_init2_stage3_expert$forecast, na.rm = TRUE)

  t337_init2_stage4_expert <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init2_stage4_sd_expert <- sd(t337_init2_stage4_expert$forecast, na.rm = TRUE)

  t337_init3_stage3_expert <- phase3 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t337_init3_stage3_sd_expert <- sd(t337_init3_stage3_expert$forecast, na.rm = TRUE)

  t337_init3_stage4_expert <- phase4 %>%
    filter(teamId == 337) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t337_init3_stage4_sd_expert <- sd(t337_init3_stage4_expert$forecast, na.rm = TRUE)

  t338_init1_stage1 <- phase1 %>% filter(teamId == 338)
  t338_init1_stage1_sd <- sd(t338_init1_stage1$forecast, na.rm = TRUE)

  t338_init1_stage2 <- phase2 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId)
  t338_init1_stage2_sd <- sd(t338_init1_stage2$forecast, na.rm = TRUE)

  t338_init1_stage3 <- phase3 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId)
  t338_init1_stage3_sd <- sd(t338_init1_stage3$forecast, na.rm = TRUE)

  t338_init1_stage4 <- phase4 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId)
  t338_init1_stage4_sd <- sd(t338_init1_stage4$forecast, na.rm = TRUE)

  t338_init2_stage2 <- phase2 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId))
  t338_init2_stage2_sd <- sd(t338_init2_stage2$forecast, na.rm = TRUE)

  t338_init2_stage3 <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t338_init2_stage3_sd <- sd(t338_init2_stage3$forecast, na.rm = TRUE)

  t338_init2_stage4 <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t338_init2_stage4_sd <- sd(t338_init2_stage4$forecast, na.rm = TRUE)

  t338_init3_stage3 <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t338_init3_stage3_sd <- sd(t338_init3_stage3$forecast, na.rm = TRUE)

  t338_init3_stage4 <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t338_init3_stage4_sd <- sd(t338_init3_stage4$forecast, na.rm = TRUE)

  t338_init1_stage1_super <- phase1 %>%
    filter(teamId == 338) %>%
    filter(userName %in% supers)
  t338_init1_stage1_sd_super <- sd(t338_init1_stage1_super$forecast, na.rm = TRUE)

  t338_init1_stage2_super <- phase2 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t338_init1_stage2_sd_super <- sd(t338_init1_stage2_super$forecast, na.rm = TRUE)

  t338_init1_stage3_super <- phase3 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t338_init1_stage3_sd_super <- sd(t338_init1_stage3_super$forecast, na.rm = TRUE)

  t338_init1_stage4_super <- phase4 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t338_init1_stage4_sd_super <- sd(t338_init1_stage4_super$forecast, na.rm = TRUE)

  t338_init2_stage2_super <- phase2 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t338_init2_stage2_sd_super <- sd(t338_init2_stage2_super$forecast, na.rm = TRUE)

  t338_init2_stage3_super <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t338_init2_stage3_sd_super <- sd(t338_init2_stage3_super$forecast, na.rm = TRUE)

  t338_init2_stage4_super <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t338_init2_stage4_sd_super <- sd(t338_init2_stage4_super$forecast, na.rm = TRUE)

  t338_init3_stage3_super <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t338_init3_stage3_sd_super <- sd(t338_init3_stage3_super$forecast, na.rm = TRUE)

  t338_init3_stage4_super <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t338_init3_stage4_sd_super <- sd(t338_init3_stage4_super$forecast, na.rm = TRUE)

  t338_init1_stage1_expert <- phase1 %>%
    filter(teamId == 338) %>%
    filter(userName %in% expertsG1$userName)
  t338_init1_stage1_sd_expert <- sd(t338_init1_stage1_expert$forecast, na.rm = TRUE)

  t338_init1_stage2_expert <- phase2 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init1_stage2_sd_expert <- sd(t338_init1_stage2_expert$forecast, na.rm = TRUE)

  t338_init1_stage3_expert <- phase3 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init1_stage3_sd_expert <- sd(t338_init1_stage3_expert$forecast, na.rm = TRUE)

  t338_init1_stage4_expert <- phase4 %>%
    filter(teamId == 338) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init1_stage4_sd_expert <- sd(t338_init1_stage4_expert$forecast, na.rm = TRUE)

  t338_init2_stage2_expert <- phase2 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t338_init2_stage2_sd_expert <- sd(t338_init2_stage2_expert$forecast, na.rm = TRUE)

  t338_init2_stage3_expert <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init2_stage3_sd_expert <- sd(t338_init2_stage3_expert$forecast, na.rm = TRUE)

  t338_init2_stage4_expert <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init2_stage4_sd_expert <- sd(t338_init2_stage4_expert$forecast, na.rm = TRUE)

  t338_init3_stage3_expert <- phase3 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t338_init3_stage3_sd_expert <- sd(t338_init3_stage3_expert$forecast, na.rm = TRUE)

  t338_init3_stage4_expert <- phase4 %>%
    filter(teamId == 338) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t338_init3_stage4_sd_expert <- sd(t338_init3_stage4_expert$forecast, na.rm = TRUE)

  t339_init1_stage1 <- phase1 %>% filter(teamId == 339)
  t339_init1_stage1_sd <- sd(t339_init1_stage1$forecast, na.rm = TRUE)

  t339_init1_stage2 <- phase2 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId)
  t339_init1_stage2_sd <- sd(t339_init1_stage2$forecast, na.rm = TRUE)

  t339_init1_stage3 <- phase3 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId)
  t339_init1_stage3_sd <- sd(t339_init1_stage3$forecast, na.rm = TRUE)

  t339_init1_stage4 <- phase4 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId)
  t339_init1_stage4_sd <- sd(t339_init1_stage4$forecast, na.rm = TRUE)

  t339_init2_stage2 <- phase2 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId))
  t339_init2_stage2_sd <- sd(t339_init2_stage2$forecast, na.rm = TRUE)

  t339_init2_stage3 <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t339_init2_stage3_sd <- sd(t339_init2_stage3$forecast, na.rm = TRUE)

  t339_init2_stage4 <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t339_init2_stage4_sd <- sd(t339_init2_stage4$forecast, na.rm = TRUE)

  t339_init3_stage3 <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t339_init3_stage3_sd <- sd(t339_init3_stage3$forecast, na.rm = TRUE)

  t339_init3_stage4 <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t339_init3_stage4_sd <- sd(t339_init3_stage4$forecast, na.rm = TRUE)

  t339_init1_stage1_super <- phase1 %>%
    filter(teamId == 339) %>%
    filter(userName %in% supers)
  t339_init1_stage1_sd_super <- sd(t339_init1_stage1_super$forecast, na.rm = TRUE)

  t339_init1_stage2_super <- phase2 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t339_init1_stage2_sd_super <- sd(t339_init1_stage2_super$forecast, na.rm = TRUE)

  t339_init1_stage3_super <- phase3 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t339_init1_stage3_sd_super <- sd(t339_init1_stage3_super$forecast, na.rm = TRUE)

  t339_init1_stage4_super <- phase4 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t339_init1_stage4_sd_super <- sd(t339_init1_stage4_super$forecast, na.rm = TRUE)

  t339_init2_stage2_super <- phase2 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t339_init2_stage2_sd_super <- sd(t339_init2_stage2_super$forecast, na.rm = TRUE)

  t339_init2_stage3_super <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t339_init2_stage3_sd_super <- sd(t339_init2_stage3_super$forecast, na.rm = TRUE)

  t339_init2_stage4_super <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t339_init2_stage4_sd_super <- sd(t339_init2_stage4_super$forecast, na.rm = TRUE)

  t339_init3_stage3_super <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t339_init3_stage3_sd_super <- sd(t339_init3_stage3_super$forecast, na.rm = TRUE)

  t339_init3_stage4_super <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t339_init3_stage4_sd_super <- sd(t339_init3_stage4_super$forecast, na.rm = TRUE)

  t339_init1_stage1_expert <- phase1 %>%
    filter(teamId == 339) %>%
    filter(userName %in% expertsG1$userName)
  t339_init1_stage1_sd_expert <- sd(t339_init1_stage1_expert$forecast, na.rm = TRUE)

  t339_init1_stage2_expert <- phase2 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init1_stage2_sd_expert <- sd(t339_init1_stage2_expert$forecast, na.rm = TRUE)

  t339_init1_stage3_expert <- phase3 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init1_stage3_sd_expert <- sd(t339_init1_stage3_expert$forecast, na.rm = TRUE)

  t339_init1_stage4_expert <- phase4 %>%
    filter(teamId == 339) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init1_stage4_sd_expert <- sd(t339_init1_stage4_expert$forecast, na.rm = TRUE)

  t339_init2_stage2_expert <- phase2 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t339_init2_stage2_sd_expert <- sd(t339_init2_stage2_expert$forecast, na.rm = TRUE)

  t339_init2_stage3_expert <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init2_stage3_sd_expert <- sd(t339_init2_stage3_expert$forecast, na.rm = TRUE)

  t339_init2_stage4_expert <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init2_stage4_sd_expert <- sd(t339_init2_stage4_expert$forecast, na.rm = TRUE)

  t339_init3_stage3_expert <- phase3 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t339_init3_stage3_sd_expert <- sd(t339_init3_stage3_expert$forecast, na.rm = TRUE)

  t339_init3_stage4_expert <- phase4 %>%
    filter(teamId == 339) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t339_init3_stage4_sd_expert <- sd(t339_init3_stage4_expert$forecast, na.rm = TRUE)

  t340_init1_stage1 <- phase1 %>% filter(teamId == 340)
  t340_init1_stage1_sd <- sd(t340_init1_stage1$forecast, na.rm = TRUE)

  t340_init1_stage2 <- phase2 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId)
  t340_init1_stage2_sd <- sd(t340_init1_stage2$forecast, na.rm = TRUE)

  t340_init1_stage3 <- phase3 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId)
  t340_init1_stage3_sd <- sd(t340_init1_stage3$forecast, na.rm = TRUE)

  t340_init1_stage4 <- phase4 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId)
  t340_init1_stage4_sd <- sd(t340_init1_stage4$forecast, na.rm = TRUE)

  t340_init2_stage2 <- phase2 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId))
  t340_init2_stage2_sd <- sd(t340_init2_stage2$forecast, na.rm = TRUE)

  t340_init2_stage3 <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t340_init2_stage3_sd <- sd(t340_init2_stage3$forecast, na.rm = TRUE)

  t340_init2_stage4 <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t340_init2_stage4_sd <- sd(t340_init2_stage4$forecast, na.rm = TRUE)

  t340_init3_stage3 <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t340_init3_stage3_sd <- sd(t340_init3_stage3$forecast, na.rm = TRUE)

  t340_init3_stage4 <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t340_init3_stage4_sd <- sd(t340_init3_stage4$forecast, na.rm = TRUE)

  t340_init1_stage1_super <- phase1 %>%
    filter(teamId == 340) %>%
    filter(userName %in% supers)
  t340_init1_stage1_sd_super <- sd(t340_init1_stage1_super$forecast, na.rm = TRUE)

  t340_init1_stage2_super <- phase2 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t340_init1_stage2_sd_super <- sd(t340_init1_stage2_super$forecast, na.rm = TRUE)

  t340_init1_stage3_super <- phase3 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t340_init1_stage3_sd_super <- sd(t340_init1_stage3_super$forecast, na.rm = TRUE)

  t340_init1_stage4_super <- phase4 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t340_init1_stage4_sd_super <- sd(t340_init1_stage4_super$forecast, na.rm = TRUE)

  t340_init2_stage2_super <- phase2 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t340_init2_stage2_sd_super <- sd(t340_init2_stage2_super$forecast, na.rm = TRUE)

  t340_init2_stage3_super <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t340_init2_stage3_sd_super <- sd(t340_init2_stage3_super$forecast, na.rm = TRUE)

  t340_init2_stage4_super <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t340_init2_stage4_sd_super <- sd(t340_init2_stage4_super$forecast, na.rm = TRUE)

  t340_init3_stage3_super <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t340_init3_stage3_sd_super <- sd(t340_init3_stage3_super$forecast, na.rm = TRUE)

  t340_init3_stage4_super <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t340_init3_stage4_sd_super <- sd(t340_init3_stage4_super$forecast, na.rm = TRUE)

  t340_init1_stage1_expert <- phase1 %>%
    filter(teamId == 340) %>%
    filter(userName %in% expertsG1$userName)
  t340_init1_stage1_sd_expert <- sd(t340_init1_stage1_expert$forecast, na.rm = TRUE)

  t340_init1_stage2_expert <- phase2 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init1_stage2_sd_expert <- sd(t340_init1_stage2_expert$forecast, na.rm = TRUE)

  t340_init1_stage3_expert <- phase3 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init1_stage3_sd_expert <- sd(t340_init1_stage3_expert$forecast, na.rm = TRUE)

  t340_init1_stage4_expert <- phase4 %>%
    filter(teamId == 340) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init1_stage4_sd_expert <- sd(t340_init1_stage4_expert$forecast, na.rm = TRUE)

  t340_init2_stage2_expert <- phase2 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t340_init2_stage2_sd_expert <- sd(t340_init2_stage2_expert$forecast, na.rm = TRUE)

  t340_init2_stage3_expert <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init2_stage3_sd_expert <- sd(t340_init2_stage3_expert$forecast, na.rm = TRUE)

  t340_init2_stage4_expert <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init2_stage4_sd_expert <- sd(t340_init2_stage4_expert$forecast, na.rm = TRUE)

  t340_init3_stage3_expert <- phase3 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t340_init3_stage3_sd_expert <- sd(t340_init3_stage3_expert$forecast, na.rm = TRUE)

  t340_init3_stage4_expert <- phase4 %>%
    filter(teamId == 340) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t340_init3_stage4_sd_expert <- sd(t340_init3_stage4_expert$forecast, na.rm = TRUE)

  t341_init1_stage1 <- phase1 %>% filter(teamId == 341)
  t341_init1_stage1_sd <- sd(t341_init1_stage1$forecast)

  t341_init1_stage2 <- phase2 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId)
  t341_init1_stage2_sd <- sd(t341_init1_stage2$forecast, na.rm = TRUE)

  t341_init1_stage3 <- phase3 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId)
  t341_init1_stage3_sd <- sd(t341_init1_stage3$forecast, na.rm = TRUE)

  t341_init1_stage4 <- phase4 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId)
  t341_init1_stage4_sd <- sd(t341_init1_stage4$forecast, na.rm = TRUE)

  t341_init2_stage2 <- phase2 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId))
  t341_init2_stage2_sd <- sd(t341_init2_stage2$forecast, na.rm = TRUE)

  t341_init2_stage3 <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t341_init2_stage3_sd <- sd(t341_init2_stage3$forecast, na.rm = TRUE)

  t341_init2_stage4 <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t341_init2_stage4_sd <- sd(t341_init2_stage4$forecast, na.rm = TRUE)

  t341_init3_stage3 <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t341_init3_stage3_sd <- sd(t341_init3_stage3$forecast, na.rm = TRUE)

  t341_init3_stage4 <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t341_init3_stage4_sd <- sd(t341_init3_stage4$forecast, na.rm = TRUE)

  t341_init1_stage1_super <- phase1 %>%
    filter(teamId == 341) %>%
    filter(userName %in% supers)
  t341_init1_stage1_sd_super <- sd(t341_init1_stage1_super$forecast, na.rm = TRUE)

  t341_init1_stage2_super <- phase2 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t341_init1_stage2_sd_super <- sd(t341_init1_stage2_super$forecast, na.rm = TRUE)

  t341_init1_stage3_super <- phase3 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t341_init1_stage3_sd_super <- sd(t341_init1_stage3_super$forecast, na.rm = TRUE)

  t341_init1_stage4_super <- phase4 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% supers)
  t341_init1_stage4_sd_super <- sd(t341_init1_stage4_super$forecast, na.rm = TRUE)

  t341_init2_stage2_super <- phase2 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% supers)
  t341_init2_stage2_sd_super <- sd(t341_init2_stage2_super$forecast, na.rm = TRUE)

  t341_init2_stage3_super <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t341_init2_stage3_sd_super <- sd(t341_init2_stage3_super$forecast, na.rm = TRUE)

  t341_init2_stage4_super <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% supers)
  t341_init2_stage4_sd_super <- sd(t341_init2_stage4_super$forecast, na.rm = TRUE)

  t341_init3_stage3_super <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% supers)
  t341_init3_stage3_sd_super <- sd(t341_init3_stage3_super$forecast, na.rm = TRUE)

  t341_init3_stage4_super <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% supers)
  t341_init3_stage4_sd_super <- sd(t341_init3_stage4_super$forecast, na.rm = TRUE)

  t341_init1_stage1_expert <- phase1 %>%
    filter(teamId == 341) %>%
    filter(userName %in% expertsG1$userName)
  t341_init1_stage1_sd_expert <- sd(t341_init1_stage1_expert$forecast, na.rm = TRUE)

  t341_init1_stage2_expert <- phase2 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init1_stage2_sd_expert <- sd(t341_init1_stage2_expert$forecast, na.rm = TRUE)

  t341_init1_stage3_expert <- phase3 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init1_stage3_sd_expert <- sd(t341_init1_stage3_expert$forecast, na.rm = TRUE)

  t341_init1_stage4_expert <- phase4 %>%
    filter(teamId == 341) %>%
    filter(userId %in% phase1$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init1_stage4_sd_expert <- sd(t341_init1_stage4_expert$forecast, na.rm = TRUE)

  t341_init2_stage2_expert <- phase2 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t341_init2_stage2_sd_expert <- sd(t341_init2_stage2_expert$forecast, na.rm = TRUE)

  t341_init2_stage3_expert <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init2_stage3_sd_expert <- sd(t341_init2_stage3_expert$forecast, na.rm = TRUE)

  t341_init2_stage4_expert <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init2_stage4_sd_expert <- sd(t341_init2_stage4_expert$forecast, na.rm = TRUE)

  t341_init3_stage3_expert <- phase3 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userName %in% expertsG1$userName)
  t341_init3_stage3_sd_expert <- sd(t341_init3_stage3_expert$forecast, na.rm = TRUE)

  t341_init3_stage4_expert <- phase4 %>%
    filter(teamId == 341) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId) %>%
    filter(userName %in% expertsG1$userName)
  t341_init3_stage4_sd_expert <- sd(t341_init3_stage4_expert$forecast, na.rm = TRUE)

  #####

  t342_init1_stage1 <- phase1 %>% filter(teamId == 342)
  t342_init1_stage1_sd <- sd(t342_init1_stage1$forecast, na.rm = TRUE)

  t342_init1_stage2 <- phase2 %>%
    filter(teamId == 342) %>%
    filter(userId %in% phase1$userId)
  t342_init1_stage2_sd <- sd(t342_init1_stage2$forecast, na.rm = TRUE)

  t342_init1_stage3 <- phase3 %>%
    filter(teamId == 342) %>%
    filter(userId %in% phase1$userId)
  t342_init1_stage3_sd <- sd(t342_init1_stage3$forecast, na.rm = TRUE)

  t342_init1_stage4 <- phase4 %>%
    filter(teamId == 342) %>%
    filter(userId %in% phase1$userId)
  t342_init1_stage4_sd <- sd(t342_init1_stage4$forecast, na.rm = TRUE)

  t342_init2_stage2 <- phase2 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId))
  t342_init2_stage2_sd <- sd(t342_init2_stage2$forecast, na.rm = TRUE)

  t342_init2_stage3 <- phase3 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t342_init2_stage3_sd <- sd(t342_init2_stage3$forecast, na.rm = TRUE)

  t342_init2_stage4 <- phase4 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t342_init2_stage4_sd <- sd(t342_init2_stage4$forecast, na.rm = TRUE)

  t342_init3_stage3 <- phase3 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t342_init3_stage3_sd <- sd(t342_init3_stage3$forecast, na.rm = TRUE)

  t342_init3_stage4 <- phase4 %>%
    filter(teamId == 342) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t342_init3_stage4_sd <- sd(t342_init3_stage4$forecast, na.rm = TRUE)

  t343_init1_stage1 <- phase1 %>% filter(teamId == 343)
  t343_init1_stage1_sd <- sd(t343_init1_stage1$forecast, na.rm = TRUE)

  t343_init1_stage2 <- phase2 %>%
    filter(teamId == 343) %>%
    filter(userId %in% phase1$userId)
  t343_init1_stage2_sd <- sd(t343_init1_stage2$forecast, na.rm = TRUE)

  t343_init1_stage3 <- phase3 %>%
    filter(teamId == 343) %>%
    filter(userId %in% phase1$userId)
  t343_init1_stage3_sd <- sd(t343_init1_stage3$forecast, na.rm = TRUE)

  t343_init1_stage4 <- phase4 %>%
    filter(teamId == 343) %>%
    filter(userId %in% phase1$userId)
  t343_init1_stage4_sd <- sd(t343_init1_stage4$forecast, na.rm = TRUE)

  t343_init2_stage2 <- phase2 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId))
  t343_init2_stage2_sd <- sd(t343_init2_stage2$forecast, na.rm = TRUE)

  t343_init2_stage3 <- phase3 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t343_init2_stage3_sd <- sd(t343_init2_stage3$forecast, na.rm = TRUE)

  t343_init2_stage4 <- phase4 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t343_init2_stage4_sd <- sd(t343_init2_stage4$forecast, na.rm = TRUE)

  t343_init3_stage3 <- phase3 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t343_init3_stage3_sd <- sd(t343_init3_stage3$forecast, na.rm = TRUE)

  t343_init3_stage4 <- phase4 %>%
    filter(teamId == 343) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t343_init3_stage4_sd <- sd(t343_init3_stage4$forecast, na.rm = TRUE)

  t344_init1_stage1 <- phase1 %>% filter(teamId == 344)
  t344_init1_stage1_sd <- sd(t344_init1_stage1$forecast, na.rm = TRUE)

  t344_init1_stage2 <- phase2 %>%
    filter(teamId == 344) %>%
    filter(userId %in% phase1$userId)
  t344_init1_stage2_sd <- sd(t344_init1_stage2$forecast, na.rm = TRUE)

  t344_init1_stage3 <- phase3 %>%
    filter(teamId == 344) %>%
    filter(userId %in% phase1$userId)
  t344_init1_stage3_sd <- sd(t344_init1_stage3$forecast, na.rm = TRUE)

  t344_init1_stage4 <- phase4 %>%
    filter(teamId == 344) %>%
    filter(userId %in% phase1$userId)
  t344_init1_stage4_sd <- sd(t344_init1_stage4$forecast, na.rm = TRUE)

  t344_init2_stage2 <- phase2 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId))
  t344_init2_stage2_sd <- sd(t344_init2_stage2$forecast, na.rm = TRUE)

  t344_init2_stage3 <- phase3 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t344_init2_stage3_sd <- sd(t344_init2_stage3$forecast, na.rm = TRUE)

  t344_init2_stage4 <- phase4 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t344_init2_stage4_sd <- sd(t344_init2_stage4$forecast, na.rm = TRUE)

  t344_init3_stage3 <- phase3 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t344_init3_stage3_sd <- sd(t344_init3_stage3$forecast, na.rm = TRUE)

  t344_init3_stage4 <- phase4 %>%
    filter(teamId == 344) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t344_init3_stage4_sd <- sd(t344_init3_stage4$forecast, na.rm = TRUE)

  t345_init1_stage1 <- phase1 %>% filter(teamId == 345)
  t345_init1_stage1_sd <- sd(t345_init1_stage1$forecast, na.rm = TRUE)

  t345_init1_stage2 <- phase2 %>%
    filter(teamId == 345) %>%
    filter(userId %in% phase1$userId)
  t345_init1_stage2_sd <- sd(t345_init1_stage2$forecast, na.rm = TRUE)

  t345_init1_stage3 <- phase3 %>%
    filter(teamId == 345) %>%
    filter(userId %in% phase1$userId)
  t345_init1_stage3_sd <- sd(t345_init1_stage3$forecast, na.rm = TRUE)

  t345_init1_stage4 <- phase4 %>%
    filter(teamId == 345) %>%
    filter(userId %in% phase1$userId)
  t345_init1_stage4_sd <- sd(t345_init1_stage4$forecast, na.rm = TRUE)

  t345_init2_stage2 <- phase2 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId))
  t345_init2_stage2_sd <- sd(t345_init2_stage2$forecast, na.rm = TRUE)

  t345_init2_stage3 <- phase3 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t345_init2_stage3_sd <- sd(t345_init2_stage3$forecast, na.rm = TRUE)

  t345_init2_stage4 <- phase4 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(userId %in% phase2$userId)
  t345_init2_stage4_sd <- sd(t345_init2_stage4$forecast, na.rm = TRUE)

  t345_init3_stage3 <- phase3 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId))
  t345_init3_stage3_sd <- sd(t345_init3_stage3$forecast, na.rm = TRUE)

  t345_init3_stage4 <- phase4 %>%
    filter(teamId == 345) %>%
    filter(!(userId %in% phase1$userId)) %>%
    filter(!(userId %in% phase2$userId)) %>%
    filter(userId %in% phase3$userId)
  t345_init3_stage4_sd <- sd(t345_init3_stage4$forecast, na.rm = TRUE)

  return(data.frame(
    setName = phase1$setName[1],
    questionName = phase1$questionName[1],
    answerText = phase1$answerText[1],
    t336_init1_stage1_sd,
    t336_init1_stage2_sd,
    t336_init1_stage3_sd,
    t336_init1_stage4_sd,
    t336_init2_stage2_sd,
    t336_init2_stage3_sd,
    t336_init2_stage4_sd,
    t336_init3_stage3_sd,
    t336_init3_stage4_sd,
    t336_init1_stage1_sd_super,
    t336_init1_stage2_sd_super,
    t336_init1_stage3_sd_super,
    t336_init1_stage4_sd_super,
    t336_init2_stage2_sd_super,
    t336_init2_stage3_sd_super,
    t336_init2_stage4_sd_super,
    t336_init3_stage3_sd_super,
    t336_init3_stage4_sd_super,
    t336_init1_stage1_sd_expert,
    t336_init1_stage2_sd_expert,
    t336_init1_stage3_sd_expert,
    t336_init1_stage4_sd_expert,
    t336_init2_stage2_sd_expert,
    t336_init2_stage3_sd_expert,
    t336_init2_stage4_sd_expert,
    t336_init3_stage3_sd_expert,
    t336_init3_stage4_sd_expert,
    t337_init1_stage1_sd,
    t337_init1_stage2_sd,
    t337_init1_stage3_sd,
    t337_init1_stage4_sd,
    t337_init2_stage2_sd,
    t337_init2_stage3_sd,
    t337_init2_stage4_sd,
    t337_init3_stage3_sd,
    t337_init3_stage4_sd,
    t337_init1_stage1_sd_super,
    t337_init1_stage2_sd_super,
    t337_init1_stage3_sd_super,
    t337_init1_stage4_sd_super,
    t337_init2_stage2_sd_super,
    t337_init2_stage3_sd_super,
    t337_init2_stage4_sd_super,
    t337_init3_stage3_sd_super,
    t337_init3_stage4_sd_super,
    t337_init1_stage1_sd_expert,
    t337_init1_stage2_sd_expert,
    t337_init1_stage3_sd_expert,
    t337_init1_stage4_sd_expert,
    t337_init2_stage2_sd_expert,
    t337_init2_stage3_sd_expert,
    t337_init2_stage4_sd_expert,
    t337_init3_stage3_sd_expert,
    t337_init3_stage4_sd_expert,
    t338_init1_stage1_sd,
    t338_init1_stage2_sd,
    t338_init1_stage3_sd,
    t338_init1_stage4_sd,
    t338_init2_stage2_sd,
    t338_init2_stage3_sd,
    t338_init2_stage4_sd,
    t338_init3_stage3_sd,
    t338_init3_stage4_sd,
    t338_init1_stage1_sd_super,
    t338_init1_stage2_sd_super,
    t338_init1_stage3_sd_super,
    t338_init1_stage4_sd_super,
    t338_init2_stage2_sd_super,
    t338_init2_stage3_sd_super,
    t338_init2_stage4_sd_super,
    t338_init3_stage3_sd_super,
    t338_init3_stage4_sd_super,
    t338_init1_stage1_sd_expert,
    t338_init1_stage2_sd_expert,
    t338_init1_stage3_sd_expert,
    t338_init1_stage4_sd_expert,
    t338_init2_stage2_sd_expert,
    t338_init2_stage3_sd_expert,
    t338_init2_stage4_sd_expert,
    t338_init3_stage3_sd_expert,
    t338_init3_stage4_sd_expert,
    t339_init1_stage1_sd,
    t339_init1_stage2_sd,
    t339_init1_stage3_sd,
    t339_init1_stage4_sd,
    t339_init2_stage2_sd,
    t339_init2_stage3_sd,
    t339_init2_stage4_sd,
    t339_init3_stage3_sd,
    t339_init3_stage4_sd,
    t339_init1_stage1_sd_super,
    t339_init1_stage2_sd_super,
    t339_init1_stage3_sd_super,
    t339_init1_stage4_sd_super,
    t339_init2_stage2_sd_super,
    t339_init2_stage3_sd_super,
    t339_init2_stage4_sd_super,
    t339_init3_stage3_sd_super,
    t339_init3_stage4_sd_super,
    t339_init1_stage1_sd_expert,
    t339_init1_stage2_sd_expert,
    t339_init1_stage3_sd_expert,
    t339_init1_stage4_sd_expert,
    t339_init2_stage2_sd_expert,
    t339_init2_stage3_sd_expert,
    t339_init2_stage4_sd_expert,
    t339_init3_stage3_sd_expert,
    t339_init3_stage4_sd_expert,
    t340_init1_stage1_sd,
    t340_init1_stage2_sd,
    t340_init1_stage3_sd,
    t340_init1_stage4_sd,
    t340_init2_stage2_sd,
    t340_init2_stage3_sd,
    t340_init2_stage4_sd,
    t340_init3_stage3_sd,
    t340_init3_stage4_sd,
    t340_init1_stage1_sd_super,
    t340_init1_stage2_sd_super,
    t340_init1_stage3_sd_super,
    t340_init1_stage4_sd_super,
    t340_init2_stage2_sd_super,
    t340_init2_stage3_sd_super,
    t340_init2_stage4_sd_super,
    t340_init3_stage3_sd_super,
    t340_init3_stage4_sd_super,
    t340_init1_stage1_sd_expert,
    t340_init1_stage2_sd_expert,
    t340_init1_stage3_sd_expert,
    t340_init1_stage4_sd_expert,
    t340_init2_stage2_sd_expert,
    t340_init2_stage3_sd_expert,
    t340_init2_stage4_sd_expert,
    t340_init3_stage3_sd_expert,
    t340_init3_stage4_sd_expert,
    t341_init1_stage1_sd,
    t341_init1_stage2_sd,
    t341_init1_stage3_sd,
    t341_init1_stage4_sd,
    t341_init2_stage2_sd,
    t341_init2_stage3_sd,
    t341_init2_stage4_sd,
    t341_init3_stage3_sd,
    t341_init3_stage4_sd,
    t341_init1_stage1_sd_super,
    t341_init1_stage2_sd_super,
    t341_init1_stage3_sd_super,
    t341_init1_stage4_sd_super,
    t341_init2_stage2_sd_super,
    t341_init2_stage3_sd_super,
    t341_init2_stage4_sd_super,
    t341_init3_stage3_sd_super,
    t341_init3_stage4_sd_super,
    t341_init1_stage1_sd_expert,
    t341_init1_stage2_sd_expert,
    t341_init1_stage3_sd_expert,
    t341_init1_stage4_sd_expert,
    t341_init2_stage2_sd_expert,
    t341_init2_stage3_sd_expert,
    t341_init2_stage4_sd_expert,
    t341_init3_stage3_sd_expert,
    t341_init3_stage4_sd_expert,
    t342_init1_stage1_sd,
    t342_init1_stage2_sd,
    t342_init1_stage3_sd,
    t342_init1_stage4_sd,
    t342_init2_stage2_sd,
    t342_init2_stage3_sd,
    t342_init2_stage4_sd,
    t342_init3_stage3_sd,
    t342_init3_stage4_sd,
    t343_init1_stage1_sd,
    t343_init1_stage2_sd,
    t343_init1_stage3_sd,
    t343_init1_stage4_sd,
    t343_init2_stage2_sd,
    t343_init2_stage3_sd,
    t343_init2_stage4_sd,
    t343_init3_stage3_sd,
    t343_init3_stage4_sd,
    t344_init1_stage1_sd,
    t344_init1_stage2_sd,
    t344_init1_stage3_sd,
    t344_init1_stage4_sd,
    t344_init2_stage2_sd,
    t344_init2_stage3_sd,
    t344_init2_stage4_sd,
    t344_init3_stage3_sd,
    t344_init3_stage4_sd,
    t345_init1_stage1_sd,
    t345_init1_stage2_sd,
    t345_init1_stage3_sd,
    t345_init1_stage4_sd,
    t345_init2_stage2_sd,
    t345_init2_stage3_sd,
    t345_init2_stage4_sd,
    t345_init3_stage3_sd,
    t345_init3_stage4_sd
  ))
}

multiYearReciprocal_convergence <- function(metaTable, data) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(metaTable$setName[i])
    currentSetName <- metaTable$setName[i]

    years <- metaTable[i, ] %>% select(year1, year2, year3)
    years <- as.character(years)
    years <- years[years != ""]

    beliefSets <- metaTable[i, ] %>% select(yourBeliefs, expertBeliefs, superBeliefs)
    beliefSets <- as.character(beliefSets)
    beliefSets <- beliefSets[beliefSets != ""]

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

        if (all(years == c(2030, 2050, 2100))) {
          setwd(paste0(yourHome, "Summary Data"))
          sample_size <- readLines("sample_size.txt")
          active_s4_forecasters <- phase4 %>% filter(timestamp > ymd("2022 10 02"))
          add <- active_s4_forecasters$userName
          add <- add[!(add %in% sample_size)]
          sample_size <- c(sample_size, add)
          write(sample_size, "sample_size.txt")
        }

        setwd(paste0(yourHome, "Summary Data"))
        convergenceTable <- read.csv("convergenceTable.csv")
        convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
        convergenceTable <- rbind(convergenceTable, convergenceRow)

        write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }
  }
}

pointDistrib_convergence <- function(metaTable, data) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")

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
      convergenceTable <- read.csv("convergenceTable.csv")
      convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
      convergenceTable <- rbind(convergenceTable, convergenceRow)

      write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }
  }
}

multiYearDistrib_convergence <- function(metaTable, data) {
  for (i in 1:length(unique(metaTable$setName))) {
    distrib <- c("5th %", "25th %", "50th %", "75th %", "95th %")
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

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

        if (all(years == c(2024, 2030, 2050))) {
          setwd(paste0(yourHome, "Summary Data"))
          sample_size <- readLines("sample_size.txt")
          active_s4_forecasters <- phase4 %>% filter(timestamp > ymd("2022 10 02"))
          add <- active_s4_forecasters$userName
          add <- add[!(add %in% sample_size)]
          sample_size <- c(sample_size, add)
          write(sample_size, "sample_size.txt")
        }

        setwd(paste0(yourHome, "Summary Data"))
        convergenceTable <- read.csv("convergenceTable.csv")
        convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
        convergenceTable <- rbind(convergenceTable, convergenceRow)

        write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }
  }
}

multiYearBinary_convergence <- function(metaTable, data) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

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

      if (all(years == c(2024, 2030, 2050))) {
        setwd(paste0(yourHome, "Summary Data"))
        sample_size <- readLines("sample_size.txt")
        active_s4_forecasters <- phase4 %>% filter(timestamp > ymd("2022 10 02"))
        add <- active_s4_forecasters$userName
        add <- add[!(add %in% sample_size)]
        sample_size <- c(sample_size, add)
        write(sample_size, "sample_size.txt")
      }

      setwd(paste0(yourHome, "Summary Data"))
      convergenceTable <- read.csv("convergenceTable.csv")
      convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
      convergenceTable <- rbind(convergenceTable, convergenceRow)

      write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }
  }
}

multiYearCountryDistrib_convergence <- function(metaTable, data) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    years <- c(metaTable$year1[i], metaTable$year2[i], metaTable$year3[i])
    years <- years[years != ""]

    countries <- c(metaTable$country1[i], metaTable$country2[i], metaTable$country3[i], metaTable$country4[i], metaTable$country5[i], metaTable$country6[i], metaTable$country7[i], metaTable$country8[i], metaTable$country9[i], metaTable$country10[i], metaTable$country11[i], metaTable$country12[i])
    countries <- countries[countries != ""]

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

        if (all(years == c(2024, 2030, 2050))) {
          setwd(paste0(yourHome, "Summary Data"))
          sample_size <- readLines("sample_size.txt")
          active_s4_forecasters <- phase4 %>% filter(timestamp > ymd("2022 10 02"))
          add <- active_s4_forecasters$userName
          add <- add[!(add %in% sample_size)]
          sample_size <- c(sample_size, add)
          write(sample_size, "sample_size.txt")
        }

        setwd(paste0(yourHome, "Summary Data"))
        convergenceTable <- read.csv("convergenceTable.csv")
        convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
        convergenceTable <- rbind(convergenceTable, convergenceRow)

        write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

        setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data/", years[j]))
      }

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }
  }
}

multiCountryBinary_convergence <- function(metaTable, data) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

    countries <- c(metaTable$country1[i], metaTable$country2[i], metaTable$country3[i], metaTable$country4[i], metaTable$country5[i], metaTable$country6[i], metaTable$country7[i], metaTable$country8[i], metaTable$country9[i], metaTable$country10[i], metaTable$country11[i], metaTable$country12[i])
    countries <- countries[countries != ""]

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
      convergenceTable <- read.csv("convergenceTable.csv")
      convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
      convergenceTable <- rbind(convergenceTable, convergenceRow)

      write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)

      setwd(paste0(yourHome, "Summary Data/", currentSetName, "/Phase Data"))
    }
  }
}

pointBinary_convergence <- function(metaTable, data) {
  for (i in 1:length(unique(metaTable$setName))) {
    print(unique(metaTable$setName)[i])
    currentSetName <- unique(metaTable$setName)[i]

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
    convergenceTable <- read.csv("convergenceTable.csv")
    convergenceRow <- convergenceAdd_active(phase1, phase2, phase3, phase4, convergenceTable)
    convergenceTable <- rbind(convergenceTable, convergenceRow)

    write.csv(convergenceTable, "convergenceTable.csv", row.names = FALSE)
  }
}
