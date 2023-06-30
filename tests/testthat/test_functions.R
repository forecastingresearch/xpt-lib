library(dplyr)
library(testthat)
library(data.table)
library(xpt)

meta <- fread("questionMetadata.csv")
questionTypes <- unique(meta$questionType)
metaTable <- meta %>% filter(questionType == questionTypes[1])

# Make a dataframe with userNames and forecasts (numeric)
questionDataProcessed <- data.frame(userName = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
                                    forecast = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0))

empty_df <- newAddInit()

new_row <- newRowInit(metaTable, questionDataProcessed, "1. Genetically Engineered Pathogen Risk",
                      "2100 (Your Beliefs)", "", 4, "Biorisk")

expect_equal(length(colnames(new_row)), length(colnames(empty_df)))

expect_equal(colnames(new_row), colnames(empty_df))