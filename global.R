# Loading the Libraries

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinybusy)
library(glmnet)
library(dplyr)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(caret)
library(visdat)
library(naniar)
library(ggplot2)
library(plotly)
library(rpart)
library(rpart.plot)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(reshape2)
library(pls)
library(visNetwork)
library(leaflet)
library(DT)
library(car)
library(shinycssloaders)
library(summarytools)
library(gridBase)
library(grid)
library(recipes)
library(rsconnect)
lexicon <- "afinn"

# Loading the Data Set
Data <- read.csv("Covid19.csv", header = TRUE, na.strings = c("", "NA", "--"))

# Data Cleaning Steps
# 1. Converting all the Missing value as "NA"
Data[Data == -99] <- NA

# 2. Addressing Incorrect Values
Data$HEALTHCARE_COST[!is.na(Data$HEALTHCARE_BASIS) & Data$HEALTHCARE_BASIS == "FREE"] <- 0

Data$GOVERNMENT <- as.character(Data$GOVERNMENT)
Data$GOVERNMENT[is.na(Data$GOVERNMENT)] <- "NONE"
Data$GOVERNMENT <- as.factor(Data$GOVERNMENT)

Data$HEALTHCARE_BASIS <- as.character(Data$HEALTHCARE_BASIS)
Data$HEALTHCARE_BASIS[is.na(Data$HEALTHCARE_BASIS)] <- "NONE"
Data$HEALTHCARE_BASIS <- as.factor(Data$HEALTHCARE_BASIS)

# Next -> Exploratory Data Analysis Showing Visualizations
# Categorical Data for Mosaic Plot
choicesA <- colnames(Data[, c(2, 12)])

# Data Pre Processing Steps
CovData <- Data

# Partitioning the dataset based on Decision Tree Output
train <- Data %>% filter(POPULATION >= 17)
test <- Data %>% filter(POPULATION < 17)

# Using Recipe based pipeline to impute values
rec <- recipes::recipe(DEATHRATE ~., data = train) %>%
  update_role("COUNTRY", new_role = "id") %>%
  step_knnimpute(recipes::all_predictors(), neighbours = 5) %>%
# Centering and Scaling all numeric predictors except the outcome variable
  step_center(all_numeric(), -has_role("outcome")) %>%   
  step_scale(all_numeric(), -has_role("outcome")) %>%
  step_dummy(all_nominal())

