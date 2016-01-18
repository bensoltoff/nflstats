## random_forest.R
## Create a random forest model for predicting NFL win probability

require(rnflstats)
require(magrittr)
require(dplyr)
require(multidplyr)
require(readr)
require(ggplot2)
require(tidyr)
require(caret)
require(randomForest)

rm(list = ls())

set.seed(11091987)

df <- read_csv("../data/pbp_cleaned.csv")
df_plays <- df %>%
  filter(type != "CONV")

# Custom features
df_plays %<>%
  # Replace pts with 0 if missing
  mutate(secs_left_adj = sqrt(secs_left),
         pts = ifelse(is.na(pts), 0, pts),
         half_two = as.numeric(qtr >= 3))

# Features to use in the model
features <- c("dwn", "ytg", "yfog", "secs_left", "secs_left_adj",
              "score_diff", "timo", "timd", "spread",
              "kneel_down", "qtr")
target <- c("win", "pts")

# Split data into training/test sets
df_plays %<>%
  select(one_of(c(features, target))) %>%
  # remove plays with missing data
  na.omit


# train win probability model using random forest
system.time({
  require(doMC)
  registerDoMC(4)
  ctrl <- trainControl(method = "oob")
  
  win_prob_rf <- train(make.names(win) ~ dwn + ytg + yfog +
                         secs_left + secs_left_adj + qtr +
                         score_diff + spread +
                         timo + timd +
                         kneel_down,
                       method = "rf",
                       data = df_plays,
                       ntree = 500,
                       nodesize = 1,
                       trControl = ctrl)
  print(win_prob_rf)
  print(win_prob_rf$finalModel)
})

saveRDS(win_prob_rf, file = "models/win_prob_rf.RDs")
