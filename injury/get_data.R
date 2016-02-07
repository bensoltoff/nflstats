library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggvis)
library(knitr)
require(gdata)
require(jsonlite)
options(digits = 4)

# Let phantomJS scrape injury report, output is written to injury.html
url <- "http://www.nfl.com/injuries?week=1"

test <- readLines("http://www.nfl.com/injuries?week=1")

test %>%
  data_frame(raw = test) %>%
  mutate(raw = str_trim(raw)) %>%
  filter(grepl("player", raw),
         grepl("position", raw),
         grepl("injury", raw),
         grepl("practiceStatus", raw),
         grepl("gameStatus", raw),
         grepl("lastName", raw),
         grepl("firstName", raw),
         grepl("player", raw),
         grepl("esbId", raw)) %>%
  unlist %>%
  as.character %>%
  paste(., collapse = "") %>%
  substr(., 1, nchar(.) - 1) %>%
  paste("[", ., "]") %>%
  gsub("player", '"player"', .) %>%
  gsub("position", '"position"', .) %>%
  gsub("injury", '"injury"', .) %>%
  gsub("practiceStatus", '"practiceStatus"', .) %>%
  gsub("gameStatus", '"gameStatus"', .) %>%
  gsub("lastName", '"lastName"', .) %>%
  gsub("firstName", '"firstName"', .) %>%
  gsub("esbId", '"esbId"', .) %>%
  fromJSON %>%
  tbl_df %>%
  unique








