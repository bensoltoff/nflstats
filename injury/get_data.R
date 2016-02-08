require(stringr)
require(dplyr)
require(ggplot2)
require(jsonlite)

rm(list = ls())

# get injury reports
get_injury_report <- function(week = 1){
  url <- paste0("http://www.nfl.com/injuries?week=", week)
  page <- readLines(url)
  
  page %<>%
    data_frame(raw = page) %>%
    mutate(raw = str_trim(raw)) %>%
    filter(grepl("player", raw),
           grepl("position", raw),
           grepl("injury", raw),
           grepl("practiceStatus", raw),
           grepl("gameStatus", raw),
           grepl("lastName", raw),
           grepl("firstName", raw),
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
    unique %>%
    mutate(week = week)
  
  return(page)
}

# were players active?
get_inactives <- function(week = 1){
  url <- paste0("http://www.nfl.com/inactives?week=", week)
  page <- readLines(url)
  
  page %<>%
    data_frame(raw = page) %>%
    mutate(raw = str_trim(raw)) %>%
    filter(grepl("player", raw),
           grepl("position", raw),
           grepl("status", raw),
           grepl("comments", raw),
           grepl("lastName", raw),
           grepl("firstName", raw),
           grepl("esbId", raw)) %>%
    unlist %>%
    as.character %>%
    paste(., collapse = "") %>%
    substr(., 1, nchar(.) - 1) %>%
    paste("[", ., "]") %>%
    gsub("player", '"player"', .) %>%
    gsub("position:", '"position":', .) %>%
    gsub("status:", '"status":', .) %>%
    gsub("comments", '"comments"', .) %>%
    gsub("lastName", '"lastName"', .) %>%
    gsub("firstName", '"firstName"', .) %>%
    gsub("esbId", '"esbId"', .) %>%
    # remove contractions
    gsub("\'t", "", .) %>%
    gsub("\\\\", "", .) %>%
    fromJSON %>%
    tbl_df %>%
    unique %>%
    mutate(week = week)
  
  return(page)
}

# get injury reports for 2015 season
injury <- lapply(1:17, function(x) get_injury_report(x)) %>%
  bind_rows

# get inactive reports for 2015 regular season
inactives <- lapply(1:17, function(x) get_inactives(x)) %>%
  bind_rows

# combine reports
both <- left_join(select(injury, injury:gameStatus, esbId, week),
                  select(inactives, status, comments, esbId:week)) %>%
  filter(gameStatus != "--") %>%
  mutate(gameStatus = factor(gameStatus, levels = c("Probable", "Questionable", "Doubtful", "Out")),
         game_prob = ifelse(gameStatus == "Out", 0,
                             ifelse(gameStatus == "Doubtful", .25,
                                    ifelse(gameStatus == "Questionable", .5,
                                           ifelse(gameStatus == "Probable", .75, NA)))),
         # status = ifelse(is.na(status), "Active", status),
         play = ifelse(status == "Active", 1,
                       ifelse(status == "Inactive", 0, NA)))


# injury report v. number of players, by inactive report
both %>%
  group_by(gameStatus, status) %>%
  summarize(n = n()) %>%
  mutate(status = ifelse(is.na(status), "Not Mentioned", status)) %>%
  ggplot(aes(gameStatus, n, fill = status)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "NFL Injury Reports (2015 Regular Season)",
       x = "Status on Injury Report",
       y = "Number of Players",
       fill = "Status on\nInactive Report") +
  theme_bw()

# proportion of players active, by injury status and week
both %>%
  filter(!is.na(status)) %>%
  group_by(gameStatus, week) %>%
  summarize(active_n = sum(play),
            n = n(),
            prop = active_n / n) %>%
  ggplot(aes(week, prop, color = gameStatus)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "NFL Injury Reports (2015 Regular Season)",
       x = "Week",
       y = "Percentage of Players Active on Game Day",
       color = "Status on\nInactive Report") +
  theme_bw()

both %>%
  filter(!is.na(status)) %>%
  ggplot(aes(week, play, group = gameStatus, color = gameStatus, fill = gameStatus)) +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "NFL Injury Reports (2015 Regular Season)",
       x = "Week",
       y = "Percentage of Players Active on Game Day",
       color = "Status on\nInactive Report",
       fill = "Status on\nInactive Report") +
  theme_bw()







