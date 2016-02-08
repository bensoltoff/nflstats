require(stringr)
require(dplyr)
require(ggplot2)
require(tidyr)
require(jsonlite)

rm(list = ls())

# get injury reports
get_injury_report <- function(week = 1, refresh = FALSE){
  # check if local copy exists
  url_local <- paste0("injury/reports/injury", week, ".html")
  url <- paste0("http://www.nfl.com/injuries?week=", week)
  
  if(file.exists(url_local) & refresh == FALSE) {
    page <- readLines(url_local)
  } else {
    page <- readLines(url)
    writeLines(page, con = url_local)
  }
  
  report <- data_frame(raw = page) %>%
    # clean up lines
    mutate(raw = str_trim(raw)) %>%
    # get team info and player statuses
    filter(grepl("<!-- awayAbbr:", raw) |
             grepl("<!-- homeAbbr:", raw) |
             grepl("<!-- awayName:", raw) |
             grepl("<!-- homeName:", raw) |
             grepl("var dataAway", raw) |
             grepl("var dataHome", raw) |
             (grepl("player", raw) &
                grepl("position", raw) &
                grepl("injury", raw) &
                grepl("practiceStatus", raw) &
                grepl("gameStatus", raw) &
                grepl("lastName", raw) &
                grepl("firstName", raw) &
                grepl("esbId", raw))) %>%
    # remove duplicates
    # unique %>%
    mutate(raw = gsub('"', "", raw)) %>%
    # split data into separate columns for each variable
    separate(raw, into = c("player", "position", "injury", "practiceStatus", "gameStatus", "lastName",
                           "firstName", "esbId"), sep = ",") %>%
    # clean up injury info
    mutate(player = gsub("\\{player:", "", player),
           position = gsub("position:", "", position),
           injury = gsub("injury:", "", injury),
           practiceStatus = gsub("practiceStatus:", "", practiceStatus),
           gameStatus = gsub("gameStatus:", "", gameStatus),
           lastName = gsub("lastName:", "", lastName),
           firstName = gsub("firstName:", "", firstName),
           esbId = gsub("esbId:", "", esbId),
           esbId = gsub("\\}", "", esbId)) %>%
    mutate_each(funs(str_trim)) %>%
    # get team name info
    mutate(team = player,
           team = ifelse(grepl("<!-- awayAbbr:", team) |
                           grepl("<!-- homeAbbr:", team) |
                           grepl("<!-- awayName:", team) |
                           grepl("<!-- homeName:", team), player, NA),
           team = gsub("<!--", "", team),
           team = gsub("-->", "", team),
           team = str_trim(team)) %>%
    # split team variables and values,
    # then create new variables
    separate(team, into = c("variable", "value"), sep = ":") %>%
    mutate(awayAbbr = ifelse(variable == "awayAbbr", value, NA),
           homeAbbr = ifelse(variable == "homeAbbr", value, NA),
           awayName = ifelse(variable == "awayName", value, NA),
           homeName = ifelse(variable == "homeName", value, NA)) %>%
    # identify if player is home or away
    mutate(loc = ifelse(grepl("dataAway", player), "Away", NA),
           loc = ifelse(grepl("dataHome", player), "Home", loc)) %>%
    fill(awayAbbr:loc) %>%
    # remove extraneous columns and rows
    select(-variable, -value) %>%
    filter(!(grepl("<!--", player) | grepl("var data", player))) %>%
    # identify player team explicitly
    mutate(teamName = ifelse(loc == "Away", awayName, NA),
           teamName = ifelse(loc == "Home", homeName, teamName),
           teamAbbr = ifelse(loc == "Away", awayAbbr, NA),
           teamAbbr = ifelse(loc == "Home", homeAbbr, teamAbbr)) %>%
    select(-awayAbbr, -homeAbbr, -awayName, -homeName) %>%
    unique %>%
    mutate_each(funs(str_trim)) %>%
    mutate(week = week)
  
  return(report)
}

# were players active?
get_inactives <- function(week = 1, refresh = FALSE){
  # check if local copy exists
  url_local <- paste0("injury/reports/inactives", week, ".html")
  url <- paste0("http://www.nfl.com/inactives?week=", week)
  
  if(file.exists(url_local) & refresh == FALSE) {
    page <- readLines(url_local)
  } else {
    page <- readLines(url)
    writeLines(page, con = url_local)
  }
  
  report <- data_frame(raw = page) %>%
    # clean up lines
    mutate(raw = str_trim(raw)) %>%
    # get team info and player statuses
    filter(grepl("<!-- awayAbbr:", raw) |
             grepl("<!-- homeAbbr:", raw) |
             grepl("<!-- awayName:", raw) |
             grepl("<!-- homeName:", raw) |
             grepl("var dataAway", raw) |
             grepl("var dataHome", raw) |
             (grepl("player", raw) &
                grepl("position", raw) &
                grepl("status", raw) &
                grepl("comments", raw) &
                grepl("lastName", raw) &
                grepl("firstName", raw) &
                grepl("esbId", raw))) %>%
    # remove duplicates
    # unique %>%
    mutate(raw = gsub('"', "", raw)) %>%
    # split data into separate columns for each variable
    separate(raw, into = c("player", "position", "status", "comments", "lastName", "firstName",
                           "esbId"), sep = ",") %>%
    # clean up injury info
    mutate(player = gsub("\\{player:", "", player),
           position = gsub("position:", "", position),
           status = gsub("status:", "", status),
           comments = gsub("comments:", "", comments),
           lastName = gsub("lastName:", "", lastName),
           firstName = gsub("firstName:", "", firstName),
           esbId = gsub("esbId:", "", esbId),
           esbId = gsub("\\}", "", esbId)) %>%
    mutate_each(funs(str_trim)) %>%
    # get team name info
    mutate(team = player,
           team = ifelse(grepl("<!-- awayAbbr:", team) |
                           grepl("<!-- homeAbbr:", team) |
                           grepl("<!-- awayName:", team) |
                           grepl("<!-- homeName:", team), player, NA),
           team = gsub("<!--", "", team),
           team = gsub("-->", "", team),
           team = str_trim(team)) %>%
    # split team variables and values,
    # then create new variables
    separate(team, into = c("variable", "value"), sep = ":") %>%
    mutate(awayAbbr = ifelse(variable == "awayAbbr", value, NA),
           homeAbbr = ifelse(variable == "homeAbbr", value, NA),
           awayName = ifelse(variable == "awayName", value, NA),
           homeName = ifelse(variable == "homeName", value, NA)) %>%
    # identify if player is home or away
    mutate(loc = ifelse(grepl("dataAway", player), "Away", NA),
           loc = ifelse(grepl("dataHome", player), "Home", loc)) %>%
    fill(awayAbbr:loc) %>%
    # remove extraneous columns and rows
    select(-variable, -value) %>%
    filter(!(grepl("<!--", player) | grepl("var data", player))) %>%
    # identify player team explicitly
    mutate(teamName = ifelse(loc == "Away", awayName, NA),
           teamName = ifelse(loc == "Home", homeName, teamName),
           teamAbbr = ifelse(loc == "Away", awayAbbr, NA),
           teamAbbr = ifelse(loc == "Home", homeAbbr, teamAbbr)) %>%
    select(-awayAbbr, -homeAbbr, -awayName, -homeName) %>%
    unique %>%
    mutate_each(funs(str_trim)) %>%
    mutate(week = week)
  
  return(report)
}

# nfl team colors
colors <- fromJSON("https://raw.githubusercontent.com/teamcolors/teamcolors.github.io/master/src/scripts/data/leagues/nfl.json")
colors_mod <- data_frame(name = colors$name,
                         main = lapply(colors$colors$hex, function(x) x[1]) %>%
                           unlist,
                         alt1 = lapply(colors$colors$hex, function(x) x[2]) %>%
                           unlist,
                         alt2 = lapply(colors$colors$hex, function(x) x[3]) %>%
                           unlist,
                         alt3 = lapply(colors$colors$hex, function(x) x[4]) %>%
                           unlist) %>%
  # avoid duplicated colors
  mutate(color = ifelse(!duplicated(main), main, alt1),
         # fix Cleveland Browns
         color = ifelse(name == "Cleveland Browns", alt1, color),
         # Fix Jets/Packers
         color = ifelse(name == "New York Jets", main, color),
         color = ifelse(name == "Green Bay Packers", alt1, color),
         color = paste0("#", color))

nfl_colors <- colors_mod$color
names(nfl_colors) <- word(colors$name, -1)

# get injury reports for 2015 season
injury <- lapply(1:17, function(x) get_injury_report(x)) %>%
  bind_rows

# get inactive reports for 2015 regular season
inactives <- lapply(1:17, function(x) get_inactives(x)) %>%
  bind_rows

# combine reports
both <- left_join(injury,
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

# proportion of players active, by inactive report and team
both %>%
  filter(!is.na(status)) %>%
  group_by(gameStatus, teamAbbr, teamName) %>%
  summarize(active_n = sum(play),
            n = n(),
            prop = active_n / n) %>%
  ggplot(aes(teamAbbr, prop, fill = teamName)) +
  facet_wrap(~ gameStatus, ncol = 2) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = nfl_colors) +
  theme(legend.position = "none")

# number of players on injury report, by team
injury %>%
  group_by(teamName, teamAbbr) %>%
  summarize(n = n()) %>%
  ungroup %>%
  arrange(-n) %>%
  mutate(teamAbbr = factor(teamAbbr, levels = teamAbbr)) %>%
  ggplot(aes(teamAbbr, n, fill = teamName)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = nfl_colors) +
  labs(x = "Team",
       y = "Number of Players on Weekly Injury Reports") +
  theme_bw() +
  theme(legend.position = "none")

# proportion of players active, by inactive report and team
both %>%
  filter(!is.na(status),
         gameStatus == "Probable") %>%
  group_by(gameStatus, teamAbbr, teamName) %>%
  summarize(active_n = sum(play),
            n = n(),
            prop = active_n / n,
            se = (prop * (1 - prop)) / n,
            lower = prop - 1.96 * se,
            upper = prop + 1.96 * se) %>%
  ungroup %>%
  arrange(-prop) %>%
  mutate(teamAbbr = factor(teamAbbr, levels = teamAbbr)) %>%
  ggplot(aes(teamAbbr, prop, ymin = lower, ymax = upper, color = teamName)) +
  geom_pointrange() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = nfl_colors) +
  labs(x = "Team",
       y = "Percentage of Probable Players Active on Game Day") +
  theme_bw() +
  theme(legend.position = "none")




