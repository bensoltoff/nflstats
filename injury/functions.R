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





