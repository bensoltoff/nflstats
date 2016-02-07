require(rnflstats)
require(magrittr)
require(dplyr)
require(multidplyr)
require(readr)
require(ggplot2)
require(tidyr)

# get data
play <- read_csv("../../Google Drive/work/research/Sports/nfl/Armchair Analysis/nfl_00-14/csv/PLAY.csv")
game <- read_csv("../../Google Drive/work/research/Sports/nfl/Armchair Analysis/nfl_00-14/csv/GAME.csv")
penalty <- read_csv("../../Google Drive/work/research/Sports/nfl/Armchair Analysis/nfl_00-14/csv/PENALTY.csv")
pbp <- read_csv("../../Google Drive/work/research/Sports/nfl/Armchair Analysis/nfl_00-14/csv/PBP.csv")

# combine data
data <- left_join(play, game) %>%
  left_join(penalty %>%
              select(-pen)) %>%
  filter(seas > 2001)

# distribution of play lengths
data %>%
  ggplot(aes(len)) +
  geom_histogram(binwidth = 1)

# get pid of plays with len > 40
long <- filter(data, len > 40)$pid

# now find plays which occur after long plays
long_plus <- long + 1

## check plays
data %>%
  filter(pid %in% long | pid %in% long_plus)

# now find pid of plays with delay of game and the play before it
delay <- filter(data, desc == "Delay of Game")$pid
delay_minus <- delay - 1
delay_plus <- delay + 1

## check plays
data %>%
  filter(pid %in% delay | pid %in% delay_minus | pid %in% delay_plus,
         seas > 2005)

data %>%
  filter(pid %in% delay_minus) %>%
  ggplot(aes(len)) +
  geom_histogram(binwidth = 1)









