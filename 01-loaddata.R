library(tidyverse)
data <- read_csv("data-raw/NFL Play by Play 2009-2018 (v5).csv")
twopoint <- filter(data, (two_point_conv_result == 'failure' | two_point_conv_result == 'success'))
twopoint <- filter(twopoint, !(play_type == 'no_play'))
twopoint <- select(twopoint, two_point_conv_result, play_id, game_id, home_team, away_team, posteam, defteam, game_date, quarter_seconds_remaining, qtr, time, desc, play_type, yards_gained)
twopoint <- mutate(twopoint, year = str_sub(game_date,1,4))
twopoint
