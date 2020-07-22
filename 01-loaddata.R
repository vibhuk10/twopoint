library(tidyverse)
library(arsenal)

data_09 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2009.csv")
data_10 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2010.csv")
data_11 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2011.csv")
data_12 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2012.csv")
data_13 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2013.csv")
data_14 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2014.csv")
data_15 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2015.csv")
data_16 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2016.csv")
data_17 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv")
data_18 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
data_19 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv")
data_19 <- select(data_19, -(touchback:touchback))
data2 <- rbind(data_09, data_10, data_11, data_12, data_13, data_14, data_15, data_16, data_17, data_18, data_19)

data  <- rbind(data_09_18, data_19)
data %>% write_csv("data-raw/NFL_pbp_2009-2019.csv")

summary(comparedf(data2, data_19))

twopoint <- filter(data, (two_point_conv_result == 'failure' | two_point_conv_result == 'success'))
twopoint <- filter(twopoint, !(play_type == 'no_play'))
twopoint <- select(twopoint, two_point_conv_result, play_id, game_id, home_team, away_team, posteam, defteam, game_date, quarter_seconds_remaining, qtr, time, desc, play_type, yards_gained, score_differential, two_point_conversion_prob, total_home_score, total_away_score)
twopoint <- mutate(twopoint, year = str_sub(game_date,1,4))
twopoint %>% write_csv("data-clean/twopoint_2009-2019.csv")

twopoint_13 <- filter(data_13, !is.na(two_point_conv_result))
twopoint_13 <- select(twopoint_13, two_point_conv_result, play_id, game_id, home_team, away_team, posteam, defteam, game_date, quarter_seconds_remaining, qtr, time, desc, play_type, yards_gained, score_differential)
twopoint_13 <- mutate(twopoint_13, year = str_sub(game_date,1,4))

extrapoint <- filter(data, (extra_point_result == 'blocked' | extra_point_result == 'failed' | extra_point_result == 'good'))
extrapoint <- select(extrapoint, extra_point_result, play_id, game_id, home_team, away_team, posteam, defteam, game_date, quarter_seconds_remaining, qtr, time, desc, score_differential, kick_distance, extra_point_prob, defensive_extra_point_conv)
extrapoint <- mutate(extrapoint, year = str_sub(game_date,1,4))
extrapoint %>% write_csv("data-clean/extrapoint_2009-2019.csv")


twopoint <- read_csv("data-clean/twopoint_2009-2019.csv")
data <- read_csv("data-raw/NFL_pbp_2009-2019.csv")
extrapoint <- read_csv("data-clean/extrapoint_2009-2019.csv")

twopoint2 <- read_csv("data-clean/twopoint_2009-2018.csv")
data_09_18 <- read_csv("data-raw/NFL Play by Play 2009-2018 (v5).csv")


