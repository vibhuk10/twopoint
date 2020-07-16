set1 <- data %>% 
          filter(qtr == 4 & quarter_seconds_remaining>600 & quarter_seconds_remaining<700 & score_differential == -7) %>% 
          select(game_id, posteam, home_team, away_team, score_differential, qtr, quarter_seconds_remaining, desc) %>% 
          group_by(game_id) %>% 
          slice(n()-1) %>% 
          ungroup()

set2 <- data %>% 
          select(game_id, score_differential, home_team, posteam) %>%
          rename(score_differential2 = score_differential, home_team2 = home_team, posteam2 = posteam) %>% 
          group_by(game_id) %>% 
          slice(n()-1) %>% 
          ungroup()

set3 <- merge(set1,set2, by="game_id")

