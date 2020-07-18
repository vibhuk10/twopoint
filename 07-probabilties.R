set1 <- data %>% 
          filter(qtr == 3 & quarter_seconds_remaining>600 & quarter_seconds_remaining<700 & score_differential == -7) %>% 
          select(game_id, posteam, home_team, away_team, score_differential, qtr, quarter_seconds_remaining, desc, game_date) %>% 
          group_by(game_id) %>% 
          slice(n()-1) %>% 
          ungroup()

set2 <- data %>% 
          select(game_id, home_team, posteam, defteam, total_home_score, total_away_score, qtr, quarter_seconds_remaining) %>%
          rename(home_team2 = home_team, posteam2 = posteam, defteam2 = defteam, qtr2 = qtr, quarter_seconds_remaining2 = quarter_seconds_remaining) %>% 
          group_by(game_id) %>% 
          slice(n()) %>% 
          ungroup()
set2 <- set2 %>% 
          mutate(score_differential2 = total_home_score-total_away_score)

set3 <- merge(set1,set2, by="game_id")

set3 <- set3 %>% 
          mutate(score_differential2 = ifelse(posteam == away_team, -score_differential2, score_differential))
set3 <- set3 %>% 
          mutate(
            comeback = case_when(
              score_differential2 == 0 ~ "tie",
              score_differential2 > 0 ~ "win",
              score_differential2 < 0 ~ "loss",
            )
          )
set3 <- set3 %>% 
  mutate(overtime = ifelse(qtr2 == 5, TRUE, FALSE))

prediction <- set3 %>% count(comeback)
prediction <- prediction %>% 
                mutate(prob = n/(sum(n)))
prediction <- prediction %>% 
                mutate(prob = (prob*100))
prediction <- prediction %>% 
                mutate(prob = round(prob, digits = 2))
prediction <- prediction %>% 
                mutate(prob = paste0(prob, "%"))
prediction

set3 %>%
  count(comeback, overtime) %>% 
  mutate(comeback = fct_reorder(comeback, n, sum)) %>% 
  ggplot() +
  geom_col(aes(x=comeback, y = n, fill = overtime)) +
  scale_fill_manual(values = c("#565656", "blue")) +
  labs(x = "end result")

prediction %>%
  count(comeback, prob) %>% 
  mutate(comeback = fct_reorder(comeback, n, sum)) %>% 
  ggplot() +
  geom_col(aes(x=comeback, y = n, fill = prob), position = "fill") +
  scale_fill_manual(values = c("#565656", "blue")) +
  labs(x = "end result")






