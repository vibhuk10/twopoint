create_combined <- function(quarter, timeleft, score, df) {
  base_plays <- df %>% 
            filter(qtr == quarter & quarter_seconds_remaining>(timeleft-50) & quarter_seconds_remaining<(timeleft+50) & score_differential == score) %>% 
            select(game_id, posteam, home_team, away_team, score_differential, qtr, quarter_seconds_remaining, desc, game_date) %>% 
            group_by(game_id) %>% 
            slice(n()-1) %>% 
            ungroup()
  
  last_plays <- df %>% 
            select(game_id, home_team, posteam, defteam, total_home_score, total_away_score, qtr, quarter_seconds_remaining) %>%
            rename(home_team2 = home_team, posteam2 = posteam, defteam2 = defteam, qtr2 = qtr, quarter_seconds_remaining2 = quarter_seconds_remaining) %>% 
            group_by(game_id) %>% 
            slice(n()) %>% 
            ungroup()
  
  last_plays <- last_plays %>% 
                  mutate(score_differential2 = total_home_score-total_away_score)
  
  combined <- left_join(base_plays,last_plays, by="game_id")
  
  combined <- combined %>% 
            mutate(score_differential2 = ifelse(posteam == away_team, -score_differential2, score_differential2))
  combined <- combined %>% 
            mutate(
              comeback = case_when(
                score_differential2 == 0 ~ "tie",
                score_differential2 > 0 ~ "win",
                score_differential2 < 0 ~ "loss",
              )
            )
  combined <- combined %>% 
    mutate(overtime = ifelse(qtr2 == 5, TRUE, FALSE))
}


create_prediction <- function(df) {
  prediction <- df %>% count(comeback)
  prediction <- prediction %>% 
    mutate(prob = n/(sum(n)))
  prediction <- prediction %>% 
    mutate(prob = round(prob, digits = 4))
  return(prediction)
}
combined <- create_combined(2,200,-7, data)
prediction <- create_prediction(combined)

combined %>%
  count(comeback, overtime) %>% 
  mutate(comeback = fct_reorder(comeback, n, sum)) %>% 
  ggplot() +
  geom_col(aes(x=comeback, y = n, fill = overtime)) +
  scale_fill_manual(values = c("#565656", "blue")) +
  labs(x = "end result")

prediction_graph <- prediction %>%
  ggplot() +
  geom_col(aes(x=comeback, y = prob)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "end result")
prediction_graph
