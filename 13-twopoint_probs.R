create_prob_two <- function(quarter, timeleft) {
  input_plays_two <- twopoint %>% 
    filter(qtr == quarter & quarter_seconds_remaining>(timeleft-200) & quarter_seconds_remaining<(timeleft+200)) %>% 
    select(game_id, posteam, home_team, away_team, score_differential, qtr, quarter_seconds_remaining, desc, game_date, two_point_conv_result)
  prob_two <- input_plays_two %>% count(two_point_conv_result)
  prob_two <- prob_two %>% 
    mutate(prob = n/(sum(n)))
  prob_two <- prob_two %>% 
    mutate(prob = round(prob, digits = 3))
  prob_two <- prob_two %>% 
    mutate(key = case_when(
      two_point_conv_result == "failure" ~ 2,
      two_point_conv_result == "success" ~ 1,
    ))
}
create_prob_score <- function(quarter, timeleft, score, result) {
    base_plays <- data %>% 
      filter(qtr == quarter & quarter_seconds_remaining>(timeleft-50) & quarter_seconds_remaining<(timeleft+50) & score_differential == score) %>% 
      select(game_id, posteam, home_team, away_team, score_differential, qtr, quarter_seconds_remaining, desc, game_date) %>% 
      group_by(game_id) %>% 
      slice(n()-1) %>% 
      ungroup()
    
    last_plays <- data %>% 
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
          score_differential2 > 0 ~ "win"
        )
      )
    combined <- combined %>% 
      mutate(overtime = ifelse(qtr2 == 5, TRUE, FALSE))
    
    prediction <- combined %>% count(comeback)
    prediction <- prediction %>% 
      mutate(prob = n/(sum(n)))
    prediction <- prediction %>% 
      mutate(prob = round(prob, digits = 4))
    prediction <- na.omit(prediction)
    prediction <- prediction %>% 
      mutate(key = ifelse(result == 'yes', 1, 2))
    return(prediction)
}
create_prob_final <- function(two, yes, no) {
  yes_no <- rbind(yes, no)
  final <- two %>% full_join(yes_no, by = "key")
  final <-  final %>% rename(n_two = n.x, prob_two = prob.x, n_game = n.y, prob_game = prob.y)
  final <- final %>%  mutate(multiplied = prob_two*prob_game)
  final <- final %>%  mutate(prob_final = sum(multiplied))
}
prob_two <- create_prob_two(4,200)
prediction_yes <- create_prob_score(4,200,(-6+2), 'yes')
prediction_no <- create_prob_score(4,200,-6, 'no')
final <- create_prob_final(prob_two, prediction_yes, prediction_no)
final