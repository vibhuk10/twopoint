create_prob_two <- function(quarter, timeleft) {
  input_plays_two <- twopoint %>% 
    filter(qtr == quarter) %>% 
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

create_prob_extra <- function(quarter, timeleft) {
  input_plays_extra <- extrapoint %>% 
    filter(qtr == quarter & !(year == "2009") & !(year == "2010") & !(year == "2011") & !(year == "2012") & !(year == "2013") & !(year == "2014") & !(year == "2015")) %>% 
    select(game_id, posteam, home_team, away_team, score_differential, qtr, quarter_seconds_remaining, desc, game_date, extra_point_result)
  input_plays_extra <- input_plays_extra %>% 
    mutate(extra_point_result2 = case_when(
      extra_point_result == "failed" ~ "failed",
      extra_point_result == "blocked" ~ "failed",
      extra_point_result == "good" ~ "good",
    ))
  prob_extra <- input_plays_extra %>% count(extra_point_result2)
  prob_extra <- prob_extra %>% 
    mutate(prob = n/(sum(n)))
  prob_extra <- prob_extra %>% 
    mutate(prob = round(prob, digits = 3))
  prob_extra <- prob_extra %>% 
    mutate(key = case_when(
      extra_point_result2 == "failed" ~ 2,
      extra_point_result2 == "good" ~ 1,
    ))
  return(prob_extra)
}

create_prob_score <- function(quarter, timeleft, score, result) {
    base_plays <- data %>% 
      filter(qtr == quarter & quarter_seconds_remaining>(timeleft-200) & quarter_seconds_remaining<(timeleft+200) & score_differential == (-1*score) & play_type == "kickoff") %>% 
      select(game_id, posteam, defteam, home_team, away_team, score_differential, qtr, quarter_seconds_remaining, desc, game_date) %>% 
      group_by(game_id) %>% 
      slice(n()) %>% 
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
      mutate(score_differential2 = ifelse(defteam == away_team, -score_differential2, score_differential2))
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
  final_prob <- final %>% pull(multiplied) %>% sum()
  final_prob <- final_prob*100
  final_prob <- round(final_prob, digits = 1)
  final_prob <- paste0(final_prob, "%")
}
display <- function(quarter, seconds, score) {
  prob_two <- create_prob_two(quarter,seconds)
  prob_extra <- create_prob_extra(quarter,seconds)
  prediction_yes_two <- create_prob_score(quarter,seconds,(score+2), 'yes')
  prediction_no <- create_prob_score(quarter,seconds,score, 'no')
  prediction_yes_extra <- create_prob_score(quarter,seconds,(score+1), "yes")
  final_two <- create_prob_final(prob_two, prediction_yes_two, prediction_no)
  final_extra <- create_prob_final(prob_extra, prediction_yes_extra, prediction_no)
  final <- tribble(
    ~"Play Type", ~"Win Probability",
    "Two point conversion", final_two,
    "Extra Point", final_extra
  )
  final
}
display(4, 400, -2)
