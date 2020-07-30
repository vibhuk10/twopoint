time_to_seconds <- function(time) {
  # this functions takes in string time input and outputs the time in seconds
  minutes <- 
    str_sub(time, 1, str_locate(time, ":")[1, 1] - 1)
  minutes <- 
    as.numeric(minutes) * 60
  seconds <- 
    str_sub(time, str_locate(time, ":")[1, 1] + 1)
  seconds <- 
    as.numeric(seconds)
  seconds_left <- minutes + seconds
  seconds_left
}

create_prob_two <- function(quarter, timeleft, data) {
  # this functions takes in the quarter and outputs the probabilities for a two point conversion
  
  input_plays_two <- 
    data %>% 
    filter(qtr == quarter) %>% 
    select(
      game_id, posteam, home_team, away_team, score_differential, 
      qtr, quarter_seconds_remaining, desc, game_date, two_point_conv_result
    )
  
  prob_two <- 
    input_plays_two %>% 
    count(two_point_conv_result) %>% 
    mutate(
      prob = n/(sum(n)),
      prob = round(prob, digits = 3),
      key = case_when(
        two_point_conv_result == "failure" ~ 2,
        two_point_conv_result == "success" ~ 1,
      )
    )
  
  prob_two
}

create_prob_extra <- function(quarter, timeleft, data) {
  # this functions takes in the quarter and outputs the probabilities for an extra point
  
  input_plays_extra <- 
    data %>% 
    filter(qtr == quarter,  year >= 2016) %>% 
    select(
      game_id, posteam, home_team, away_team, score_differential, qtr, 
      quarter_seconds_remaining, desc, game_date, extra_point_result
    ) %>% 
    mutate(
      extra_point_result2 = 
        case_when(
          extra_point_result == "failed" ~ "failed",
          extra_point_result == "blocked" ~ "failed",
          extra_point_result == "good" ~ "good"
        )
    )
    
  prob_extra <- 
    input_plays_extra %>% 
    count(extra_point_result2) %>% 
    mutate(
      prob = n/(sum(n)),
      prob = round(prob, digits = 3),
      key = case_when(
        extra_point_result2 == "failed" ~ 2,
        extra_point_result2 == "good" ~ 1,
    ))
  
  prob_extra
}

create_prob_score <- function(quarter, timeleft, score, result, lower_seconds_bound, upper_seconds_bound, kickoffs_data, last_plays_data) {
  # this functions takes in the quarter, timeleft, score, and result and outputs the probabilities of winning
  
  base_plays1 <- 
    kickoffs_data %>% 
    filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*score) & play_type == "kickoff") %>% 
    group_by(game_id) %>% 
    slice(n()) %>% 
    ungroup()
  
  base_plays2 <- 
    kickoffs_data %>% 
    filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*score) & yardline_100 < 85 & yardline_100 > 70) %>% 
    group_by(game_id) %>% 
    slice(1) %>% 
    ungroup()
  
  base_plays <- rbind(base_plays1, base_plays2)
  base_plays <- distinct(base_plays, game_id, .keep_all = TRUE)
    
    
    combined <- 
      base_plays %>% 
      left_join(last_plays_data, by = "game_id") %>% 
      mutate(
        score_differential2 = ifelse(defteam == away_team, -score_differential2, score_differential2),
        comeback = case_when(
          score_differential2 > 0 ~ "win"
        )
      )
    
    prediction <- 
      combined %>% 
      count(comeback) %>% 
      mutate(
        count = sum(n),
        prob = n/(sum(n)),
        prob = round(prob, digits = 4)
        ) %>% 
      na.omit(prediction) %>% 
      mutate(key = ifelse(result == 'yes', 1, 2))
    
    prediction
}

create_prob_final <- function(two, yes, no) {
  # this functions combines the probabities using expected probabilty output the final probabilty of winning the game
  
  yes_no <- rbind(yes, no)
  
  final <- two %>% 
    full_join(yes_no, by = "key") %>% 
    rename(n_two = n.x, prob_two = prob.x, n_game = n.y, prob_game = prob.y) %>%
    mutate(
      multiplied = prob_two*prob_game,
      prob_final = sum(multiplied)
      )
  
  final_prob <- final %>%
    pull(multiplied) %>%
    sum() 
  
  final_prob
}

amount_of_games <- function(yes_data, no_data) {
  # this functions takes the data used and outputs the amount of games used to come up with the probabilities
  
  games <- yes_data$count + no_data$count
}

display <- function(quarter, time, score, lower_seconds_bound, upper_seconds_bound, two_data, extra_data, kickoffs_data, last_plays_data) {

  # convert time (string) to seconds (numeric)
  seconds <- 
    time_to_seconds(time)
  
  # summarize two point conversions from this quarter
  prob_two <- 
    create_prob_two(
      quarter,
      timeleft = seconds,
      data = two_data
    )
  
  # summarize extra points from the given quarter, after 2016 (when xp was moved)
  prob_extra <- 
    create_prob_extra(
      quarter,
      timeleft = seconds,
      data = extra_data
    )
  
  # calculate probability you'd win if you converted the 2pt attempt based on past games
  prediction_yes_two <- 
    create_prob_score(
      quarter,
      timeleft = seconds, 
      score = score + 2, # see change if they convert 2pt
      result = "yes",
      lower_seconds_bound,
      upper_seconds_bound,
      kickoffs_data = kickoffs_data,
      last_plays_data = last_plays_data
    )
  
  # calculate probability you'd win if you didn't the 2pt attempt or extra point based on past games
  prediction_no <- 
    create_prob_score(
      quarter,
      timeleft = seconds,
      score = score,
      result = 'no',
      lower_seconds_bound,
      upper_seconds_bound,
      kickoffs_data = kickoffs_data,
      last_plays_data = last_plays_data
    )
  
  # calculate probability you'd win if you made the extra point based on past games
  prediction_yes_extra <- 
    create_prob_score(
      quarter,
      timeleft = seconds,
      score = score+1,
      result = "yes",
      lower_seconds_bound,
      upper_seconds_bound,
      kickoffs_data = kickoffs_data,
      last_plays_data = last_plays_data
    )
  
  # uses expected probability to create a final win probability if you went for the two point conversion
  final_two <- 
    create_prob_final(
      prob_two,
      prediction_yes_two,
      prediction_no
    )
  
  # uses expected probability to create a final win probability if you went for the extra point
  final_extra <- 
    create_prob_final(
      prob_extra,
      prediction_yes_extra,
      prediction_no
    )
  
  #summarizes the number of games used to calculate the probability given for two point conversion
  games_two <- 
    amount_of_games(
      yes_data =  prediction_yes_two,
      no_data = prediction_no
    )
  
  #summarizes the number of games used to calculate the probability given for extra point
  games_extra <- 
    amount_of_games(
      yes_data =  prediction_yes_extra,
      no_data = prediction_no
    )
  
  #creates a table with win probabilities for each option
  final <- 
    tibble(
      play_type = c("two point", "extra point"),
      win_prob = c(final_two, final_extra),
      games = c(games_two, games_extra)
    )
  
  final
}

display(quarter = 3,
        time = "2:20",
        score = -3,
        lower_seconds_bound = -50,
        upper_seconds_bound = 50,
        two_data = twopoint,
        extra_data = extrapoint,
        kickoffs_data = kickoffs,
        last_plays_data = last_plays
        )