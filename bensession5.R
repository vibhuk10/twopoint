display <- function(quarter, seconds, score) {
  # summarize two point conversions from this quarter
  prob_two <- create_prob_two(quarter,seconds)
  
  # summarize extra points from the given quarter, after 2016 (when xp was moved)
  prob_extra <- create_prob_extra(quarter, seconds)
  
  # calculate probability you'd win if you converted the 2pt attempt based on past games
  # look at games within 200 seconds of this game            
  prediction_yes_two <- 
    create_prob_score(
      quarter,
      timeleft = seconds, 
      score = score + 2, # see change if they convert 2pt
      result = "yes"
    )
  
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

display(quarter = 4, seconds = 90, score = -2)