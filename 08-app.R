library(shiny)

ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Probabilities"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    selectInput("quarter", "Quarter:", 
                choices = c("1st" = "1",
                  "2nd" = "2",
                  "3rd" = "3",
                  "4th" = "4")),
    sliderInput("time", "Seconds Remaining in the Quarter:",
                min = 0, max = 900, value = 450, step = 10),
    textInput("score", "Score Differential:", -7)
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("countPlot"),
    plotOutput("probPlot"),
    dataTableOutput("probData"),
  )
  
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    output$countPlot <- renderPlot({
      
        qtrInput <- input$quarter
        secondsInput <- input$time
        scoreInput <- input$score
        
        set1 <- data %>% 
          filter(qtr == qtrInput & quarter_seconds_remaining>(secondsInput-50) & quarter_seconds_remaining<(secondsInput+50) & score_differential == scoreInput) %>% 
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
          mutate(score_differential2 = ifelse(posteam == away_team, -score_differential2, score_differential2))
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
        
        set3 %>%
          count(comeback, overtime) %>% 
          mutate(comeback = fct_reorder(comeback, n, sum)) %>% 
          ggplot() +
          geom_col(aes(x=comeback, y = n, fill = overtime)) +
          scale_fill_manual(values = c("#565656", "blue")) +
          labs(x = "end result")
      
    })
    output$probPlot <- renderPlot({
      
      qtrInput <- input$quarter
      secondsInput <- input$time
      scoreInput <- input$score
      
      set1 <- data %>% 
        filter(qtr == qtrInput & quarter_seconds_remaining>(secondsInput-50) & quarter_seconds_remaining<(secondsInput+50) & score_differential == scoreInput) %>% 
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
        mutate(score_differential2 = ifelse(posteam == away_team, -score_differential2, score_differential2))
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
        mutate(prob = round(prob, digits = 2))
      
      prediction_graph <- prediction %>%
        ggplot() +
        geom_col(aes(x=comeback, y = prob)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "end result")
      prediction_graph
      
    })
    output$probData <- renderDataTable({
      
      qtrInput <- input$quarter
      secondsInput <- input$time
      scoreInput <- input$score
      
      set1 <- data %>% 
        filter(qtr == qtrInput & quarter_seconds_remaining>(secondsInput-50) & quarter_seconds_remaining<(secondsInput+50) & score_differential == scoreInput) %>% 
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
        mutate(score_differential2 = ifelse(posteam == away_team, -score_differential2, score_differential2))
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
      prediction
      
    })
}

shinyApp(ui, server)
