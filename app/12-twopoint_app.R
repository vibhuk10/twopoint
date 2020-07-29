library(shiny)
source("13-twopoint_probs.R")
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Probabilities"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    selectInput("quarter", "Quarter:", 
                choices = c("1st" = 1,
                            "2nd" = 2,
                            "3rd" = 3,
                            "4th" = 4)),
    textInput("time", "Seconds Remaining in the Quarter:", "02:00"),
    numericInput("score", "Score Differential:", -2),
    sliderInput("lower_bound", "Input Amount of Seconds for Lower Bound of Time Left:",
                min = -900, max = 0, value = -100, step = 10),
    sliderInput("upper_bound", "Input Amount of Seconds for Upper Bound of Time Left:",
                min = 0, max = 900, value = 100, step = 10),
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("probPlot"),
  )
  
)

server <- function(input, output) {
  
  output$probPlot <- renderPlot({
    
    qtrInput <- input$quarter
    timeInput <- input$time
    scoreInput <- input$score
    lower_boundInput <- input$lower_bound
    upper_boundInput <- input$upper_bound
    
    timeInput <- time_to_seconds(timeInput)
    
    probabilities_table <- 
      display(
        quarter = qtrInput,
        seconds = timeInput,
        score = scoreInput,
        lower_seconds_bound = lower_boundInput,
        upper_seconds_bound = upper_boundInput,
        two_data = twopoint,
        extra_data = extrapoint,
        full_data = data
      )
    
    probabilities_table %>% 
      ggplot(aes(x = play_type, y = win_prob, fill = play_type)) +
      geom_col() +
      geom_text(aes(y = win_prob + 0.03, label = paste0(round(win_prob, 3) * 100, "% "))) +
      scale_fill_manual(values = c("blue", "orange")) + 
      guides(fill = FALSE) +
      labs(x = "", y = "Win probability", title = "Should you go for two?") +
      scale_y_continuous(labels = scales::percent)
    
  })
}

shinyApp(ui, server)
