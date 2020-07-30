library(tidyverse)
library(shiny)

source("./app/14-data_twopoint_app.R")
source("./app/13-twopoint_probs.R")

ui <- pageWithSidebar(
  
  # App title ----
  headerPanel(title = "Two Point Conversion Tool", windowTitle = "Two Point Conversion Tool"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    selectInput("quarter", "Quarter:", 
                choices = c("1st" = 1,
                            "2nd" = 2,
                            "3rd" = 3,
                            "4th" = 4)),
    textInput("time", "Seconds Remaining in the Quarter:", "2:00"),
    numericInput("score", "Score Differential:", -1),
    sliderInput("lower_bound", "Amount of Seconds for Lower Bound for Time Left:",
                min = -900, max = 0, value = -50, step = 10),
    sliderInput("upper_bound", "Amount of Seconds for Upper Bound for Time Left:",
                min = 0, max = 900, value = 50, step = 10),
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("probPlot", width = "auto", height = "560px"),
  )
  
)

server <- function(input, output) {
  
  output$probPlot <- renderPlot({
    
    qtrInput <- input$quarter
    timeInput <- input$time
    scoreInput <- input$score
    lower_boundInput <- input$lower_bound
    upper_boundInput <- input$upper_bound
    
    probabilities_table <- 
      display(
        quarter = qtrInput,
        time = timeInput,
        score = scoreInput,
        lower_seconds_bound = lower_boundInput,
        upper_seconds_bound = upper_boundInput,
        two_data = twopoint,
        extra_data = extrapoint,
        kickoffs_data = kickoffs,
        last_plays_data = last_plays
      )
    
    theme_set(theme_classic(base_size = 20))
    
    probabilities_table %>% 
      ggplot(aes(x = play_type, y = win_prob, fill = play_type)) +
      geom_col() +
      geom_text(size = 6, aes(y = win_prob + 0.03, label = paste0(round(win_prob, 3) * 100, "% ", " of ", games, " games"))) +
      scale_fill_manual(values = c("#BA262B", "#2E5090")) + 
      guides(fill = FALSE) +
      labs(x = "", y = "Win probability") +
      ggtitle("Should you go for 2?")+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(labels = scales::percent)
    
  })
}

shinyApp(ui, server)
