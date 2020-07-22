library(shiny)
source("07-probabilities.R")
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
        
        combined <- create_combined(qtrInput,secondsInput,scoreInput, data)
        
        combined %>%
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
      
      combined <- create_combined(qtrInput,secondsInput,scoreInput, data)
      prediction <- create_prediction(combined)
      
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
      
      combined <- create_combined(qtrInput,secondsInput,scoreInput, data)
      prediction <- create_prediction(combined)
      prediction <-  prediction %>% mutate(prob = (prob*100))
      prediction <- prediction %>% mutate(prob = paste0(prob, "%"))
      prediction
      
    })
}

shinyApp(ui, server)
