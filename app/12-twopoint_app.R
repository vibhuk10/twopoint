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
    sliderInput("time", "Seconds Remaining in the Quarter:",
                min = 0, max = 900, value = 450, step = 10),
    numericInput("score", "Score Differential:", -2),
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    dataTableOutput("probData"),
  )
  
)

server <- function(input, output) {
  
  output$probData <- renderDataTable({
    
    qtrInput <- input$quarter
    secondsInput <- input$time
    scoreInput <- input$score
    display(qtrInput, secondsInput, scoreInput)
    
  })
}

shinyApp(ui, server)
