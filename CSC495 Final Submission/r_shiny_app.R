shinyApp(
  # Define UI for random distribution app ----
  ui <- fluidPage(
    
    # App title ----
    titlePanel("Pitch Type Prediction"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select the random distribution type ----
        br(),
        
        # Input: Slider for the number of observations to generate ----
        sliderInput("a",
                    "Pitch Speed:",
                    value = 80,
                    min = 60,
                    max = 100),
        
        sliderInput("b",
                    "Spin Rate:",
                    value = 1800,
                    min = 700,
                    max = 3300),
        
        sliderInput("c",
                    "Verticle Break:",
                    value = 10,
                    min = -20,
                    max = 45),
        
        sliderInput("d",
                    "Horizontal Break:",
                    value = 10,
                    min = -30,
                    max = 45),
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Prediction", tableOutput("prediction")),
                    tabPanel("Results", tableOutput("tableResults"))
        )
        
      )
    )
  ),
  
  # Define server logic for random distribution app ----
  server <- function(input, output) {
    print(isolate(input$outcome))
    
    
    output$prediction <- renderTable({
      pred1 = bb_test1[5,]
      pred1$RelSpeed = input$a
      pred1$SpinRate = input$b
      pred1$InducedVertBreak = input$c
      pred1$HorzBreak = input$d
      
      predictions = as.data.frame(predict(rf_model, newdata=pred1))
      predictions
      
    })
    
    output$tableResults <- renderTable({
      pred
    })
    
  }
)


ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

install.packages("shiny")
library(shiny)

pred
