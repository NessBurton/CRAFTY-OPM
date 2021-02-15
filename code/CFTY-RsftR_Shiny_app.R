
library(shiny)

### user interface -------------------------------------------------------------

# add elements as arguments to fluidPage
# input & output functions
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number bitch",
              min = 1, value = 25, max = 100),
  plotOutput("hist")
)

### server ---------------------------------------------------------------------

# tell server how to turn inputs into outputs
server <- function(input,output){
  
  # save objects to display to outputs
  #output$outputId #<- renderObject({code block})
  
  # access input values using input$inputID
  # e.g.
  output$hist <- renderPlot({hist(rnorm(input$num))})
 
  
}

### launch app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
