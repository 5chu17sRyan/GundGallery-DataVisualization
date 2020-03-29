#install.packages("shiny")
setwd("/Users/wangzhaofang/Desktop/Gundproject/GundGallery-DataVisualization")
source("modelR.R")
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "numTree", 
              label = "# of trees", 
              value = 50, 
              min = 1, 
              max = 108),
  sliderInput(inputId = "DBH", 
              label = "Diameter at breast height (ft)", 
              value = 2,
              min = 0.2, 
              max = 5, 
              step = 0.01),
  textOutput("stats")
  #output plot 
    mainPanel(
      plotOutput(outputId="hist")
    )
)
server <- function(input, output){
  output$stats <- renderText({paste(
    "Your total benefit is $", round(input$numTree * benefit_from_CBH(pi * input$DBH), digit = 1)
  )})

  output$hist <- renderPlot({hist(rnorm(100))})
}
shinyApp(ui = ui, server = server)

