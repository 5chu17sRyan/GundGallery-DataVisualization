install.packages("shiny")
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "numTree", label = "# of trees", value = 50, min = 1, max = 108)
  sliderInput(inputId = "DBH", label = "Diameter at breast height (ft)", min = 0.2, max = 5, step = 0.01)
)
server <- function(input, output){
  output
}
shinyApp(ui = ui, server = server)
