install.packages("shiny")
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num", label = "# of trees", value = 50, min = 1, max = 108)
)
server <- function(input, output){}
shinyApp(ui = ui, server = server)
