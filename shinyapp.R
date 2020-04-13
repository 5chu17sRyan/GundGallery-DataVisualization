#install.packages("shiny")
library(ggplot2)
library(shiny)

#Contains benefit_from_CBH() and health_score()
source("DataProcessing.R")

#Contains create_title(), create_description(), create_sliders(), and show_output()
source("UserInterface.R")



#ui-----------
ui <- fluidPage(
  
  create_title(),
  create_description(),

  fluidRow(
    create_sliders(), 
    show_output()
  )
)


#server-----------------

server <- function(input, output){
  output$stats <- renderText({
    benefit <- input$numTree * benefit_from_CBH(pi * input$DBH)
    paste(
      "The system's health score is: ", health_score(benefit)
    )})

  #donut plot--------------

  #create dataframe
  donut_data <- data.frame(
    type = c("Filled", "Unfilled"),
    value = c(40, 60) #random numbers that are changed in render
  )

  #Make donut chart
  draw_plot <- function(donut_data){

    #define benefit again
    #If we initially calculated benefit outside of the renderText function we could avoid calculating it twice
    benefit <- input$numTree * benefit_from_CBH(pi * input$DBH)

    #make bar chart into pie chart
    donut_plot <- ggplot(donut_data, aes(x = 2, y = value, fill = type)) +
      geom_bar(size = 1, color = "transparent", stat = "identity")+
      theme_void()+ #blanking everything else out
      coord_polar("y", start = 0)+
      xlim(-4, 2.5) +   #donut thickness
      annotate(geom = 'text', x = -4, y =100, color="forest green",size=20, label=health_score(benefit)) #make text

    #make pie chart into donut chart
    pie_chart <- donut_plot + coord_polar("y", start = 0)
    pie_chart +
      theme(legend.position = "none") + #no legend
      scale_fill_manual(values = c("light gray", "forest green"))
  }

  #render donut
  output$donut <- renderPlot({
    benefit <- input$numTree * benefit_from_CBH(pi * input$DBH)
    donut_data$value[2] <- health_score(benefit) #Filled
    donut_data$value[1] <- 100-health_score(benefit) #Unfilled
    draw_plot(donut_data)
  })

}

shinyApp(ui = ui, server = server)
