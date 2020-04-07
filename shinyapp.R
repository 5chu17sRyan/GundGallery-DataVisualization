#install.packages("shiny")
library(ggplot2)
library(shiny)

benefit_from_CBH <-function(CBH){
  benefit <- exp(-0.23102 + 0.80001 * log(CBH))
  return (benefit)
}

health_score <- function(benefit){
  score <- round(100 * benefit / (108 * benefit_from_CBH(pi * 5)), digit = 1)
  return (score)
}

#ui-----------
ui <- fluidPage(
  
  titlePanel(title=div(img(src="http://cliparts.co/cliparts/dc9/KRR/dc9KRRLEi.png", height = 70), "Gambier Tree Simulator"), windowTitle = "Gambier Tree Simulator "), 
  tags$h4("This simulation is based on tree data gathered on Kenyon College grounds. You can change the number of trees and the average size of the trees to see the impact removing trees versus letting them grow has on the ecosystem. The display on the right shows a score for the benefit these trees have to the ecosystem"),
  

  fluidRow(
    column( 5,
            sliderInput(inputId = "numTree",
                        label = "Number of Trees",
                        value = 50,
                        min = 1,
                        max = 108,
                        width = '100%'),
            sliderInput(inputId = "DBH",
                        label = "Diameter at breast height (ft)",
                        value = 2.5,
                        min = 0.2,
                        max = 5,
                        step = 0.01,
                        width = '100%'),
            style = "padding-top:50px"
              
    ),
    column(7,
      #textOutput("stats"),
       
      #output donut
      mainPanel(
        plotOutput(outputId = "donut", width = '150%', height = 600)
      )
    )
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
    type = c("Unfilled", "Filled"),
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
      scale_fill_manual(values = c("forest green", "light gray"))
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
