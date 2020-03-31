#install.packages("shiny")
setwd("/Users/lisaw/Documents/Software Development/DataVisualization/Zach's Code")


library(dplyr)
library(ggplot2)
library(ggiraph)
library(ggplot2)
library(scales)


source("modelR.R")
library(shiny)

#ui-----------
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
    textOutput("stats"),
    
    #output donut
    mainPanel(
      plotOutput(outputId = "donut")
    )
)

#server-----------------
server <- function(input, output){ #check this
    output$stats <- renderText({paste(
        "Benefit: $", round(input$numTree * benefit_from_CBH(pi * input$DBH), digit = 1)
    )})
    

    
    #donut plot--------------
    
    #create dataframe
    donut_data <- data.frame(
      type = c("Unfilled", "Filled"),
      value = c(40, 60) #change this to reflect default values later
    )
    
    # Functions
    draw_plot <- function(donut_data){
      donut_plot <- ggplot(donut_data, aes(x = "", y = value, fill = type)) +
        geom_bar(width = 1, size = 1, color = "gray", stat = "identity")
      
      pie_chart <- donut_plot + coord_polar("y", start = 0)
      pie_chart + 
        theme(axis.text.x = element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              legend.position = "none",
              axis.ticks = element_blank(),
              plot.title = element_text(size = 19, face = "bold")) +
        scale_fill_manual(values = c("dark green", "white"))
      
    }
  
    #render donut
      output$donut <- renderPlot({
        donut_data$value[2] <- round(input$numTree * benefit_from_CBH(pi * input$DBH), digit = 1) #Filled
        donut_data$value[1] <- 776.2-round(input$numTree * benefit_from_CBH(pi * input$DBH), digit = 1) #Unfilled
        draw_plot(donut_data)
      })
      
    }

shinyApp(ui = ui, server = server)
