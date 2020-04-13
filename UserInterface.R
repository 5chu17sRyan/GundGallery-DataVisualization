
create_tree_sim_title <-function(){
  titlePanel(title=div(img(src="http://cliparts.co/cliparts/dc9/KRR/dc9KRRLEi.png", height = 70), 
                       "Gambier Tree Simulator"), 
             windowTitle = "Gambier Tree Simulator ") 
}

create_tree_sim_description <- function(){
  tags$h4("This simulation is based on tree data gathered on Kenyon College grounds. You can change the number of trees and the average size of the trees to see the impact removing trees versus letting them grow has on the ecosystem. The display on the right shows a score for the benefit these trees have to the ecosystem")
}

create_input_sliders <- function(){
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
          
  )
}

show_output <- function(){
  column(7,
         #textOutput("stats"),
         
         #output donut
         mainPanel(
           plotOutput(outputId = "donut", width = '150%', height = 600)
         )
  )
}
