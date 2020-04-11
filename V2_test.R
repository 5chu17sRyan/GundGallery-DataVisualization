library(ggplot2)
library(shiny)
library(leaflet)
library(sp)
data <- read.csv("/Users/wangzhaofang/Desktop/GundProject/GundGallery-DataVisualization/Tree_data.csv")
data.SP <- SpatialPointsDataFrame(data[, c(13,14)], data[, -c(13,14)])
#ui-----------
ui <- bootstrapPage(
  tags$style(type = "text/css", "html,
             body {width:100%;height:100%}"),
  titlePanel(title=div(img(src="http://cliparts.co/cliparts/dc9/KRR/dc9KRRLEi.png", height = 70), "Gambier Tree Simulator"), windowTitle = "Gambier Tree Simulator "),
  tags$h4("This simulation is based on tree data gathered on Kenyon College grounds. You can change the number of trees and the average size of the trees to see the impact removing trees versus letting them grow has on the ecosystem. The display on the right shows a score for the benefit these trees have to the ecosystem"),
  leafletOutput("map1", width= "50%", height = "50%")
 
)


#server-----------------

server <- function(input, output, session){
  # output$map1 <- renderLeaflet({
  #   leaflet(data = data) %>% 
  #     addTiles() %>% 
  #     addCircleMarkers(
  #       lng = ~Longtitude,
  #       lat = ~Latitude
  #       # label = paste("LayerID=", ID, sep="")
  #     )
  # })
  output$map1 <- renderLeaflet({
    leaflet(data = data) %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircleMarkers(
        data = data,
        lng = ~long,
        lat = ~lat
      )
  })
  
}

shinyApp(ui = ui, server = server)
