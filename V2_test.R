library(ggplot2)
library(shiny)
library(leaflet)
library(sp)
library(htmltools)
TotalOzone <-1304.66
TotalPm <- 67.12
TotalCO2 <- 980776.97
totalB <- 462.84
benefit <- totalB
data <- read.csv("/Users/wangzhaofang/Desktop/GundProject/GundGallery-DataVisualization/Tree_data.csv")
data.SP <- SpatialPointsDataFrame(data[, c(13,14)], data[, -c(13,14)])
accessData <- data.frame(data[,c(1,5,9,11,12)])
data$ID <- as.numeric(data$ID)
accessData[3,3]
ID = paste(data$ID)
labels <- lapply(seq(nrow(accessData)), function(i) {
  paste0( 'Tree Info<p>', '<br>', "Tree: ID: ", accessData[i, 1], '<br>', 
          "Total Benefit: ", accessData[i, 2], '<br> ', 
          "Ozone aborbed: ", accessData[i, 3],'<br>', 
          "PM 2.5 absorbed: ", accessData[i, 4],'<br>', 
          "CO2 absorbed: ", accessData[i, 5], '</p>' ) 
})
#ui-----------
ui <- bootstrapPage(
  tags$style(type = "text/css", "html,
             body {width:100%;height:100%}"),
  titlePanel(title=div(img(src="http://cliparts.co/cliparts/dc9/KRR/dc9KRRLEi.png", height = 70), "Gambier Tree Simulator"), windowTitle = "Gambier Tree Simulator "),
  tags$h4("This simulation is based on tree data gathered on Kenyon College grounds. You can change the number of trees and the average size of the trees to see the impact removing trees versus letting them grow has on the ecosystem. The display on the right shows a score for the benefit these trees have to the ecosystem"),
  leafletOutput("map1", width= "50%", height = "50%"),
  # actionButton(inputId = "clean", lable = "Start over"),
  htmlOutput("message1")
)


#server-----------------

server <- function(input, output, session){

  output$map1 <- renderLeaflet({
    leaflet(data = data) %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircleMarkers(
        data = data,
        lng = ~long,
        lat = ~lat,
        layerId = ID,
        label = lapply(labels, htmltools::HTML)
        # label = HTML(paste0("<h3>Tree Info</h3>", "Tree ID = ", ID, "CO2 = ", sep = "\t"))
      )
  })
  newTotalPm <- reactive({
    TotalPm <<- TotalPm - accessData[input$map1_marker_click$id, 4]
  })
  newTotalCO2 <- reactive({
    TotalCO2 <<- TotalCO2 - accessData[input$map1_marker_click$id, 5]
  })
  newTotalO <- reactive({
    TotalOzone <<- TotalOzone - accessData[input$map1_marker_click$id, 3]
  })
  newTotalB <- reactive({
    totalB <<- totalB - accessData[input$map1_marker_click$id, 2]
  })
  updateB <- eventReactive(input$map1_marker_click,{
      newTotalB() 
    })
  updateO <- eventReactive(input$map1_marker_click,{
    newTotalO() 
  })
  updateCO2 <- eventReactive(input$map1_marker_click,{
    newTotalCO2() 
  })
  updatePM <- eventReactive(input$map1_marker_click,{
    newTotalPm() 
  })
  
  observe(
    leafletProxy("map1") %>%
      
      removeMarker(input$map1_marker_click$id)
  )
  
  #This is the printing function, it definitetly needs to be made prettier lol
  observeEvent(input$map1_marker_click,{
                 # output$message1 <- renderText({
                 #   paste("Health score: ", round(100 * (updateB() / 462.84), digit = 1 ),
                 #         "PM 2.5 absorbed: ", updatePM(), "Ozone gas absorbed: ", updateO(),
                 #         "CO2 absorbed: ", updateCO2())
                 # })
    output$message1 <- renderUI({
      str1 <- paste("Health score: ", round(100 * (updateB() / 462.84), digit = 1 ))
      str2 <- paste("PM 2.5 absorbed: ", updatePM())
      str3 <- paste("Ozone gas absorbed: ", updateO())
      str4 <- paste("CO2 absorbed: ", updateCO2())
      HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    })
               })
}

shinyApp(ui = ui, server = server)
