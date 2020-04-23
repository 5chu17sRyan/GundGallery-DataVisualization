newTotalPm <- reactive({
  TotalPm <<- TotalPm - accessData[input$map1_marker_click$id, 4]
})

newTotalCO2 <- reactive({
  TotalCO2 <<- TotalCO2 - accessData[input$map1_marker_click$id, 5]
})

newTotalRunoff <- reactive({
  TotalRunoffAvoided <<- TotalRunoffAvoided - accessData[input$map1_marker_click$id, 3]
})

newTotalB <- reactive({
  totalB <<- totalB - accessData[input$map1_marker_click$id, 2]
})

updateB <- eventReactive(input$map1_marker_click,{
  newTotalB()
})

updateRunoff <- eventReactive(input$map1_marker_click,{
  newTotalRunoff()
})

updateCO2 <- eventReactive(input$map1_marker_click,{
  newTotalCO2()
})

updatePM <- eventReactive(input$map1_marker_click,{
  newTotalPm()
})