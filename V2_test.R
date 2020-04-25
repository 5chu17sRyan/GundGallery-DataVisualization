library(ggplot2)
library(shiny)
library(leaflet)
library(sp)
library(htmltools)


TotalRunoffAvoided <-16577.51
TotalPm <- 67.12
TotalCO2 <- 980776.97
totalB <- 462.84
benefit <- totalB

data <- read.csv("/Users/ryans/OneDrive/Desktop/Spring 2020/Software System Design/GGDataVisualization/GundGallery-DataVisualization/Tree_data.csv")
#Creates data frame for latitude and longitude
data.SP <- SpatialPointsDataFrame(data[, c(13,14)], data[, -c(13,14)])

#Creates a data frame which holds
#ID, Total Benefit ($), Runoff Avoided (gal), Particulate Matter Removed (oz), Lifetime CO2 equivalent of carbon stored (lbs)
#for each tree
accessData <- data.frame(data[,c(1,5,7,11,12)])
data$ID <- as.numeric(data$ID)
accessData[3,3]
ID = paste(data$ID)

labels <- lapply(seq(nrow(accessData)), function(i) {
  paste0( 'Tree Info<p>', '<br>', "Tree: ID: ", accessData[i, 1], '<br>',
          "Total Benefit: ", accessData[i, 2], ' dollars<br> ',
          "Stormwater Runoff avoided: ", accessData[i, 3],' gallons<br>',
          "Particulate Matter removed: ", accessData[i, 4],' ounces per year<br>',
          "CO2 absorbed: ", accessData[i, 5], ' pounds</p>' )
})


#ui-----------

ui <- fluidPage(
  tags$style(type = "text/css", "html,
             body {width:100%;height:100%}"),
  titlePanel(title=div(img(src="http://cliparts.co/cliparts/dc9/KRR/dc9KRRLEi.png", height = 70), "Gambier Tree Simulator"),
             windowTitle = "Gambier Tree Simulator "),
  tags$h4("This simulation is based on tree data gathered on Kenyon College grounds. You can change the number of trees and the average size of the trees to see the impact removing trees versus letting them grow has on the ecosystem. The display on the right shows a score for the benefit these trees have to the ecosystem"),

  fluidRow(
    column(4,
           leafletOutput("map1",
                         width= '100%',
                         height = 500),
           # actionButton(inputId = "clean", lable = "Start over"),

           # htmlOutput("message1")
           ),
    column(8,
           mainPanel(plotOutput(outputId = "donut", width = '100%', height = 500))
           )
  ),
  
  #Detailed data descriptions
  fluidRow(
    column(4, 
           tags$p("Description of Stormwater Runoff Avoided")
           ),
    column(4,
           tags$p("Description of Particulate Matter Removed")
           ),
    column(4,
           tags$p("Description of Carbon Sequestered")
           )
  )
)


#server-----------------

server <- function(input, output, session){

  #Renders Map
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

  #Reactions to clicks on map markers, Recalcualtes tree benefits by subtracting clicked data point
  updateB <- eventReactive(input$map1_marker_click,{
    totalB <<- totalB - accessData[input$map1_marker_click$id, 2]
  })

  updateRunoff <- eventReactive(input$map1_marker_click,{
    TotalRunoffAvoided <<- TotalRunoffAvoided - accessData[input$map1_marker_click$id, 3]
  })

  updateCO2 <- eventReactive(input$map1_marker_click,{
    TotalCO2 <<- TotalCO2 - accessData[input$map1_marker_click$id, 5]
  })

  updatePM <- eventReactive(input$map1_marker_click,{
    TotalPm <<- TotalPm - accessData[input$map1_marker_click$id, 4]
  })

  #Removes marker when clicked
  observe(
    leafletProxy("map1") %>%
      removeMarker(input$map1_marker_click$id)
  )

  #donut plot--------------

  #create dataframe
  donut_data <- data.frame(
    type = c("Filled", "Unfilled"),
    value = c(40, 60) #40 and 60 are placeholder values that are changed in render
  )

  #Make donut chart
  draw_plot <- function(donut_data, benefit){

    #define benefit again
    #benefit <-  round(100 * (updateB() / 462.84), digit = 1 )

    #Bright Cardinal: Red=255=ff, Green=37=25, Blue=56=38
    #Black: Red=0=00. Green=0=00, Blue=0=00
    #Forest Green: Red=34=22, Green=139=8b, Blue=34=22

    #Equations to calculate color values based on value of benefit (green -> black)
    red <- floor((34-0)/100*benefit+0)
    green <- floor((139-0)/100*benefit+0)
    blue <- floor((34-0)/100*benefit+0)

    #rgb = floor(144708.78*benefit+2263842)

    #Convert RGB values to hexadecimal
    hex_strings <- as.hexmode(c(red, green, blue))

    #Concatenate to create full hexcode
    code <- paste(hex_strings, collapse='')
    hash <- "#"
    hexcode <- paste(c(hash, code), collapse='')




    #make bar chart into pie chart
    donut_plot <- ggplot(donut_data, aes(x = 2, y = value, fill = type)) +
      geom_bar(size = 1, color = "transparent", stat = "identity")+
      theme_void()+ #blanking everything else out
      coord_polar("y", start = 0)+
      xlim(-4, 2.5) +   #donut thickness
      annotate(geom = 'text', x = -4, y =100, color=hexcode,size=20, label=benefit) #make text

    #make pie chart into donut chart
    pie_chart <- donut_plot + coord_polar("y", start = 0)
    pie_chart +
      theme(legend.position = "none") + #no legend
      scale_fill_manual(values = c("light gray", hexcode))
  }


  #This is the printing function, it definitely needs to be made prettier lol
  observe(if(is.null(input$map1_marker_click))
              output$message1 <- renderUI({
                  str1 <- paste("Health score: ", 100)
                  str2 <- paste("Stormwater Runoff Avoided: ", TotalRunoffAvoided, " gallons")
                  str3 <- paste("Particulate Matter removed: ", TotalPm, " ounces per year")
                  str4 <- paste("CO2 absorbed: ", TotalCO2)
                  HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
              })

          else
              output$message1 <- renderUI({
                  str1 <- paste("Health score: ", round(100 * (updateB() / 462.84), digit = 1 ))
                  str2 <- paste("Stormwatter Runoff Avoided ", round(updateRunoff(), digit = 1), " gallons")
                  str3 <- paste("Particulate Matter removed: ", round(updatePM(), digit = 1), " ounces per year")
                  str4 <- paste("CO2 absorbed: ", round(updateCO2(), digit = 1), " pounds")
                  HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
              })
          )

  #Renders donut
  observe(if(is.null(input$map1_marker_click))
            output$donut <- renderPlot({
              benefit <- 100
              donut_data$value[2] <- benefit #Filled
              donut_data$value[1] <- 100-benefit #Unfilled
              draw_plot(donut_data, benefit)
            })

          else
            output$donut <- renderPlot({
              benefit <- round(100 * (updateB() / 462.84), digit = 1 )
              donut_data$value[2] <- benefit #Filled
              donut_data$value[1] <- 100-benefit #Unfilled
              draw_plot(donut_data, benefit)
            })
  )
}

shinyApp(ui = ui, server = server)
