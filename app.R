library(ggplot2)
library(shiny)
library(leaflet)
library(sp)
library(htmltools)
library(rsconnect)

#deployApp()

TotalRunoffAvoided <-16577.51
TotalPm <- 67.12
TotalCO2 <- 980776.97
totalB <- 462.84
benefit <- totalB

data <- read.csv("Tree_data.csv")
#Creates data frame for latitude and longitude
#data.SP <- SpatialPointsDataFrame(data[, c(13,14)], data[, -c(13,14)])

#Creates a data frame which holds
#ID, Total Benefit ($), Runoff Avoided (gal), Particulate Matter Removed (oz), Lifetime CO2 equivalent of carbon stored (lbs)
#for each tree
accessData <- data.frame(data[,c(1,5,7,11,12,15)])
data$ID <- as.numeric(data$ID)
(accessData[114, 6])

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
  tags$h1(img(src = "Tree Icon 1.png", width = 60), "Gambier Tree Simulator"),
  tags$h4("This simulation is based on tree data gathered on Kenyon College grounds by the Office of Green Initiatives and David Heithaus. All of the trees in this simulation were real trees on Campus that were removed due to construction."),
  tags$h4("Click on the trees to remove them from the environment. The display on the right shows a score for the benefit these trees have to the ecosystem"),

  fluidRow(
    column(6,
           tabsetPanel(id="map_tab",
                       tabPanel("Map",
                                leafletOutput("map1",
                                              width= 500,
                                              height = 500)
                                # actionButton(inputId = "clean", lable = "Start over"),
                                # htmlOutput("message1")
                                )
                       )
           ),
    column(6,
           tabsetPanel(tabPanel("Health Score",plotOutput(outputId = "donut", width = 500, height = 500)))
    )
  ),

  #Adding blank row between application and further information
  fluidRow(tags$div(style='height:75px;')),

  #Detailed data descriptions
  fluidRow(
    column(4,
           tags$h4("Stormwater Runoff Avoided"),
           htmlOutput(outputId = "RunoffDescription")
           ),
    column(4,
           tags$h4("Particulate Matter Removed"),
           htmlOutput(outputId = "PM_Description")
           ),
    column(4,
           tags$h4("Oxygen Produced"),
           htmlOutput(outputId = "CO2Description")
           #tags$p("Description of Carbon Sequestered")
           )
  ),

  fluidRow(tags$div(style='height:50px;'))
)


#server-----------------

server <- function(input, output, session){
  TotalRunoffAvoided <-16577.51
  TotalPm <- 67.12
  TotalCO2 <- 980776.97
  totalB <- 462.84
  benefit <- totalB

  greenLeafIcon <- makeIcon(
    iconUrl = "Tree Icon 2.png",
    iconWidth = 38, iconHeight = 38,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = "Tree Shadow.png",
    shadowWidth = 38, shadowHeight = 38,
    shadowAnchorX = 22, shadowAnchorY = 94
  )

  #Renders Map
  output$map1 <- renderLeaflet({
    leaflet(data = data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(
        data = data,
        lng = ~long,
        lat = ~lat,
        icon = greenLeafIcon,
        layerId = ID,
        label = lapply(labels, htmltools::HTML)
        # label = HTML(paste0("<h3>Tree Info</h3>", "Tree ID = ", ID, "CO2 = ", sep = "\t"))
      )
  })

  #Reactions to clicks on map markers, Recalcualtes tree benefits by subtracting clicked data point
  
  updateB <- eventReactive(input$map1_marker_click,{
    if(accessData[input$map1_marker_click$id, 6] == 0)
      totalB <<- totalB - accessData[input$map1_marker_click$id, 2]
    else
      totalB <<- totalB
      })

  updateRunoff <- eventReactive(input$map1_marker_click,{
    if(accessData[input$map1_marker_click$id, 6] == 0)
      TotalRunoffAvoided <<- TotalRunoffAvoided - accessData[input$map1_marker_click$id, 3]
    else
      TotalRunoffAvoided <<- TotalRunoffAvoided
  })

  updateCO2 <- eventReactive(input$map1_marker_click,{
    if(accessData[input$map1_marker_click$id, 6] == 0)
      TotalCO2 <<- TotalCO2 - accessData[input$map1_marker_click$id, 5]
    else
      TotalCO2 <<- TotalCO2
  })

  updatePM <- eventReactive(input$map1_marker_click,{
    if(accessData[input$map1_marker_click$id, 6] == 0)
      TotalPm <<- TotalPm - accessData[input$map1_marker_click$id, 4]
    else
      TotalPm <<- TotalPm 
  })
  
  observeEvent(input$map1_marker_click,{
    accessData[input$map1_marker_click$id, 6] <<- accessData[input$map1_marker_click$id, 6] + 1
  }, priority = -1)
  
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

    red <- 0
    green <- 200
    blue <- 0

    if (benefit > 50 & benefit <= 100) {
      red <- floor(400-((200-0)/50*benefit))
      hex_strings <- as.hexmode(c(red, 200, blue))
    } else if (benefit >= 0 & benefit <= 50) {
      red <- 200
      green <- floor((200-0)/50*benefit+0)
      hex_strings <- as.hexmode(c(200, green, blue))
    } else if (benefit < 0){
      hex_strings <- as.hexmode(c(150, 0, 0))
    }

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


  #Events when map marker is clickeds
  observe(if(is.null(input$map1_marker_click))
    {
      #Render donut plot
      output$donut <- renderPlot({
        benefit <- 100
        donut_data$value[2] <- benefit #Filled
        donut_data$value[1] <- 100-benefit #Unfilled
        draw_plot(donut_data, benefit)
      })
    }
    else
    {
      #Render donut plot
      output$donut <- renderPlot({
        benefit <- round(100 * (updateB() / 462.84), digit = 1 )
        donut_data$value[2] <- benefit #Filled
        donut_data$value[1] <- 100-benefit #Unfilled
        draw_plot(donut_data, benefit)
      })


      #Calculation of Hazard Rate (Increase in mortality)
      campusSize <- 4046356.42 #meters squared
      heightOfAtmosphere <- 8200 #meters
      atmosphereAboveCampus <- campusSize*heightOfAtmosphere #meters cubed

      PM_Increase <- 67.12 - updatePM() #ounces per year

      microgramsPerOunce <- 28349523.1
      PM_microgramsIncrease <- PM_Increase*microgramsPerOunce #micrograms

      PM_concentrationIncrease <- PM_microgramsIncrease/atmosphereAboveCampus #micrograms per meter squared

      #"1.9 increase in PM concentration associated with 1.02 times the rate of death"
      rateOfDeathIncrease <- round( 1.02^(PM_concentrationIncrease/1.9), 6 ) #per year


      #Description of PM Removed
      output$PM_Description <- renderText({
      PMdescription <- paste("Particulate Matter is a pollutant which primarily comes from the exhaust fumes of vehicles and is associated with increasing mortality. Prior to removing trees, the trees in this ecosystem removed 67.12 oz of Particulate Matter (PM) per year. Because you removed some of these trees, the rate of death in Gambier would increase by",
                             "<b>", rateOfDeathIncrease, "</b>", "per year",
                             "<a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3734610/'>[1]</a>.",
                             "If no new trees were planted, after 100 years the rate of death would have increased by",
                             "<b>", round(rateOfDeathIncrease^100, 6), "</b>", "times.<p></p>",
                             "<p><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4740163/'>More information on pollution and respiraory diesases</a></p>",
                             "<p><a href='https://blog.ecosia.org/how-trees-reduce-air-pollution-world-environment-day/'>More information on how trees reduce pollution</a></p>",
                             "<p><a href='https://www.brighthub.com/environment/green-living/articles/61664.aspx'>Plants can reduce pollution too!</a></p>"
                             )
        HTML(PMdescription)
      })


      #Description of Stormwater Runoff Avoided
      avgTreeLifespan <- 95.0577864 #years
      totalRunoffAvoidedPerYear <- 174.1836269
      runoffIncrease <- round( totalRunoffAvoidedPerYear - ( updateRunoff()/avgTreeLifespan ), 2)

      output$RunoffDescription <- renderText({
        runoffDescription <- paste("Because of trees, whenever it rains some rain water catches on the trees' leaves. This water would end up evaporating before it ever reached the ground, preventing flooding. Prior to removing trees, the trees in this ecosystem prevented 174.18 gallons of stormwater runoff a year. Because you removed some of these trees, the yearly ammount of rainfall that could reach the ground and cause flodding in Gambier would increase by",
                                   "<b>", runoffIncrease, "</b>", " gallons.<p></p>",
                                   "<p><a href='https://www.charteredforesters.org/2017/06/trees-can-reduce-floods/'>More information on how trees prevent flooding</a></p>"
                                   )
        HTML(runoffDescription)
      })


      #Description of Oxygen Produced
      molecularMassCO2 <- 44.0095 #grams per mole
      molecularMassO2 <- 31.9988 #grams per mol
      ratioOfMasses <- molecularMassO2/molecularMassCO2

      maxCarbonSeqesteredPerYear <- 10317.69 #pounds
      carbonSequesteredDecrease <- maxCarbonSeqesteredPerYear - ( updateCO2()/avgTreeLifespan)
      oxygenProductionDecrease <- round( carbonSequesteredDecrease*ratioOfMasses, 2)

      output$CO2Description <- renderText({
        co2Description <- paste("Through photosynthesis, trees can convert carbon dioxide into oxygen. Prior to removing trees, the trees in this ecosystem produced 7,501.87 pounds of oxygen per year. Because you removed some of these trees, ",
                                "<b>", oxygenProductionDecrease, "</b>", " less pounds of oxygen are being produced each year.<p></p>",
                                "<p><a href = 'https://www.charteredforesters.org/2017/06/trees-can-reduce-floods/'>More information on how trees produce oxygen</a></p>"
                                )
        HTML(co2Description)
      })
    }
  )


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
}

shinyApp(ui = ui, server = server)
