# Martin Kloster
# Date: 11/8/2024
# Class: Exploratory and Cloud Based Data Analysis (STAT 442)
# Professor: Dr. Bill Alsaker

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(sf)
library(viridisLite)

# Load Accident CSV
Accidents <- read.csv("accident.csv", header = TRUE)

# Define UI for application
ui <- fluidPage(
    # Theme
    theme = shinytheme("darkly"),

    # Application title
    titlePanel("Pedestrians in Accidents by State and Weather (2022)"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("State",
                        "Select State:",
                        c("All", unique(Accidents[,2])),
                        selected = "All",
                        multiple = FALSE,
                        selectize = TRUE),
            conditionalPanel(condition = "input.Options.includes('Compare')", 
                        selectInput("State2",
                                    "Select Second State:",
                                    c(unique(Accidents[,2])),
                                    selected = "All",
                                    multiple = FALSE,
                                    selectize = TRUE),),
            selectInput("Weather",
                        "Select Weather:",
                        c("All", unique(Accidents[,63])),
                        selected = "All",
                        multiple = FALSE,
                        selectize = TRUE),
            # checkboxGroupInput("Variable",
            #             "Select Variable(s):",
            #             c("Pedestrians" = "PEDS", "Fatalaties" = "FATALS"),
            #             selected = "PEDS"),
            checkboxGroupInput("Options",
                        "Select Options:",
                        c("Exclude Outliers" = "Outliers", "Comparison" = "Compare"),
                        selected = "Outliers")
        ),
            

        # Show a plot of the generated distribution
        mainPanel(
                  tabsetPanel(
                            tabPanel("Map", leafletOutput("mapPlot")),
                            tabPanel("Barchart",
                                    fluidRow(plotOutput("distPlot")),
                                    fluidRow(conditionalPanel(condition = "input.Options.includes('Compare')",
                                                              plotOutput("distPlot2")))
                            )
                  ),
            #tabPanel("Extra Data", plotOutput("distPlot"))
        )
    )
)

# Define server
server <- function(input, output) {
  # Interactive Map Data Creation
  map_df = reactive({
    # Filtering Logic
    Data <- Accidents
    if(input$State != "All"){Data <- filter(Data, input$State == Accidents[,2])}
    if(input$Weather != "All"){Data <- filter(Data, input$Weather == Data[,63])}
    
    # Simple Data Processing (Conversion for map points)
    Data %>%
      filter(!is.na(LONGITUD) & !is.na(LATITUDE)) %>%
      st_as_sf(coords = c("LONGITUD","LATITUDE")) %>%
      st_set_crs(4326)
  })
  
  # Interactive Map Element
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -96, lat = 38.5, zoom = 4) %>%
      addCircleMarkers(data = map_df(), radius = .5)
  })
  # Main Bar chart of variable
    output$distPlot <- renderPlot({
        # Filtering Logic
        Data <- Accidents
        if(input$State != "All"){Data <- filter(Data, input$State == Accidents[,2])}
        if(input$Weather != "All"){Data <- filter(Data, input$Weather == Data[,63])}
        
        # Setting Limits
        if (length(input$Options) != 0){
          if("Outliers" %in% input$Options){
            XLimit <- quantile(Data$PEDS, probs = .75, na.rm = FALSE) + (1.5 * IQR(Data$PEDS))
            if(XLimit <= 2){XLimit <- 2.5}
            }
          else(XLimit <- max(Data$PEDS) + .5)
        }
        else {
          XLimit <- max(Data$PEDS) + .5
        }

        # Create the plots based on the input
        if(input$State == "All" & input$Weather == "All"){
          ggplot(data = Data) +
            geom_bar(mapping = aes(x = PEDS, fill = WEATHERNAME)) +
            labs(x = "Pedestrians", y = "Count", title = "Pedestrians in Accidents: All States | All Weather", fill = "States") +
            xlim(-.5,XLimit) +
            theme_bw()
        }
        
        else if(input$State != "All" & input$Weather == "All"){
          ggplot(data = Data) +
            geom_bar(mapping = aes(x = PEDS, fill = WEATHERNAME)) +
            labs(x = "Pedestrians", y = "Count", title = paste("Pedestrians in Accidents:", input$State, "| All Weather"), fill = "Weather") +
            xlim(-.5,XLimit) +
            theme_bw()
        }
        
        else if(input$State == "All" & input$Weather != "All"){
          ggplot(data = Data) +
            geom_bar(mapping = aes(x = PEDS, fill = STATENAME)) +
            labs(x = "Pedestrians", y = "Count", title = paste("Pedestrians in Accidents: All States |", input$Weather), fill = "States") +
            xlim(-.5,XLimit) +
            theme_bw()
        }
        
        else if(input$State != "All" & input$Weather != "All"){
          ggplot(data = Data) +
            geom_bar(mapping = aes(x = PEDS), fill = "#303941") +
            labs(x = "Pedestrians", y = "Count", title = paste("Pedestrians in Accidents:", input$State, "|", input$Weather)) +
            xlim(-.5,XLimit) +
            theme_bw()
        }
    })
    output$distPlot2 <- renderPlot({
        # Filtering Logic
        Data2 <- Accidents
        if(input$State2 != "All"){Data2 <- filter(Data2, input$State2 == Accidents[,2])}
        if(input$Weather != "All"){Data2 <- filter(Data2, input$Weather == Data2[,63])}
        
        # Setting Limits
        if (length(input$Options) != 0){
          if("Outliers" %in% input$Options){
          XLimit2 <- quantile(Data2$PEDS, probs = .75, na.rm = FALSE) + (1.5 * IQR(Data2$PEDS))
          if(XLimit2 <= 2){XLimit2 <- 2.5}}
          else(XLimit2 <- max(Data2$PEDS) + .5)
        }
        else {
          XLimit2 <- max(Data2$PEDS) + .5
        }

        # Create the plots based on the input
        if(input$Weather == "All"){
          ggplot(data = Data2) +
            geom_bar(mapping = aes(x = PEDS, fill = WEATHERNAME)) +
            labs(x = "Pedestrians", y = "Count", title = paste("Pedestrians in Accidents:", input$State2, "| All Weather"), fill = "Weather") +
            xlim(-.5,XLimit2) +
            theme_bw()
        }
        
        else if(input$Weather != "All"){
          ggplot(data = Data2) +
            geom_bar(mapping = aes(x = PEDS), fill = "#303941") +
            labs(x = "Pedestrians", y = "Count", title = paste("Pedestrians in Accidents:", input$State2, "|", input$Weather)) +
            xlim(-.5,XLimit2) +
            theme_bw()
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
