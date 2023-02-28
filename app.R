library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(sf)

tracks <- read.csv("https://raw.githubusercontent.com/amndazhang/project-dynamar-map/master/Processed_GPE3_Tracks_BUM_SAL.csv")
unique_fish <- unique(tracks$species)

map <- leaflet() %>% addProviderTiles("CartoDB.Positron")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                checkboxInput("all", "Show all", TRUE),
                conditionalPanel(
                  condition = "input.all == false",
                  selectInput("tags", "Tag SN: ", unique(tracks$ptt), multiple = FALSE)
                ),
                checkboxInput("points", "Show points", TRUE),
                checkboxInput("lines", "Show paths", TRUE),
                checkboxInput("legend", "Show legend", TRUE)),
  absolutePanel(bottom = 10, left = 20,
                p("Acknowledgment to scientists who donated the data HERE."))
)

server <- function(input, output, session) {
  
  # Basic map
  observe({
    filteredData <- reactive({ tracks[tracks$ptt,] })
    if (input$legend){
      leafletProxy("map", data = filteredData()) %>% addLegend("bottomright", 
                                                      colors = c("red",  "blue"),
                                                      labels = c("SAILFISH", "BLUE MARLIN"),
                                                      title = "Species",
                                                      opacity = 1)
    } else {
      leafletProxy("map", data = filteredData()) %>% clearControls()
    }
  })
  
  # Fish paths
  observe({
    
    if (!input$all){ # show specific
      filteredData <- reactive({
        tracks[tracks$ptt == input$tags,]
      })
      
      proxy <- leafletProxy("map", data = filteredData())
      proxy %>% clearShapes()
      
      s <- tracks[tracks$ptt == input$tags,]$species
      if (input$lines) {
        proxy %>% addPolylines(lng = ~lon, lat = ~lat, 
                               weight = 3, 
                               label = ~htmlEscape(species),
                               color = ifelse(s=="BUM", "blue", "red"))
      }
      if (input$points) {
        proxy %>% addCircles(radius = 10, weight = 5, 
                             color = ifelse(s=="BUM", "blue", "red"),
                             fillColor = "red", fillOpacity = 10, 
                             label = ~htmlEscape(date))
      }
    } else { # show all
      output$map <- renderLeaflet({
        leaflet(tracks) %>% addTiles() %>%
          fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%
          # Overlay groups
          addCircles(lng = ~lon, lat = ~lat, weight = 2, radius = 5,
                     label = ~htmlEscape(date), group = "Show All") %>%
          addPolylines(lng = ~lon, lat = ~lat, weight = 2, group = "Show All")
      })
    }
  })
}

shinyApp(ui, server)