library(shiny)
library(leaflet)
library(htmltools)
library(RColorBrewer)

tracks <- read.csv("https://raw.githubusercontent.com/amndazhang/project-dynamar-map/master/Processed_GPE3_Tracks_BUM_SAL.csv")
unique_fish <- unique(tracks$species)

map <- leaflet() %>% addProviderTiles("CartoDB.Positron")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                checkboxInput("showall", "Show all", TRUE),
                selectInput("tags", "Tag SN: ", unique(tracks$ptt), multiple = FALSE),
                checkboxInput("points", "Show points", TRUE),
                checkboxInput("lines", "Show paths", TRUE),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(tracks) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  observe({
    if (!input$showall){
      filteredData <- reactive({tracks[tracks$ptt == input$tags,]})
    } else {
      filteredData <- reactive({tracks[tracks$ptt,]})
    }
    
    leafletProxy("map", data = filteredData()) %>% 
      clearShapes() %>% 
      addCircles(lng = ~lon, lat = ~lat, weight = 2, radius = 5,
                 label = ~htmlEscape(date), group = "Points") %>% 
      addPolylines(lng = ~lon, lat = ~lat, weight = 2, color = "red", group = "Outline")
    
    if (input$legend) {
      leafletProxy("map", data = filteredData()) %>% 
        clearControls() %>%
        addLegend("bottomright", 
                  colors = c("red",  "blue"),
                  labels = c("SAL", "BUM"),
                  title = "Species",
                  opacity = 1)
    }
  })
  
}

shinyApp(ui, server)