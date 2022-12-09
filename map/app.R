library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)

tracks <- read.csv("https://raw.githubusercontent.com/amndazhang/project-dynamar-map/master/Processed_GPE3_Tracks_BUM_SAL.csv")
unique_fish <- unique(tracks$species)

map <- leaflet() %>% addProviderTiles("CartoDB.Positron")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Depth", value = c(-2000, 0), min = -2000, max = 0),
                selectInput("tags", "Tag SN: ", unique(tracks$ptt), multiple = TRUE),
                checkboxInput("legend", "Show legend", TRUE),
                checkboxInput("points", "Show points", TRUE),
                checkboxInput("lines", "Show paths", TRUE),
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(tracks) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  # Update Tag SN input.
  filteredData <- reactive({
    tracks[tracks$ptt == input$tags,]
  })
  
  # Update points and paths checkboxes.
  observe({
    proxy <- leafletProxy("map", data = filteredData())
    proxy %>% clearShapes()
    
    if (input$lines) {
      proxy %>% addPolylines(lng = ~lon, lat = ~lat, color = "#ff9632", 
                             weight = 5, fillOpacity = 2, 
                             label = ~htmlEscape(tracks$ptt))
    }
    if (input$points) {
      proxy %>% addCircles(radius = 20, weight = 10, color = "#cf212e",
                           fillColor = "red", fillOpacity = 2, 
                           label = ~htmlEscape(tracks$date))
    } 
  })
  
}

shinyApp(ui, server)