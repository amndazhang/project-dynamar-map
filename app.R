library(shiny)
library(leaflet)
library(htmltools)
library(RColorBrewer)

tracks <- read.csv("https://raw.githubusercontent.com/amndazhang/project-dynamar-map/master/Processed_GPE3_Tracks_BUM_SAL.csv")
unique_fish <- unique(tracks$species)

map <- leaflet() %>% addProviderTiles("CartoDB.Positron")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(tracks) %>% addTiles() %>%
    fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%
    # Overlay groups
    addCircles(lng = ~lon, lat = ~lat, weight = 2, radius = 5,
               label = ~htmlEscape(date), group = "Points") %>%
    addPolylines(lng = ~lon, lat = ~lat, weight = 2, group = "Outline") %>%
    # Layers control
    addLayersControl(
      baseGroups = c("Show All", "Show Specific"),
      overlayGroups = c("Points", "Outline"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend("bottomright", 
              colors = c("#ff4d00",  "#0033ff"),
              labels = c("SAL", "BUM"),
              title = "Species",
              opacity = 1)
  })

  observe({
  }) 
  
}

shinyApp(ui, server)