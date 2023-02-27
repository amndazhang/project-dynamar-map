library(shiny)
library(leaflet)
library(htmltools)
library(RColorBrewer)

tracks <- read.csv("https://raw.githubusercontent.com/amndazhang/project-dynamar-map/master/Processed_GPE3_Tracks_BUM_SAL.csv")

map <- leaflet() %>% addProviderTiles("CartoDB.Positron")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("tags", "Tag SN: ", unique(tracks$ptt), multiple = TRUE))
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(tracks) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%
      # Overlay groups
      #addCircles(lng = ~lon, lat = ~lat, weight = 2, radius = 5,
      #           label = ~htmlEscape(date), group = "Show All") %>%
      #addPolylines(lng = ~lon, lat = ~lat, weight = 2, group = "Show All") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Show All", "Show Specific"),
        overlayGroups = c("Points", "Outline"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend("bottomright", 
                colors = c("#ff4d00",  "#0033ff"),
                labels = c("SAILFISH", "BLUE MARLIN"),
                title = "Species",
                opacity = 1)
  })
  
  observe({
    if (!is.null(input$tags)){
      filteredData <- reactive({tracks[tracks$ptt == input$tags,]})
      s <- tracks[tracks$ptt == input$tags,]$species
      leafletProxy("map", data = filteredData()) %>% 
        #clearShapes() %>% 
        addCircles(lng = ~lon, lat = ~lat, weight = 3, radius = 100,
                   label = ~htmlEscape(date), color = ifelse(s=="BUM", "blue", "red")) %>% 
        addPolylines(lng = ~lon, lat = ~lat, weight = 2, 
                     label = ~htmlEscape(species), color = ifelse(s=="BUM", "blue", "red"))
    }
  })
  
}

shinyApp(ui, server)