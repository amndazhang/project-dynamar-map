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
                checkboxInput("showAll", "Show All", TRUE),
                selectInput("tags", "Tag SN: ", unique(tracks$ptt), multiple = TRUE),
                checkboxInput("points", "Show points", TRUE),
                checkboxInput("lines", "Show paths", TRUE),
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(tracks) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  observe({
    
    if (input$showAll) {
      
      s <- tracks[tracks$ptt, ]$species
      c <- ifelse(s=="BUM", brewer.pal(9, "Blues"), brewer.pal(9, "Reds"))
      output$map <- renderLeaflet({
        leaflet(tracks) %>% addTiles() %>%
          fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%
          addCircles(lng = ~lon, lat = ~lat, weight = 2, radius = 5,
                     label = ~htmlEscape(date)) %>%
          addPolylines(lng = ~lon, lat = ~lat, 
                       weight = 3, 
                       label = ~htmlEscape(species),
                       color = "#ff9632")
      })
      
      # proxy <- leafletProxy("map", tracks)
      # #s <- tracks[tracks$ptt, ]$species
      # proxy %>% addPolylines(lng = ~lon, lat = ~lat, 
      #                        weight = 3, fillOpacity = 2, 
      #                        label = ~htmlEscape(species),
      #                        color = "#ff9632") #ifelse(s=="BUM", "#ff9632", "#d743d9"))
      # proxy %>% addCircles(radius = 20, weight = 10, 
      #                      color = "#ff9632", #ifelse(s=="BUM", "#ff9632", "#d743d9"))
      #                      fillColor = "red", fillOpacity = 1, 
      #                      label = ~htmlEscape(date))
    } 
    # else {
      
    #   filteredData <- reactive({
    #     tracks[tracks$ptt == input$tags,]
    #   })
    #   proxy <- leafletProxy("map", data = filteredData())
    #   proxy %>% clearShapes()
      
    #   s <- tracks[tracks$ptt == input$tags,]$species
    #   if (input$lines) {
    #     proxy %>% addPolylines(lng = ~lon, lat = ~lat, 
    #                            weight = 3, fillOpacity = 2, 
    #                            label = ~htmlEscape(species),
    #                            color = ifelse(s=="BUM", "#ff9632", "#d743d9"))
    #   }
    #   if (input$points) {
    #     proxy %>% addCircles(radius = 20, weight = 10, 
    #                          color = ifelse(s=="BUM", "#ff9632", "#d743d9"),
    #                          fillColor = "red", fillOpacity = 1, 
    #                          label = ~htmlEscape(date))
    #   }   
    # }
  }) 
}

shinyApp(ui, server)