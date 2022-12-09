# install.packages("leaflet")
library(leaflet)

runs2 <- read.csv("https://raw.githubusercontent.com/amndazhang/project-dynamar-map/master/Processed_GPE3_Tracks_BUM_SAL.csv")
unique_fish <- unique(runs2$species)

m <- leaflet() %>% addProviderTiles("CartoDB.Positron")

for (i in 1:length(unique_fish)) {
  run_species <- unique_fish[i]
  m <- addPolylines(m, lng=runs2[runs2$species == run_species,"lon"],
                    lat=runs2[runs2$species == run_species,"lat"],
                    color = ifelse(run_species=="BUM","blue","red"),
                    weight=1)
  
  m <- addCircles(m, lng=runs2[runs2$species == run_species,"lon"], 
                  lat=runs2[runs2$species == run_species,"lat"], 
                  color = "black", weight = 1, radius = 5)
}

#print(value)

ui <- fluidPage(
  titlePanel("Project DynaMar Map"),

  sidebarLayout(
    
    sidebarPanel(
      checkboxGroupInput("display", "Display:", c("lines", "points")),
      verbatimTextOutput("value")
      #selectInput("tags", "Tag SN: ", unique(runs2$ptt), multiple = TRUE),
      #tableOutput("data"),
      #sliderInput("rng", "Depth Range", value = c(-2000, 0), min = -2000, max = 0)
    ),
    
    mainPanel(
      m
    )
  )
)

server <- function(input, output, session) {
  #output$value <- reactive({input$display})

} 

shinyApp(ui = ui, server = server)
