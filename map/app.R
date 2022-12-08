# install.packages("leaflet") # Download and install leaflet package
library(leaflet) # Loads the leaflet library for use

runs2 <- read.csv("https://raw.githubusercontent.com/amndazhang/project-dynamar-map/master/Processed_GPE3_Tracks_BUM_SAL.csv")
unique_fish <- unique(runs2$species)

# Call the leaflet package to setup the maps and add the stops
m <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") # Add the map tiles

 #For each unique run number, map the route

for (i in 1:length(unique_fish)) {
  run_species <- unique_fish[i] # Get the run number.
  m <- addPolylines(m, lng=runs2[runs2$species == run_species,"lon"],
                    lat=runs2[runs2$species == run_species,"lat"],
                    color = ifelse(run_species=="BUM","blue","red"),
                    weight=1)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Project DynaMAR Map"),
  
  # map projection
  m
)

# Define server logic
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
