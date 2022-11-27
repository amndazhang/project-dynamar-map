#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages("leaflet") # Download and install leaflet package
library(leaflet) # Loads the leaflet library for use

runs <- read.csv("https://raw.githubusercontent.com/mlayton20/dataanalysis/master/my-runs.csv") # Read in the run data
unique_runs <- unique(runs$run) # Store the unqiue runs.

# Call the leaflet package to setup the maps and add the stops
m <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") # Add the map tiles

# For each unique run number, map the route
for (i in 1:length(unique_runs)) {
  run_number <- unique_runs[i] # Get the run number.
  m <- addPolylines(m, lng=runs[runs$run == run_number,"longitude"],
                    lat=runs[runs$run == run_number,"latitude"],
                    color = ifelse(run_number==1,"blue","red"))
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
