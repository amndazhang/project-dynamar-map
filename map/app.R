#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)

# airport locations
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
# flights between airports
flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
flights$id <- seq_len(nrow(flights))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Project DynaMAR Map"),

    # map projection
    fig
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # map projection
    geo <- list(
      scope = 'north america',
      projection = list(type = 'azimuthal equal area'),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray80")
    )
    
    fig <- plot_geo(locationmode = 'USA-states', color = I("red"))
    fig <- fig %>% add_markers(
      data = air, x = ~long, y = ~lat, text = ~airport,
      size = ~cnt, hoverinfo = "text", alpha = 0.5
    )
    fig <- fig %>% add_segments(
      data = group_by(flights, id),
      x = ~start_lon, xend = ~end_lon,
      y = ~start_lat, yend = ~end_lat,
      alpha = 0.3, size = I(1), hoverinfo = "none"
    )
    fig <- fig %>% layout(
      title = 'Feb. 2011 American Airline flight paths<br>(Hover for airport names)',
      geo = geo, showlegend = FALSE, height=800
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
