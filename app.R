library(leaflet) # Loads the leaflet library for use
runs <- read.csv("https://raw.githubusercontent.com/amndazhang/project-dynamar-map/master/Processed_GPE3_Tracks_BUM_SAL.csv") # Read in the run data

species_list <- runs$species
unique_tags <- unique(runs$ptt) # list of unique tags
unique_species <- vector(mode = "list", length = 0)
for (tag in unique_tags) { # going through unique ptts
  unique_species <- append(unique_species, species_list[match(tag,unique_tags)])
}

m <- leaflet() %>%
  addProviderTiles("CartoDB.Positron")
# For each unique run number, map the route
unique_species
for (s in unique_species) {
  m <- addPolylines(m, lng=~lon, lat=~lat,
                    color = ifelse(s=="BUM","blue","red"))
}
m  # Print the map