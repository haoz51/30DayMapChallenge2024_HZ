# 30 Day Map Challenge: Day 15-Data: My Data
# November 15, 2024
# Author: Hao Zhu, haoz51@upenn.edu


library(leaflet)
library(sf)
library(tidyverse)
library(osmdata)
library(htmlwidgets)



locations <- data.frame(
  name = c(
    "Lincoln Memorial", 
    "Washington Monument", 
    "Smithsonian National Museum of Natural History",
    "The White House",
    "National Gallery of Art",
    "Jefferson Memorial",
    "National Portrait Gallery"
  ),
  lat = c(
    38.8893,  # Lincoln Memorial
    38.8895,  # Washington Monument
    38.8913,  # Smithsonian National Museum of Natural History
    38.8977,  # The White House
    38.8913,  # National Gallery of Art
    38.8814,  # Jefferson Memorial
    38.8975   # National Portrait Gallery
  ),
  lon = c(
    -77.0502,  # Lincoln Memorial
    -77.0353,  # Washington Monument
    -77.0261,  # Smithsonian National Museum of Natural History
    -77.0365,  # The White House
    -77.0199,  # National Gallery of Art
    -77.0365,  # Jefferson Memorial
    -77.0228   # National Portrait Gallery
  ),
  day = c(
    "Day 1",  # Lincoln Memorial
    "Day 1",  # Washington Monument
    "Day 2",  # Smithsonian National Museum of Natural History
    "Day 1",  # The White House
    "Day 2",  # National Gallery of Art
    "Day 2",  # Jefferson Memorial
    "Day 2"   # National Portrait Gallery
  )
)
# Define bounding box for the area (approximate for Washington, DC)
bbox <- c(-77.12, 38.85, -76.90, 39.00)

# Add image links for each location
locations$image <- c(
  "https://raw.githubusercontent.com/haoz51/30DayMapChallenge2024_HZ/refs/heads/main/15-Data%20My%20data/img/Lincoln%20Memorial.jpg",      # Lincoln Memorial
  "https://raw.githubusercontent.com/haoz51/30DayMapChallenge2024_HZ/refs/heads/main/15-Data%20My%20data/img/Washington%20Monument.jpg",   # Washington Monument
  "https://raw.githubusercontent.com/haoz51/30DayMapChallenge2024_HZ/refs/heads/main/15-Data%20My%20data/img/Smithsonian%20National%20Museum%20of%20Natural%20History.jpg",# Smithsonian Museum
  "https://raw.githubusercontent.com/haoz51/30DayMapChallenge2024_HZ/refs/heads/main/15-Data%20My%20data/img/The%20White%20House.jpg",           # The White House
  "https://raw.githubusercontent.com/haoz51/30DayMapChallenge2024_HZ/refs/heads/main/15-Data%20My%20data/img/National%20Gallery%20of%20Art.jpg",      # National Gallery of Art
  "https://raw.githubusercontent.com/haoz51/30DayMapChallenge2024_HZ/refs/heads/main/15-Data%20My%20data/img/Jefferson%20Memorial.jpg",    # Jefferson Memorial
  "https://raw.githubusercontent.com/haoz51/30DayMapChallenge2024_HZ/refs/heads/main/15-Data%20My%20data/img/National%20Portrait%20Gallery.jpg" # National Portrait Gallery
)



day2_locations <- locations %>%
  filter(day == "Day 2") %>%
  mutate(order = case_when(
    name == "Smithsonian National Museum of Natural History" ~ 2,
    name == "National Gallery of Art" ~ 3,
    name == "Jefferson Memorial" ~ 4,
    name == "National Portrait Gallery" ~ 1
  )) %>%
  arrange(order)

day1_locations <- locations %>%
  filter(day == "Day 1") %>%
  mutate(order = case_when(
    name == "The White House" ~ 1,
    name == "Washington Monument" ~ 2,
    name == "Lincoln Memorial" ~ 4
  )) %>%
  arrange(order)

# Create a leaflet map
tour_map <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(locations$lon), lat = mean(locations$lat), zoom = 14) %>%
  
  # Add markers for all locations with popup
  addCircleMarkers(
    data = locations,
    lng = locations$lon, 
    lat = locations$lat,
    label = ~name,
    radius = 5,
    color = ~ifelse(day == "Day 1", "blue", "orange"),
    popup = ~ifelse(
      !is.na(image),  # Add image only if the URL exists
      paste0(
        "<strong>", name, "</strong><br>",
        "Day: ", day, "<br>",
        "<img src='", image, "' style='width:200px;height:auto;'><br>"
      ),
      paste0("<strong>", name, "</strong><br>Day: ", day)  # Fallback for locations without images
    )) %>%
  
  # Add walking path for Day 1
  addPolylines(
    lng = day1_locations$lon,
    lat = day1_locations$lat,
    color = "blue",
    weight = 3,
    opacity = 0.8,
    popup = "Walking path for Day 1"
  ) %>%
  
  # Add walking path for Day 2
  addPolylines(
    lng = day2_locations$lon,
    lat = day2_locations$lat,
    color = "orange",
    weight = 3,
    opacity = 0.8,
    popup = "Walking path for Day 2"
  ) %>%
  
  # Add base layers and layer control
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("OpenStreetMap", group = "Street Map") %>%
  addLayersControl(
    baseGroups = c("Street Map", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%

# Add a title
addControl(
  html = "<h2 style='color: darkblue; font-family: Arial; text-align: center;'>Exploring Washington, DC: Places I Visited on Oct. 03-04 During Fall Break</h2>",
  position = "bottomleft"
)%>%
  
addControl(
  html = "<h4 style='color: gray; font-family: Arial; text-align: center;'>
              #30DayMapChallenge 2024: Day 15 - Data:My Data<br>
              Hao Zhu | linkedin.com/in/haozhu0501<br>
              R Libraries:leaflet, sf, tidyverse, osmdata
            </h4>",
  position = "bottomleft"
)

# Display the map
tour_map

# Save the Leaflet map as an HTML file

saveWidget(tour_map, file = "output/map_15.html", selfcontained = TRUE)

