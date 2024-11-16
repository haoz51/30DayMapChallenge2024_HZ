library(leaflet)
library(sf)
library(tidyverse)
library(osmdata)

locations <- data.frame(
  name = c("Lincoln Memorial", "Washington Monument", "National Air and Space Museum", "Capitol Building"),
  lat = c(38.8893, 38.8895, 38.8881, 38.8899),
  lon = c(-77.0502, -77.0353, -77.0199, -77.0091))

# Define bounding box for the area (approximate for Washington, DC)
bbox <- c(-77.12, 38.85, -76.90, 39.00)

# Get data on footpaths and pedestrian ways
walking_paths <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway", value = c("footway", "pedestrian")) %>%
  osmdata_sf()

# Initialize a leaflet map
tour_map <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(locations$lon), lat = mean(locations$lat), zoom = 14)

# Add markers for each stop
tour_map <- tour_map %>%
  addCircleMarkers(
    lng = locations$lon, 
    lat = locations$lat,
    label = locations$name,  # use locations$name directly without the tilde
    radius = 5,
    color = "blue",
    popup = locations$name  # directly reference locations$name
  )


# Add walking paths to the map (if downloaded)
if (!is.null(walking_paths$osm_lines)) {
  tour_map <- tour_map %>%
    addPolylines(data = walking_paths$osm_lines, color = "gray", weight = 2)
}

# Display the map
tour_map

tour_map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("OpenStreetMap", group = "Street Map") %>%
  addProviderTiles("Stamen.Toner", group = "Toner Map") %>%
  addLayersControl(
    baseGroups = c("Street Map", "Toner Map", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addCircleMarkers(lng = locations$lon, lat = locations$lat, label = locations$name, radius = 5, color = "blue", popup = locations$name)



