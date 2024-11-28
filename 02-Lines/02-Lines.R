# Load necessary libraries
library(sf)
library(ggplot2)
library(osmdata)
library(ggspatial)

# Define the center point and increased radius for more zoom out
center_lon <- -77.06
center_lat <- 38.89
radius <- 0.025  # Increased radius for more zoom out

# Create a perfect circular boundary
theta <- seq(0, 2*pi, length.out = 1000)
# Calculate the latitude correction factor based on the center latitude
lat_correction <- cos(center_lat * pi/180)
circle <- data.frame(
  lon = center_lon + radius * cos(theta) / lat_correction,
  lat = center_lat + radius * sin(theta)
)
circle_sf <- st_as_sf(circle, coords = c("lon", "lat"), crs = 4326) %>%
  st_combine() %>%
  st_cast("POLYGON")

# Get map features with larger bounding box
buffer <- 0.005
dc_bbox <- c(
  center_lon - (radius/lat_correction + buffer),
  center_lat - (radius + buffer),
  center_lon + (radius/lat_correction + buffer),
  center_lat + (radius + buffer)
)

# Get map features
dc_water <- opq(bbox = dc_bbox) %>%
  add_osm_feature(key = 'water') %>%
  osmdata_sf()

dc_streets <- opq(bbox = dc_bbox) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

# Create a slightly larger circle for clipping to avoid edge effects
clip_radius <- radius * 1.01
clip_circle <- data.frame(
  lon = center_lon + clip_radius * cos(theta) / lat_correction,
  lat = center_lat + clip_radius * sin(theta)
)
clip_circle_sf <- st_as_sf(clip_circle, coords = c("lon", "lat"), crs = 4326) %>%
  st_combine() %>%
  st_cast("POLYGON")

# Clip all features to the larger circle
if (!is.null(dc_streets$osm_lines)) {
  dc_streets$osm_lines <- st_intersection(dc_streets$osm_lines, clip_circle_sf)
}
if (!is.null(dc_water$osm_multipolygons)) {
  dc_water$osm_multipolygons <- st_intersection(dc_water$osm_multipolygons, clip_circle_sf)
}

# Create the map
ggplot() +
  # Add streets
  {if (!is.null(dc_streets$osm_lines))
    geom_sf(data = dc_streets$osm_lines, color = "mediumblue", size = 0.1)} +
  
  # Add water features
  {if (!is.null(dc_water$osm_multipolygons))
    geom_sf(data = dc_water$osm_multipolygons, fill = "mediumblue", color = NA)} +
  
  # Add circle outline
  geom_sf(data = circle_sf, fill = NA, color = NA, size = 0.3) +
  
  # Add map elements
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(
    location = "bl",
    bar_cols = c("gray30", "white"),
    text_family = "sans",
    style = "ticks"
  ) +
  
  # Add labels
  labs(
    title = "Potomac River Area",
    subtitle = "Washington, D.C.",
    caption = "#30DayMapChallenge 2024: Day 02: Lines | Hao Zhu | linkedin.com/in/haozhu0501| Data: OpenStreetMap"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = 80, r = 20, b = 20, l = 20, unit = "pt"),
    plot.title = element_text(size = 36, color = "mediumblue", face = "bold", margin = margin(b = 10), hjust = 0),
    plot.subtitle = element_text(size = 16, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 8, color = "gray50"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  # Use equal aspect ratio with adjusted coordinates
  coord_sf(
    crs = st_crs(paste0("+proj=ortho +lat_0=", center_lat, " +lon_0=", center_lon)),
    xlim = c(-radius * 111000, radius * 111000),
    ylim = c(-radius * 111000, radius * 111000),
    expand = FALSE,
    clip = "on"
  )

# Save the map with perfect 1:1 aspect ratio
ggsave("output/Map-02.png", 
       bg = "white", 
       width = 10,
       height = 10,
       dpi = 300,
       units = "in")

