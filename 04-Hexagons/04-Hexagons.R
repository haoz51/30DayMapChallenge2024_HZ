# Required packages
library(ggplot2)    # For visualization
library(sf)         # For spatial operations
library(hexbin)     # For hexagon binning
library(dplyr)      # For data manipulation
library(tidyr)      # For data cleaning
library(viridis)    # For color schemes
library(maps)       # For base maps
library(ggimage)
library(mapproj)
library(viridisLite) 


ozone_data <- read.csv("data/ad_viz_plotval_data.csv") 

  ozone_clean <- ozone_data %>%
  select(
    site_name = Local.Site.Name,
    latitude = Site.Latitude,
    longitude = Site.Longitude,
    ozone = Daily.Max.8.hour.Ozone.Concentration,
    aqi = Daily.AQI.Value
  ) %>%
  group_by(site_name, latitude, longitude) %>%
  summarize(
    avg_ozone = mean(ozone, na.rm = TRUE),
    max_ozone = max(ozone, na.rm = TRUE),
    avg_aqi = mean(aqi, na.rm = TRUE),
    n_measurements = n()
  )

# Get California state boundary
california <- map_data("state") %>%
  filter(region == "california")

# Choose a color from the palette
annotation_color <- viridis(5, option = "plasma")[2]  # Select the fourth color from the palette

####Mapping

  map <-ggplot() +
  # Add California base map
  geom_polygon(data = california, 
               aes(x = long, y = lat, group = group),
               fill = "white", color = "gray50", alpha = 0.3) +
    annotate(
      "path",
      x = -119 + 2 * cos(seq(0, 2 * pi, length.out = 100)),  # Longitude for circle
      y = 35 + 2 * sin(seq(0, 2 * pi, length.out = 100)),    # Latitude for circle
      color = annotation_color, 
      linetype = "dashed",
      size = 0.8
    ) +
  #geom_image(aes(image = "img/The Aliso Canyon gas leak.webp", x = -116, y = 39), size = 0.25) +
    geom_text(
     aes(x = -126, y = 35.25, label = "Potential areas affected by \nAliso Canyon Gas Leak in 2015"),
     color = annotation_color,
     family = "sans",
     size = 4,  # Adjust size of text
     hjust = 0,
     fontface = "italic" ) +  
  # Add hexagon bins
    geom_text(aes(label = "#30DayMapChallenge 2024: Day 4-Hexagons", x = -126, y = 33.75), size = 3, color = "gray60", family = "serif", hjust = 0) +
    geom_text(aes(label = "Hao Zhu | linkedin.com/in/haozhu0501", x = -126, y = 33.5), size = 3, color = "gray60", family = "serif", hjust = 0) +
    geom_text(aes(label = "Source: EPA, Los Angeles Daily News", x = -126, y = 33.25), size = 3, color = "gray60", family = "serif", hjust = 0) +
  
    stat_summary_hex(
    data = ozone_clean,
    aes(x = longitude, y = latitude, z = avg_ozone),
    fun = mean,
    bins = 20,
    alpha = 0.8
  ) +
  # Customize the appearance
  scale_fill_viridis(
    name = "Average Ozone\n(ppm)",
    option = "plasma",
    breaks = round(seq(0.015, 0.055, length.out = 8), 3),  # Adjust range as per your data
    labels = round(seq(0.015, 0.055, length.out = 8), 3)   # Ensure consistent rounding
  ) +
  # Add labels and title
  labs(
    title = "California Ozone Concentrations in 2015",
    x = "Longitude",
    y = "Latitude"
  ) +
  # Theme customization
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.justification = c(0, 1),
    legend.box.just = "left",
    legend.key.width = unit(2, "cm"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_map()
  
  map
  
  # Save the final map
  ggsave("output/Map-04.png", map, width = 10, height = 8, bg = "white")
  
  
  
  