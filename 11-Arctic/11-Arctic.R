# 30 Day Map Challenge: Day 11-Arctic
# November 11, 2024
# Author: Hao Zhu, haoz51@upenn.edu

# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)
library(ggOceanMaps)
library(patchwork)
library(ggtext)
library(cowplot)


# Load and process polar bear data
polar_bear_data <- read.csv("data/polarBear_distribution_1985_2016/polarBear_satelliteTelemetry_kernelUD_beaufortChukchi_Durner_1985_2016.csv")
polar_bear_data <- polar_bear_data %>%
  filter(!is.na(latitude_ud), !is.na(longitude_ud))

polar_bear_sf <- st_as_sf(polar_bear_data, coords = c("longitude_ud", "latitude_ud"), crs = 4326) %>%
  mutate(period = case_when(
    period == 8595 ~ "1985-1995",
    period == 9606 ~ "1996-2006",
    period == 716  ~ "2007-2016",
    TRUE ~ as.character(period)
  ))

# Define country and sea annotation points
annotation_points <- data.frame(
  name = c("GREENLAND", "CANADA", "RUSSIA"),
  lon = c(-40, -110, 110),
  lat = c(70, 65, 68)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(3995))

annotation_point_sea <- data.frame(
  name = c("Beaufort Sea", "Chukchi Sea", "Norwegian Sea", "Arctic Ocean"),
  lon = c(-140, -168, 20, 90),
  lat = c(74, 70, 73, 90)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(3995))


# Create full Arctic map
full_map <- basemap(limits = 60, bathy.style = "rcb") +
  geom_sf(fill = "grey90", color = "grey30") + 
  geom_sf(data = polar_bear_sf, aes(color = factor(period)), size = 0.5, alpha = 0.5) +
  geom_sf_text(data = annotation_points, aes(label = name), size = 5, color = "black", family = "mono", fontface = "bold") +
  geom_sf_text(data = annotation_point_sea, aes(label = name), size = 3, color = "royalblue4", family = "mono", fontface = "bold") +
  theme(legend.position = "none",
        plot.title = element_text(size = 26, face = "bold"), 
        plot.subtitle = element_markdown(size = 14))+        
  scale_color_manual(values = c("1985-1995" = "rosybrown1", "1996-2006" = "rosybrown3", "2007-2016" = "rosybrown4"), name = "Tracking Period") +
  labs(title = "Polar Bear Habitat Distribution in the Arctic",
       subtitle = "Overview of Tracking Data in the Beaufort and Chukchi Seas (<span style='color:rosybrown1;'>**1985-1995**</span>, <span style='color:rosybrown3;'>**1996-2006**</span>, <span style='color:rosybrown4;'>**2007-2016**</span>)")

# Create zoomed-in map
zoomed_map <- basemap(limits = c(-2e6, 1e6, 0, 3e6), shapefiles = "Arctic", bathy.style = "rcb") +
  geom_sf(data = polar_bear_sf, aes(color = factor(period)), size = 1.5, alpha = 0.5) +
  theme(legend.position = "none",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  scale_color_manual(values = c("1985-1995" = "rosybrown1", "1996-2006" = "rosybrown3", "2007-2016" = "rosybrown4"), name = "Tracking Period")



# Combine maps side-by-side using patchwork
combined_map <- full_map | zoomed_map # Arrange horizontally

logo_path <- "img/polarbear.png" 

# Adding polar bear image 
combined_map_with_image <- ggdraw(combined_map) +
  draw_image(
    logo_path,                                 
    x = 0.82, y = 0.05,                       
    width = 0.2, height = 0.2                  
  )+
  draw_text("#30DayMapChallenge 2024: Day 11 - Arctic", x = 0.05, y = 0.1, size = 10, color = "gray60", hjust = 0, family = "serif") +
  draw_text("Hao Zhu | linkedin.com/in/haozhu0501", x = 0.05, y = 0.07, size = 10, color = "gray60", hjust = 0, family = "serif") +
  draw_text("R Libraries: ggplot2, dplyr, sf, ggOceanMaps, patchwork, ggtext", x = 0.05, y = 0.04, size = 10, color = "gray60", hjust = 0, family = "serif") +
  draw_text("Source: U.S. Geological Survey data, stickpng", x = 0.05, y = 0.01, size = 10, color = "gray60", hjust = 0, family = "serif")


print(combined_map_with_image)

# Save the final map
ggsave("output/Map-011.png", combined_map_with_image, width = 14, height = 10)



