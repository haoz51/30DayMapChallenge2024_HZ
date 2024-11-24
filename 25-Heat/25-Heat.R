
# Load libraries
library(rnaturalearth)
library(mapsf)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tmap)
library(cartography)


world_sf <- ne_countries(scale = "medium", returnclass = "sf")
# Read the CSV data
wildfire_data <- read.csv("data/annual-area-burnt-per-wildfire/annual-area-burnt-per-wildfire.csv")

# Filter for 2024 data and remove aggregated regions
wildfire_2024 <- wildfire_data %>%
  filter(Year == 2024) %>%
  filter(!Entity %in% c("World", "Africa", "Asia", "Europe", 
                        "North America", "South America", "Oceania"))

# Remove rows with NA values
wildfire_2024 <- na.omit(wildfire_2024)


world_data <- world_sf %>%
  left_join(wildfire_2024, by = c("iso_a3" = "Code"))

# Set bounding box for Africa
africa_bbox <- st_bbox(c(xmin = -10, xmax = 52, ymin = -37, ymax = 38))


# Filter valid data with correct coordinates and within Africa bounds
valid_data <- world_data %>%
  filter(
    !is.na(Annual.area.burnt.per.wildfire), # Non-missing wildfire data
    !is.na(st_coordinates(st_centroid(geometry))[, 1]), # Non-missing longitude
    !is.na(st_coordinates(st_centroid(geometry))[, 2]), # Non-missing latitude
    st_coordinates(st_centroid(geometry))[, 1] >= africa_bbox["xmin"] &
      st_coordinates(st_centroid(geometry))[, 1] <= africa_bbox["xmax"] &
      st_coordinates(st_centroid(geometry))[, 2] >= africa_bbox["ymin"] &
      st_coordinates(st_centroid(geometry))[, 2] <= africa_bbox["ymax"]
  )

# Extract longitude and latitude for plotting
centroid_coords <- st_coordinates(st_centroid(valid_data$geometry))
valid_data$longitude <- centroid_coords[, 1]
valid_data$latitude <- centroid_coords[, 2]

# Set up the PNG device
png(filename = "output/Map-25.png",
    width = 1200,
    height = 1000,
    res = 150)

# Set margins and background
par(mar = c(4, 4, 4, 2), bg = "#5B7A99")

# Plot background shadow for aesthetics
plot(st_geometry(world_sf) + c(1, -1), # Offset the shadow
     col = "grey30",  # Dark shadow color
     border = NA,     # No border for shadow
     xlim = c(africa_bbox["xmin"], africa_bbox["xmax"]),
     ylim = c(africa_bbox["ymin"], africa_bbox["ymax"]))

# Plot actual land on top
plot(st_geometry(world_sf), 
     col = "grey90",  # Light grey for land
     border = "white",
     xlim = c(africa_bbox["xmin"], africa_bbox["xmax"]),
     ylim = c(africa_bbox["ymin"], africa_bbox["ymax"]),
     add = TRUE)      # Add to existing plot

# Add title with left alignment
title("Area Burnt by Wildfire in Africa, 2024",
      col.main = "#E48959",
      adj = 0,  # Left alignment
      cex.main = 1.5)

# Define legend values
legend_values <- c(1000, 5000, 10000, 20000, 40000)
max_legend_value <- max(legend_values)

# Calculate circle sizes
max_circle_size <- 0.3  
circle_sizes <- sqrt(valid_data$Annual.area.burnt.per.wildfire / max_legend_value) * max_circle_size

# Plot proportional circles
symbols(
  x = valid_data$longitude,
  y = valid_data$latitude,
  circles = circle_sizes,
  inches = max_circle_size,
  bg = "#E48959",
  fg = "black",
  add = TRUE
)

# Add legend
legendCirclesSymbols(
  pos = "bottomleft",
  title.txt = "Area Burnt in hectares",
  title.cex = 0.8,
  cex = 1,
  border = "black",
  lwd = 0.5,
  values.cex = 0.7,
  var = legend_values,
  inches = max_circle_size * sqrt(legend_values / max_legend_value),
  col = "#E48959",
  frame = FALSE,
  values.rnd = 0,
  style = "c"
)

# Add an image/logo
logo <- png::readPNG("img/forest fire.png")  # Replace with your image path
x_img <- africa_bbox["xmax"] - 5  # X position for image
y_img <- africa_bbox["ymin"] + 2   # Y position for image

# Add the image
rasterImage(logo,
            xleft = x_img, 
            ybottom = y_img,
            xright = x_img + 20,  # Adjust width
            ytop = y_img + 20,    # Adjust height
            interpolate = TRUE)

# Add annotations
x_pos <- africa_bbox["xmax"] + 10
y_start <- africa_bbox["ymin"] - 1.5

text(x_pos, y_start + 4, 
     "#30DayMapChallenge 2024: Day 25-Heat", 
     cex = 0.8, 
     col = "white", 
     family = "serif", 
     adj = 1)

text(x_pos, y_start + 2, 
     "Hao Zhu | linkedin.com/in/haozhu0501", 
     cex = 0.8, 
     col = "white", 
     family = "serif", 
     adj = 1)

text(x_pos, y_start, 
     "Source: Global Wildfire Information System", 
     cex = 0.8, 
     col = "white", 
     family = "serif", 
     adj = 1)

# Close the PNG device
dev.off()