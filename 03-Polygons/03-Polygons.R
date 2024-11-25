# Required packages
library(tidycensus)
library(tidyverse)
library(sf)
library(viridis)
library(cowplot)
library(biscale)
library(ggtext) 


# Get King County tract-level data
king_county <- get_acs(
  geography = "tract",
  state = "WA",
  county = "King",
  variables = c(
    commute_time = "B08013_001",  
    workers = "B08012_001",       
    public_transit = "B08301_010" 
  ),
  year = 2021,
  geometry = TRUE
) %>%
  select(GEOID, NAME, variable, estimate)

# Reshape and calculate metrics
king_data <- king_county %>%
  spread(variable, estimate) %>%
  mutate(
    avg_commute = commute_time / workers,
    pct_transit = (public_transit / workers) * 100
  )

# Create bivariate classifications
king_data <- bi_class(king_data, 
                      x = avg_commute, 
                      y = pct_transit, 
                      style = "quantile", 
                      dim = 4)  # Using 4x4 grid to match your example

# Create the map
map <- ggplot() +
  geom_sf(data = king_data, 
          aes(fill = bi_class), 
          color = "white", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue2", dim = 4) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_markdown(size = 14)  # Changed to element_markdown
  ) +
  
  # Add lines with different colors
  geom_segment(
    data = data.frame(
      x = c(-122.27, -121.35),
      y = c(47.25, 47.32),
      xend = c(-122.33, -121.4),
      yend = c(47.59, 47.28),
      color = c("#BE64AC", "#5ac8c8")
    ),
    aes(x = x, y = y, xend = xend, yend = yend, color = color),
    size = 0.3
  ) +
  scale_color_identity() +
  geom_segment(
    data = data.frame(
      x = c(-122.27, -121.35),
      y = c(47.25, 47.32),
      xend = c(-122.58, -121),
      yend = c(47.25, 47.32),
      color = c("#BE64AC", "#5ac8c8")
    ),
    aes(x = x, y = y, xend = xend, yend = yend, color = color),
    size = 0.3
  ) +
  
  # Add circles at the end of lines
  geom_point(
    data = data.frame(
      x = c(-122.33, -121.4),
      y = c(47.59, 47.28),
      color = c("#BE64AC", "#5ac8c8")
    ),
    aes(x = x, y = y, color = color),
    size = 2,
    shape = 21,
    fill = "white",
    stroke = 0.5
  ) +
  
  annotate("text", x = -122.4, y = 47.21, 
           label = "High public transit usage,\nLow average commute times",
           size = 4, fontface = "bold", color = "#be64ac") +
  annotate("text", x = -121.2, y = 47.35,
           label = "Low public transit usage,\nHigh average commute time",
           size = 4, fontface = "bold", color = "#5ac8c8") +
  annotate("text", x = -122.8, y = 47.1, 
           label = "#30DayMapChallenge 2024: Day 3-Polygons",
           size = 3, fontface = "bold", color = "grey60",
           hjust = 0) +
  annotate("text", x = -122.8, y = 47.08, 
           label = "#Hao Zhu | linkedin.com/in/haozhu0501",
           size = 3, fontface = "bold", color = "grey60",
           hjust = 0) +
  annotate("text", x = -122.8, y = 47.06, 
           label = "#Source:U.S. Census Bureau, ACS 5-Year Estimates ",
           size = 3, fontface = "bold", color = "grey60",
           hjust = 0) +

   labs(
    title = "Commuting Infrastructure Usage in King County, WA",
    subtitle = "<span style='color:#5ac8c8'>Average Commute Time</span> to Work and <span style='color:#be64ac'>Public Transit Usage</span> in 2021"
  )

# Create legend
legend <- bi_legend(
  pal = "DkBlue2",
  dim = 4,
  xlab = "Average Commute Time (minutes)",
  ylab = "% Using Public Transit",
  size = 10,
  flip_axes = FALSE,
  rotate_pal = FALSE
)

# Combine map and legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.7, 0.05, 0.25, 0.25)

# Save the plot
ggsave("output/Map-03.png", finalPlot, bg = "white", width = 14, height = 8, dpi = 300)



