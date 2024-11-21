# 30 Day Map Challenge: Day 16-Choropleth
# November 16, 2024
# Author: Hao Zhu, haoz51@upenn.edu

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(readr)
library(RColorBrewer)
library(ggrepel)
library(cowplot)

# Step 1: Read the data
internet_data <- read.csv("data/share-of-individuals-using-the-internet (1)/share-of-individuals-using-the-internet.csv")

# Step 2: Get the world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Step 3: Clean and prepare the data
internet_clean <- internet_data %>%
  filter(!is.na(`Individuals.using.the.Internet....of.population.`)) %>%
  select(Entity, time, `Individuals.using.the.Internet....of.population.`)%>%
  rename(pct_internet_pop = "Individuals.using.the.Internet....of.population.")

# Step 4: Merge the map data with internet usage data
world_data <- world %>%
  left_join(internet_clean, by = c("name_ciawf" = "Entity"))

world_data <- world_data %>%
  mutate(usage_category = cut(`pct_internet_pop`,
                              breaks = c(0, 20, 40, 60, 80, 100),
                              labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                              include.lowest = TRUE))


# Identify top 3, bottom 3, and US
top_3 <- internet_clean %>%
  arrange(desc(pct_internet_pop)) %>%
  slice(1:3)

bottom_3 <- internet_clean %>%
  arrange(pct_internet_pop) %>%
  slice(1:3)

us <- internet_clean %>%
  filter(Entity == "United States")

annotations <- bind_rows(top_3, bottom_3, us)

annotation_positions <- data.frame(
  country = c("United States", "South Sudan", "Burundi", "Somalia"),
  latitude = c(37.0902, 6.8770, 	-3.3731, 5.1521),  
  longitude = c(-95.7129, 31.3070,29.9189, 46.1996),  
  label = c("United States:91.8%","South Sudan:6.5%", "Burundi:5.8%", "Somalia:2%")
)

# Customizing the map with points and annotations
internet_map <- ggplot(data = world_data) +
  geom_sf(aes(fill = usage_category), color = "black", size = 0.1) +
  # Add points for country locations
  geom_point(
    data = annotation_positions,
    aes(x = longitude, y = latitude),
    color = "#FA889C",
    size = 3
  ) +
  # Add text annotations with lines
  geom_text_repel(
    data = annotation_positions,
    aes(x = longitude, y = latitude, label = label),
    size = 4,
    color = "white",
    bg.color = "#FA889C",    # The background color of the text (outline effect)
    bg.r = 0.15,              # The radius of the background outline (creates the text outline)
    nudge_x = 20,  # Move text horizontally further from the point
    nudge_y = 3,   # Move text vertically further from the point
    segment.color = "#FA889C",  # Line color connecting the point to the text
    segment.size = 0.3,       # Line thickness
    box.padding = 0.5,        # Padding around the text box
    point.padding = 0.3,      # Padding between the point and the line
    max.overlaps = 100        # Ensure all annotations are shown
  ) +
  scale_fill_manual(
    values = c("#F3EC87", "#A9CA80", "#70A47A", "#4B7C6E", "#375557"),
    name = "Internet Usage (% of Population)",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1
    )
  ) +
  theme_void() +
  labs(
    title = "Internet Usage: A Contrast Between the United States and the Bottom Three Countries",
    subtitle = "Exploring the Gap in Internet Adoption Rates as a Percentage of Population",
    caption = "Data: Our World in Data"
  ) 

internet_map <- internet_map +
  draw_text("#30DayMapChallenge 2024: Day 16 - Choropleth", 
            x = -165, y = -45, size = 10, color = "gray60", hjust = 0, family = "serif") +
  draw_text("Hao Zhu | linkedin.com/in/haozhu0501", 
            x = -165, y = -50, size = 10, color = "gray60", hjust = 0, family = "serif") +
  draw_text("R Libraries: ggplot2, dplyr, rnaturalearth, readr, ggrepel, cowplot", 
            x = -165, y = -55, size = 10, color = "gray60", hjust = 0, family = "serif") +
  draw_text("Source:Our World in Data", 
            x = -165, y = -60, size = 10, color = "gray60", hjust = 0, family = "serif")+
  theme(
    text = element_text(color = "#f5f5f2"),
    plot.background = element_rect(fill = "#22211d", color = NA),
    panel.background = element_rect(fill = "#22211d", color = NA),
    legend.background = element_rect(fill = "#22211d", color = NA),
    plot.title = element_text(
      size = 20, hjust = 0.01, color = "#f5f5f2",
      margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")
    ),
    plot.subtitle = element_text(
      size = 15, hjust = 0.01, color = "#f5f5f2",
      margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")
    ),
    plot.caption = element_text(
      size = 10, color = "#f5f5f2",
      margin = margin(b = 0.3, r = -99, t = 0.3, unit = "cm")
    ),
    legend.position = c(0.2, -0.05),
    legend.text = element_text(size = 9, color = "#f5f5f2"),
    legend.title = element_text(size = 11, face = "bold", color = "#f5f5f2")
  )

  

internet_map
# Save the plot
ggsave("output/Map-16.png", internet_map, width = 14, height = 10, bg = "#22211d")
