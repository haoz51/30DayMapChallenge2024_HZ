# Install and load required packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggrepel)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a dataframe of regions you mapped
my_maps <- data.frame(
  country = c("United States", "Greenland", "Russia", "Canada", # Arctic map
              "United States", # Pandas, DC, King County, California
              "Somalia", "South Sudan", "Burundi", # Internet usage
              "Africa"), # All of Africa for wildfires
  category = c(rep("Arctic Study", 4),
               rep("US Studies", 1),
               rep("Internet Study", 3),
               "Wildfire Study"),
  details = c(rep("Polar Bear Habitat", 4),
              "Pandas, Urban Studies, Air Quality",
              rep("Digital Divide Analysis", 3),
              "Fire Impact Analysis")
)

# Add Africa as a continent
africa_countries <- world %>%
  filter(continent == "Africa")

# Join with spatial data
mapped_countries <- world %>%
  left_join(my_maps, by = c("name_long" = "country"))

# For Africa, update all African countries to have the wildfire category
mapped_countries$category[mapped_countries$continent == "Africa"] <- "Wildfire Study"
mapped_countries$details[mapped_countries$continent == "Africa"] <- "Fire Impact Analysis"

# Create the map
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +
  geom_sf(data = mapped_countries %>% filter(!is.na(category)), 
          aes(fill = category), color = "white") +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  labs(title = "My 30 Day Map Challenge Journey",
       subtitle = "Regions and Themes Explored in 2024",
       caption = "#30DayMapChallenge 2024: Day 30 - Final Map\nHao Zhu | linkedin.com/in/haozhu0501",
       fill = "Map Themes") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 26, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 8, color = "gray50")) +
  coord_sf(expand = FALSE)

# Save the map
ggsave("30-The final map/output/Map-30.png", bg="white",width = 12, height = 8, dpi = 300)
