# Required libraries
library(tidycensus)
library(tidyverse)
library(sf)
library(patchwork)

# Get education data
mi_education <- get_acs(
  geography = "county",
  variables = c(
    "B15003_022", # Bachelor's degree
    "B15003_001"  # Total population 25 years and over
  ),
  state = "MI",
  year = 2021,
  geometry = TRUE
) %>%
  select(-moe) %>%
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>%
  rename(
    bachelors = B15003_022,
    total = B15003_001
  ) %>%
  mutate(
    pct_bachelors = (bachelors / total) * 100,
    above_median_edu = pct_bachelors >= median(pct_bachelors)
  )

# Get income data
mi_income <- get_acs(
  geography = "county",
  variables = "B19013_001", # Median household income
  state = "MI",
  year = 2021,
  geometry = TRUE
) %>%
  select(-moe) %>%
  mutate(above_median_inc = estimate >= median(estimate))

# Calculate medians for legends
median_edu <- median(mi_education$pct_bachelors, na.rm = TRUE)
median_inc <- median(mi_income$estimate, na.rm = TRUE)

# Create education map
edu_map <- ggplot(mi_education) +
  geom_sf(aes(fill = above_median_edu), color = "#2b2b2b", size = 0.1) +
  scale_fill_manual(
    values = c("#2b2b2b", "#f5f5f5"),
    labels = c(
      paste0("Below median (≤", round(median_edu, 1), "%)"),
      paste0("Above median (>", round(median_edu, 1), "%)")
    ),
    name = "Educational Achievement"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(color = "#2b2b2b"),
    legend.title = element_text(color = "#2b2b2b", , face = "bold",size = 14),
    plot.title = element_text(color = "#2b2b2b", hjust = 0.5, size = 10),
    plot.subtitle = element_text(color = "#2b2b2b", hjust = 0.5, size = 8),
    # Removed background rectangles
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank()
  ) #+
 # labs(
  #  title = "Educational Achievement",
 #   subtitle = "Bachelor's Degree Attainment\n(Population 25 years and over)"
 # )

# Create income map
inc_map <- ggplot(mi_income) +
  geom_sf(aes(fill = above_median_inc), color = "#2b2b2b", size = 0.1) +
  scale_fill_manual(
    values = c("#2b2b2b", "#f5f5f5"),
    labels = c(
      paste0("Below median (≤$", round(median_inc/1000, 0), "K)"),
      paste0("Above median (>$", round(median_inc/1000, 0), "K)")
    ),
    name = "Household Income"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(color = "#2b2b2b"),
    legend.title = element_text(color = "#2b2b2b",, face = "bold", size = 14),
    plot.title = element_text(color = "#2b2b2b", hjust = 0.5, size = 10),
    plot.subtitle = element_text(color = "#2b2b2b", hjust = 0.5, size = 8),
    # Removed background rectangles
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank()
  ) #+
 # labs(
   # title = "Household Income",
   # subtitle = "Median Household Income\n(Past 12 months)"
 # )
  

# Combine maps using patchwork
combined_map <- edu_map + inc_map +
  plot_annotation(
    title = "Hand in Hand: Where Education and Income Follow Similar Paths in Michigan Counties",
    subtitle = "#30DayMapChallenge 2024: Day 3-Polygons | Hao Zhu | linkedin.com/in/haozhu0501| Source:  ACS 5-Year Estimates, 2021 ",
    theme = theme(
      plot.title = element_text(color = "#2b2b2b", face = "bold", hjust = 0, size = 20),
      plot.subtitle = element_text(color = "#2b2b2b", hjust = 0, size = 10),
      plot.caption = element_text(color = "#2b2b2b", hjust = 0),
      plot.background = element_blank()
    )
  )

# Save the combined map with transparent background
ggsave("output/Map-22.png", 
       plot = combined_map,
       width = 15, 
       height = 8, 
       dpi = 300,
       bg = "white")  # Changed background to transparent