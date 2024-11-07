# 30 Day Map Challenge: Day 1-point
# November 1, 2024
# Author: Hao Zhu, haoz51@upenn.edu


library(ggplot2)
library(dplyr)
library(ggimage)
library(stringr)

# Data frame for pandas
pandas <- data.frame(
  zoo = c("San Diego Zoo", "Smithsonian's National Zoo"),
  city = c("San Diego", "Washington"),
  state = c("CA", "DC"),
  lat = c(32.7353, 38.9296),
  lon = c(-117.1494, -77.0493),
  num_pandas = c(2, 2)
)

# Mapping
ggplot() +
  borders("state", colour = "gray80", fill = "gray90") +
  geom_point(data = pandas, aes(x = lon, y = lat, size = num_pandas),
             color = "darkgreen", alpha = 0.8) +
  geom_text(aes(x = -117, y = 31, label = "San Diego Zoo"), color = "black", family = "mono", size = 5, hjust = 0.5, fontface = "bold") +
  geom_text(aes(x = -77, y = 41, label = "Smithsonian's National Zoo"), color = "black", family = "mono", size = 5, hjust = 0.5, fontface = "bold") +
  geom_image(aes(image = "img/XinBao-SD.png", x = -121, y = 42), size = 0.15) +
  geom_image(aes(image = "img/YunChuan-SD.png", x = -110, y = 42), size = 0.16) +
  geom_image(aes(image = "img/BaoLi-SNZ.png", x = -80, y = 34), size = 0.35) +
  geom_image(aes(image = "img/QingBao-SNZ.png", x = -92, y = 34), size = 0.35) +
  geom_text(aes(x = -121, y = 38, label = "Xin Bao"), color = "darkgreen", family = "mono", size = 4, hjust = 0.5, fontface = "bold") +
  geom_text(aes(x = -110, y = 38, label = "Yun Chuan"), color = "darkgreen", family = "mono", size = 4, hjust = 0.5, fontface = "bold") +
  geom_text(aes(x = -77, y = 29, label = "Bao Li"), color = "darkgreen", family = "mono", size = 4, hjust = 0.5, fontface = "bold") +
  geom_text(aes(x = -88, y = 29, label = "Qing Bao"), color = "darkgreen", family = "mono", size = 4, hjust = 0.5, fontface = "bold") +
  geom_text(aes(x = -77, y = 27,label = "宝力 (BOW-lee)\nBorn in 2021\nMale"),color = "olivedrab", family = "serif", size = 3, hjust = 0.5) +
  geom_text(aes(x = -88, y = 27,label = "青宝 (ching-BOW)\nBorn in 2021\nFemale"),color = "olivedrab", family = "serif", size = 3, hjust = 0.5) +
  geom_text(aes(x = -110, y = 36,label = "云川 (yoon chu-an)\nBorn in 2007\nMale"),color = "olivedrab", family = "serif", size = 3, hjust = 0.5) +
  geom_text(aes(x = -121, y = 36,label = "鑫宝 (sing-Bow)\nBorn in 2008\nFemale"),color = "olivedrab", family = "serif", size = 3, hjust = 0.5) +
  
  theme_void() +

  geom_text(aes(label = str_wrap("Where to visit Pandas in U.S.", 38), x = -130, y = 57), size = 10, color = "darkgreen", family = "mono", hjust = 0, fontface = "bold") +
  geom_text(aes(label = "Currently only 4 pandas in the United States, housed in two zoos", x = -130, y = 55), size = 5, color = "black", family = "serif", hjust = 0) +
  geom_text(aes(label = "#30DayMapChallenge 2024: Day 1-Point", x = -130, y = 15), size = 3, color = "gray60", family = "serif", hjust = 0) +
  geom_text(aes(label = "Hao Zhu | linkedin.com/in/haozhu0501", x = -130, y = 14), size = 3, color = "gray60", family = "serif", hjust = 0) +
  geom_text(aes(label = "R Libraries: ggplot2, dplyr, ggimage, stringr ", x = -130, y = 13), size = 3, color = "gray60", family = "serif", hjust = 0) +
  geom_text(aes(label = "Source: Google, San Diego Zoo Wildlife Alliance, Smithsonian's National Zoo & conservation Biology Institute", x = -130, y = 12), size = 3, color = "gray60", family = "serif", hjust = 0) +
  theme_void() +
  theme(
    legend.position = "none")

# Saving the Map
ggsave("output/Map-01.png", width = 10, height = 8)


