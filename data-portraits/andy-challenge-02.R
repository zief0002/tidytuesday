## DATA PORTRAITS - CHALLENGE 2
## ICD TIDY TUESDAY
## MARCH 2021



# Load library
library(tidyverse)
library(showtext) #To use googlefonts
library(patchwork)



# Import data
conjugal <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/conjugal.csv')

# Convert to long format
conjugal = conjugal %>%
  pivot_longer(3:5, names_to = "Status", values_to = "Percentage") %>%
  # Re-order factor levels to correspond to plot
  mutate(
    Population = factor(Population, levels = c("Negroes", "Germany"))
  ) %>% 
  # Compute x-coordinates for text in bars
  group_by(Population, Age) %>% 
  mutate(
    half_perc = 0.5*Percentage,
    x_coord = half_perc + lag(Percentage, 2, default = 0) + lag(Percentage, 1, default = 0),
    perc_text = paste0(Percentage, "%")
  )


## Loading Google fonts (https://fonts.google.com/)
font_add_google(name = "Cutive Mono")



## Automatically use showtext to render google fonts
showtext_auto()



# Create plot for 15-40 age group
p1 = conjugal %>%
  filter(Age == "15-40") %>%
ggplot(aes(x = Percentage, y = Population, fill = Status)) +
  geom_bar(stat = "identity", width = 0.75)  +
  geom_text(aes(x = x_coord, label = perc_text), size = 2) +
  theme_light() +
  scale_x_continuous(name = "", breaks = NULL) +
  scale_y_discrete(name = "") +
  scale_fill_manual(name = "", values = c("#707a6a", "#e6b329", "#be324b")) +
  ggtitle("CONJUGAL CONDITION") +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "#dfd2c1"),
    legend.key = element_rect(fill = "#dfd2c1"),
    plot.background = element_rect(fill = "#dfd2c1"),
    panel.background = element_rect(fill = "#dfd2c1"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(family = "Cutive Mono"),
    legend.text = element_text(family = "Cutive Mono"),
    plot.title = element_text(hjust = 0.5, family = "Cutive Mono", face = "bold", size = 14),
    plot.margin = margin(15, 0, 0, 50, "pt"), #trbl
    strip.background = element_blank(),
    strip.text = element_blank(),
    aspect.ratio = .1
  ) + 
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  annotate("text", x = -26, y = "Germany", label = c("Age\n15-40"), vjust = 1, family = "Cutive Mono", size = 3) +
  annotate("text", x = -20, y = "Germany", label = "{", vjust = 0.9, family = "Cutive Mono", size = 12, alpha = 0.6)
  

# Create plot for 40-60 age group
p2 = conjugal %>%
  filter(Age == "40-60") %>%
  ggplot(aes(x = Percentage, y = Population, fill = Status)) +
  geom_bar(stat = "identity", width = 0.75)  +
  geom_text(aes(x = x_coord, label = perc_text), size = 2) +
  theme_light() +
  scale_x_continuous(name = "", breaks = NULL) +
  scale_y_discrete(name = "") +
  scale_fill_manual(name = "", values = c("#707a6a", "#e6b329", "#be324b")) +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "#dfd2c1"),
    legend.key = element_rect(fill = "#dfd2c1"),
    plot.background = element_rect(fill = "#dfd2c1"),
    panel.background = element_rect(fill = "#dfd2c1"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(family = "Cutive Mono"),
    legend.text = element_text(family = "Cutive Mono"),
    plot.title = element_text(hjust = 0.5, family = "Cutive Mono", face = "bold", size = 14),
    plot.margin = margin(15, 0, 0, 50, "pt"), #trbl
    strip.background = element_blank(),
    strip.text = element_blank(),
    aspect.ratio = .1
  ) + 
  guides(fill = FALSE) +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  annotate("text", x = -26, y = "Germany", label = c("\n40-60"), vjust = 0.6, family = "Cutive Mono", size = 3) +
  annotate("text", x = -20, y = "Germany", label = "{", vjust = 0.9, family = "Cutive Mono", size = 12, alpha = 0.6)


# Create plot for 60 and over age group
p3 = conjugal %>%
  filter(Age == "60 and over") %>%
  ggplot(aes(x = Percentage, y = Population, fill = Status)) +
  geom_bar(stat = "identity", width = 0.75)  +
  geom_text(aes(x = x_coord, label = perc_text), size = 2) +
  theme_light() +
  scale_x_continuous(name = "", breaks = NULL) +
  scale_y_discrete(name = "") +
  scale_fill_manual(name = "", values = c("#707a6a", "#e6b329", "#be324b")) +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "#dfd2c1"),
    legend.key = element_rect(fill = "#dfd2c1"),
    plot.background = element_rect(fill = "#dfd2c1"),
    panel.background = element_rect(fill = "#dfd2c1"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(family = "Cutive Mono"),
    legend.text = element_text(family = "Cutive Mono"),
    plot.title = element_text(hjust = 0.5, family = "Cutive Mono", face = "bold", size = 14),
    plot.margin = margin(15, 0, 0, 50, "pt"), #trbl
    strip.background = element_blank(),
    strip.text = element_blank(),
    aspect.ratio = .1
  ) + 
  guides(fill = FALSE) +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  annotate("text", x = -26, y = "Germany", label = c("60\nAND\nOVER"), vjust = 0.8, family = "Cutive Mono", size = 3) +
  annotate("text", x = -20, y = "Germany", label = "{", vjust = 0.9, family = "Cutive Mono", size = 12, alpha = 0.6)


# Layout plots and fill in background between plots
p4 = p1  /p2 / p3 & theme(plot.background = element_rect(fill = "#dfd2c1", color = "#dfd2c1"))

   
# Output the plot 
ggsave(p4, filename = "~/Desktop/challenge_02.png", width = 12, height = 3.7)


