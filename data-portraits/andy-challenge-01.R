## DATA PORTRAITS - CHALLENGE 1
## ICD TIDY TUESDAY
## MARCH 2021



# Load library
library(tidyverse)
library(showtext) #To use googlefonts



# Import data
georgia_pop <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')


# Convert to long format
ga_long = georgia_pop %>%
  pivot_longer(2:3, names_to = "Race", values_to = "Percentage")



## Loading Google fonts (https://fonts.google.com/)
font_add_google(name = "Cutive Mono")



## Automatically use showtext to render google fonts
showtext_auto()



# Create plot
p1 = ggplot(data = ga_long, aes(x = Year, y = Percentage, group = Race, linetype = Race)) +
  geom_line()  +
  theme_light() +
  scale_y_reverse(name = "", breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(name = "", breaks = seq(from = 1790, to = 1890, by = 10)) +
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  coord_flip(expand = FALSE) +
  ggtitle("COMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION OF GEORGIA") +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "#decbb0"),
    legend.key = element_rect(fill = "#decbb0"),
    plot.background = element_rect(fill = "#decbb0"),
    panel.background = element_rect(fill = "#decbb0"),
    panel.grid.major.x = element_line(color = rgb(1,0,0,0.4)),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = rgb(1,0,0,0.4)),
    panel.grid.minor.y = element_blank(),
    #axis.line.x = element_line(color = "black"),
    #axis.line.y = element_line(color = "black"),
    panel.border = element_rect(color = "black"),
    axis.text = element_text(family = "Cutive Mono"),
    legend.text = element_text(family = "Cutive Mono"),
    plot.title = element_text(hjust = 0.5, family = "Cutive Mono", face = "bold", size = 14)
  )



# Output the plot with a 2:3 aspect ratio
ggsave(p1, filename = "~/Desktop/challenge_01.png", width = 6, height = 9)


