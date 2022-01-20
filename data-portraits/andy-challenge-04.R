## DATA PORTRAITS - CHALLENGE 2
## ICD TIDY TUESDAY
## MARCH 2021



# Load library
library(tidyverse)
library(showtext) #To use googlefonts
library(patchwork)



# Import data
freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')

# Make long
freed_slaves_long = freed_slaves %>%
  pivot_longer(cols = 2:3, names_to = "Status", values_to = "Percentage")

# Labels data
freed_slaves_labels = freed_slaves %>%
  mutate(
    my_label = paste0(Free, "%"),
    y_pos = if_else(Year == 1870, 90, Slave+1)
  )

## Loading Google fonts (https://fonts.google.com/)
font_add_google(name = "Cutive Mono")



## Automatically use showtext to render google fonts
showtext_auto()



# Create plot
p1 = ggplot(data = freed_slaves_long, aes(x = Year, y = Percentage)) +
  geom_area(aes(fill = Status)) +
  theme_light() +
  scale_x_continuous(name = "", breaks = seq(from = 1790, to = 1870, by = 10), 
                     position = "top") +
  scale_y_continuous(name = "", breaks = NULL, expand = c(0, 0)) +
  scale_fill_manual(name = "", values = c("#58845c", "#1c1c1c")) +
  geom_text(data = freed_slaves_labels, aes(x = Year, y = y_pos, label = my_label), 
            size = 3, vjust = 0, fontface = 'bold') +
  annotate("text", x = 1830, y = 95, label = "FREE — LIBRE", fontface = 'bold') +
  annotate("text", x = 1830, y = 60, label = "SLAVES\nENSCLAVES", fontface = 'bold', color = "white") +
  labs(
    title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES",
    subtitle = "\nPROPORTION DES NÈGRES ET DES ESCLAVES EN AMÈRIQUE\n\n\nDONE BY ATLANTA UNIVERSITY\n"
    ) +
  guides(fill = FALSE) +
  theme(
    plot.background = element_rect(fill = "#dfd2c1"),
    panel.background = element_rect(fill = "#dfd2c1"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(face = 'bold'),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 13)
  ) 
  

   
# Output the plot 
ggsave(p1, filename = "~/Desktop/challenge_04.png", width = 8, height = 10)


