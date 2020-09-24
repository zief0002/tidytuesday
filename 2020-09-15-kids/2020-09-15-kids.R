# https://towardsdatascience.com/how-to-make-beautiful-small-multiple-us-maps-in-r-ad7e557cd463


##################################################
### Load libraries
##################################################

library(tidyverse)
library(geofacet)
library(magrittr)



##################################################
### Import data
##################################################

kids = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')
kids

# state	[character]	United States state (and the District of Columbia)
# variable	[character]	Variable
# year	[character]	Year
# raw	[double]	The value of the variable; a numeric value
# inf_adj	[double]	The value of the variable, adjusted for inflation, a numeric value
# inf_adj_perchild	[double]	The value of the variable adjusted for inflation, per child; a numeric value



##################################################
### Create data frame with state names and state abbreviations
##################################################

states = data.frame(
  state = c(state.name, "District of Columbia"),
  abb = c(state.abb, "DC")
)


##################################################
## Prep data
##################################################

kids %<>%                                       # The %<>% operator does assignment and piping
  left_join(states, by = "state") %>%           # Add state abbreviations to kids data
  filter(variable %in% c("highered")) %>%       # Only keep highered rows
  mutate(year_num = as.numeric(year) - 1997)    # Center year so 1997 = 0



##################################################
## Plot 1: Geofacet
##################################################

p1 = ggplot(data = kids, aes(x = year_num, y = inf_adj_perchild)) +
  geom_line(color = "#007A87") +
  theme_bw() +
  facet_geo(~abb) + # This is the magic function
  labs(
    title = "Higher Education Spending By State",
    subtitle = "Inflation Adjusted Per Student, 1997 - 2016",
    x = element_blank(),
    y = element_blank()
    ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    strip.background = element_rect(color = "white")
  )


ggsave(p1, filename = "~/Desktop/plot_1.png", width = 12, height = 9, units = "in")



##################################################
## Plot 2: Geofacet and color area by presidential party
##################################################

p2 = ggplot(data = kids, aes(x = year_num, y = inf_adj_perchild)) +
  geom_rect(mapping=aes(xmin = 0,  xmax = 4,  ymin = 0, ymax = 4), fill = "#ADD8E6", alpha = .05) + #Democrat - Clinton
  geom_rect(mapping=aes(xmin = 4,  xmax = 12, ymin = 0, ymax = 4), fill = "#FF9999", alpha = .05) + #Republican - Bush
  geom_rect(mapping=aes(xmin = 12, xmax = 19, ymin = 0, ymax = 4), fill = "#ADD8E6", alpha = .05) + #Democrat - Obama
  geom_line(color = "#007A87") +
  theme_bw() +
  facet_geo(~abb) +
  labs(
    title = "Higher Education Spending By State",
    subtitle = "Inflation Adjusted Per Student, 1997 - 2016",
    caption = "Years in which a Democrat was President of the United States are shaded blue, and those in which a Republican was President are shaded red.",
    x = element_blank(),
    y = element_blank()
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    strip.background = element_rect(color = "white")
  )


ggsave(p2, filename = "~/Desktop/plot_2.png", width = 12, height = 9, units = "in")



##################################################
## Plot 3: Geofacet and color line by slope
##################################################


state_slopes = kids %>%
  group_by(state) %>%
  nest() %>%
  #split(.$state) %>%
  mutate(
    lm_output = map(data, ~lm(inf_adj_perchild ~ 1 + year_num, data = .x)  %>% broom::tidy(conf.int = TRUE) )
    ) %>%
  select(-data) %>%
  unnest(lm_output) %>%
  filter(term == "year_num") %>%
  mutate(
    positive_slope = case_when(
      conf.low < 0 & conf.high < 0 ~ "Negative",
      conf.low < 0 & conf.high > 0 ~ "No Change",
      TRUE ~ "Positive"
      )
  )


kids2 = kids %>%
  left_join(state_slopes, by = "state")


p3 = ggplot(data = kids2, aes(x = year_num, y = inf_adj_perchild)) +
  geom_line(aes(color = positive_slope)) +
  theme_bw() +
  facet_geo(~abb) +
  labs(
    title = "Higher Education Spending By State",
    subtitle = "Inflation Adjusted Per Student, 1997 - 2016",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_color_manual(
    name = "",
    values = c("#43464B", "#FF3819")
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    strip.background = element_rect(color = "white"),
    legend.position = c(0.95, 0.3)
  ) 


ggsave(p3, filename = "~/Desktop/plot_3.png", width = 12, height = 9, units = "in")


