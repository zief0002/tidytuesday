# Load libraries
library(tidyverse)
library(maps)
library(gganimate)


# Import the dataset
post_offices <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')


# Get MN post offices
# The mutate() creates a sequence of years the PO was open for each PO and then unnest()
# expands that into one row per each year
mn_post_offices = post_offices %>%
  filter(state == "MN") %>%
  select(name, state, established, discontinued, latitude, longitude) %>%
  replace_na(list(discontinued = 2021)) %>%
  mutate(year = purrr::map2(established, discontinued, seq)) %>%
  unnest(year)


# Create the animation
p1 = ggplot(data = mn_post_offices, aes(x = longitude, y = latitude)) +
  geom_polygon(data = mn, aes(x = long, y = lat, group = group), color = "#ababab", fill = "white") +
  geom_point(size = 0.02, color = "darkblue") +
  coord_map("mercator") +
  ggthemes::theme_map()  +
  transition_time(year, range = c(1825L, 2021L)) +
  ease_aes('linear', interval = 0.001) +
  enter_fade() +
  exit_fade() +
  labs(title = 'Year: {frame_time}')


# Output the animation to an animated GIF on my desktop
anim_save(p1, filename = "~/Desktop/post-offices-mn.gif", duration = 30, nframes = 195)


