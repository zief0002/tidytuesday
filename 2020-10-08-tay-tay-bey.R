######################################
# Load libraries
######################################

library(tidyverse)
library(tidytext)
library(waffle)
library(hrbrthemes)
library(extrafont)



######################################
# Load Fonts
######################################

# Run once
#font_import()

# Run each session
loadfonts(quiet = TRUE)

# Get fontawesome families
fonttable() %>% 
  as_tibble() %>% 
  filter(grepl("Awesom", FamilyName)) %>% 
  select(afmfile, FullName, FamilyName, FontName)



######################################
# Import data
######################################

#beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
#sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')

taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')



######################################
# Create data frame where each row is one lyric word
######################################

tay = taylor_swift_lyrics %>%
  unnest_tokens("lines", Lyrics) %>%
  group_by(Title) %>%
  mutate(
    wordnumber = row_number()
    ) %>%
  ungroup()



######################################
# Partial match on 'lov' (e.g., love, loving, lover)
######################################

love = tay %>%
  filter(str_detect(lines, "^lov")) %>%
  group_by(Album) %>%
  summarize(N = n()) %>%
  ungroup() %>%
  rename(title = Album) %>%
  mutate(
    title = str_to_title(title),
    Love = "Yes"
  )




######################################
# Partial match on 'heartbreak'
######################################

breakup = tay %>%
  filter(str_detect(lines, "heartbreak")) %>%
  group_by(Album) %>%
  summarize(N = n()) %>%
  rename(title = Album) %>%
  mutate(
    title = str_to_title(title),
    Love = "No"
    )



######################################
# Get unique albums; separaye release date into different columns
######################################

tay_albums = charts %>%
  filter(artist == "Taylor Swift") %>%
  select(title, released) %>%
  distinct() %>%
  separate(released, " ", into = c("month", "day", "year")) %>%
  arrange(year)



######################################
# Create final data frame for plotting
######################################

tay_tay = rbind(love, breakup) %>%
  left_join(tay_albums) %>%
  mutate(
    title = factor(title, levels = c("Taylor Swift", "Fearless", "Speak Now", "Red", "1989", "Reputation",
                                     "Lover", "Folklore"))
  )




######################################
# Create pictograph
######################################

# Get icon names
fa_grep("heart")


# Create plot
ggplot(data = tay_tay, aes(fill = Love, values = N)) +
  geom_pictogram(n_rows = 10, aes(label = Love, colour = Love), family = "FontAwesome5Free-Solid", 
                 flip = TRUE, size = 4) +
  facet_wrap(~title, nrow = 2) +
  coord_equal() +
  scale_label_pictogram(
    name = NULL,
    values = c("heart", "heart-broken")
  ) +
  scale_color_manual(
    name = NULL,
    values = c("#f783ac", "#f783ac")
  ) +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  guides(color = FALSE, label = FALSE)


