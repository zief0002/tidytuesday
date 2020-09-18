######################################
# Import data
######################################

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')



######################################
# Load Libraries
######################################

library(tidyverse)
library(tidytext)



######################################
# Create initial dataset of sentences, and starting letters
######################################

friends2 = friends %>%
  unnest_tokens(sentence, text, token = "sentences") %>% #Get sentences
  mutate(
    text = str_to_lower(sentence),             #Make text lower-case
    start_letter = str_extract(sentence, "^.") #Extract first character from text
    ) %>%
  filter(start_letter %in% letters) %>%
  mutate(
    start_letter = factor(start_letter, levels = letters)
  )



######################################
# Create Corpus
######################################

# Data from blog post https://glossarch.wordpress.com/2014/01/14/the-most-common-letter-to-start-a-sentence-with-in-english/

brown_freq = data.frame(
  start_letter = str_to_lower(c("T","I","A","H","S","W","B","M","O","F","N","P","C","D","E","Y","L","R","G","J","U","V","K","Q","Z","X")),
  freq = c(11928,7006,4830,4653,3225,3100,2412,1836,1735,1462,1314,1034,981,941,868,859,713,666,517,354,299,143,143,41,10,2)
) %>% 
  mutate(prop = freq / sum(freq))



######################################
# Create dataframe of proportions of starting letters for each character
######################################

# Which characters do I want?
stars = c("Chandler Bing", "Joey Tribbiani", "Monica Geller", "Phoebe Buffay", "Rachel Green", "Ross Geller", "#ALL#")


# Counts of each letter for each character
letter_counts = friends2 %>% 
  filter(speaker %in% stars) %>%         #Get only main characters
  group_by(speaker, start_letter) %>% 
  summarize(N = n()) %>%                 #Count sentences that start with each letter for each character
  ungroup()


# Turn counts into proportions and join with the corpus data
my_summary = letter_counts %>%
  group_by(speaker) %>%
  summarize( P = N / sum(N)) %>%
  ungroup() %>%
  mutate(start_letter = letter_counts$start_letter) %>%
  left_join(brown_freq, by = "start_letter")


# Add in the 'missing' data (not all character speak sentences that begin with every letter)
missing_letters = data.frame(
  speaker = c(unique(letter_counts$speaker), "Monica Geller", rep("#ALL#", 5)),
  P = rep(0, 13),
  start_letter = c(rep("x", 7), "z", "f", "j", "k", "q", "z"),
  freq = c(rep(2, 7), 10, 1462, 354, 143, 41, 10), 
  prop = c(rep(0.0000391604, 7), 0.0001958020, 0.0286262531, 0.0069313910, 0.0027999687, 0.0008027882, 0.0001958020)
)



######################################
# Create dataframe of differences (character - corpus)
######################################

viz_data = rbind(my_summary, missing_letters) %>%
  mutate(
    .resid = P - prop,                                       #Create variable of residual differences
    negative = if_else(.resid < 0, "Negative", "Positive"),  #Create categorical variable for color
    start_letter = str_to_upper(start_letter)                #Make the start letters upper-case for better plotting
    )



######################################
# Use icons in plots
######################################

# Load library
library(png)


# Read in PNG files
rachel   = readPNG("/Users/zief0002/Dropbox/syntax/tidy-tuesday/2020-09-08-friends/rachel.png")
phoebe   = readPNG("/Users/zief0002/Dropbox/syntax/tidy-tuesday/2020-09-08-friends/phoebe.png")
monica   = readPNG("/Users/zief0002/Dropbox/syntax/tidy-tuesday/2020-09-08-friends/monica.png")
ross     = readPNG("/Users/zief0002/Dropbox/syntax/tidy-tuesday/2020-09-08-friends/ross.png")
joey     = readPNG("/Users/zief0002/Dropbox/syntax/tidy-tuesday/2020-09-08-friends/joey.png")
chandler = readPNG("/Users/zief0002/Dropbox/syntax/tidy-tuesday/2020-09-08-friends/chandler.png")
all      = readPNG("/Users/zief0002/Dropbox/syntax/tidy-tuesday/2020-09-08-friends/all-friends.png")


# Create rasters from PNGs to be useable in ggplot
rachel2   = as.raster(rachel)
phoebe2   = as.raster(phoebe)
monica2   = as.raster(monica)
ross2     = as.raster(ross )
joey2     = as.raster(joey)
chandler2 = as.raster(chandler)
all2      = as.raster(all)


# The next parts of code creates 7 different plots; one per character.


##################################################
### Rachel Green
##################################################

rachel_data = viz_data %>%
  filter(speaker == "Rachel Green")
  
p1 = ggplot(data = rachel_data, aes(x = start_letter, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", aes(fill = negative)) +
  scale_x_discrete(drop = FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.25), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#D6DF23", "#49AEB4")) +
  annotation_raster(rachel2, xmin = "A", xmax = "D", ymin = 0.14, ymax = 0.25) +
  #annotate("text", x = "E", y = 0.20, label = "Rachel's sentences start\nwith the letters 'O', 'W', and 'Y'\nmore than is typical, and 'T' less\nthan is typical.", hjust = 0, size = 3) +
  labs(
    x = "",
    y = "Difference"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = FALSE)

p1


##################################################
### Phoebe Buffay
##################################################

phoebe_data = viz_data %>%
  filter(speaker == "Phoebe Buffay")

p2 = ggplot(data = phoebe_data, aes(x = start_letter, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", aes(fill = negative)) +
  scale_x_discrete(drop = FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.25), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#D6DF23", "#49AEB4")) +
  annotation_raster(phoebe2, xmin = "A", xmax = "D", ymin = 0.14, ymax = 0.25) +
  #annotate("text", x = "E", y = 0.20, label = "Phoebe's sentences start\nwith the letter 'O' more than\nis typical, and 'T' less than\nis typical.", hjust = 0, size = 3) +
  labs(
    x = "",
    y = "Difference"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = FALSE)

p2


##################################################
### Monica Geller
##################################################

monica_data = viz_data %>%
  filter(speaker == "Monica Geller")

p3 = ggplot(data = monica_data, aes(x = start_letter, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", aes(fill = negative)) +
  scale_x_discrete(drop = FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.25), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#D6DF23", "#49AEB4")) +
  annotation_raster(monica2, xmin = "A", xmax = "D", ymin = 0.14, ymax = 0.25) +
  #annotate("text", x = "E", y = 0.20, label = "Monica's sentences start with\nthe letter 'O', 'W', and 'Y' more\nthan is typical, and 'T' less than\nis typical.", hjust = 0, size = 3) +
  labs(
    x = "",
    y = "Difference"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = FALSE)

p3



##################################################
### Chandler Bing
##################################################

chandler_data = viz_data %>%
  filter(speaker == "Chandler Bing")

p4 = ggplot(data = chandler_data, aes(x = start_letter, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", aes(fill = negative)) +
  scale_x_discrete(drop = FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.25), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#D6DF23", "#49AEB4")) +
  annotation_raster(chandler2, xmin = "A", xmax = "D", ymin = 0.14, ymax = 0.25) +
  #annotate("text", x = "E", y = 0.20, label = "Chandler's sentences start with\nthe letter 'O', 'W', and 'Y' more\nthan is typical, and 'T' less than\nis typical.", hjust = 0, size = 3) +
  labs(
    x = "",
    y = "Difference"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = FALSE)

p4


##################################################
### Joey Tribbiani
##################################################

joey_data = viz_data %>%
  filter(speaker == "Joey Tribbiani")

p5 = ggplot(data = joey_data, aes(x = start_letter, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", aes(fill = negative)) +
  scale_x_discrete(drop = FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.25), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#D6DF23", "#49AEB4")) +
  annotation_raster(joey2, xmin = "A", xmax = "D", ymin = 0.14, ymax = 0.25) +
  #annotate("text", x = "E", y = 0.20, label = "Joey's sentences start with\nthe letter 'O', 'W', and 'Y' more\nthan is typical, and 'T' less than\nis typical.", hjust = 0, size = 3) +
  labs(
    x = "",
    y = "Difference"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = FALSE)

p5


##################################################
### Ross Geller
##################################################

ross_data = viz_data %>%
  filter(speaker == "Ross Geller")

p6 = ggplot(data = ross_data, aes(x = start_letter, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", aes(fill = negative)) +
  scale_x_discrete(drop = FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.25), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#D6DF23", "#49AEB4")) +
  annotation_raster(ross2, xmin = "A", xmax = "D", ymin = 0.14, ymax = 0.25) +
  #annotate("text", x = "E", y = 0.20, label = "Ross' sentences start with\nthe letter 'O', 'W', and 'Y' more\nthan is typical, and 'T' less than\nis typical.", hjust = 0, size = 3) +
  labs(
    x = "",
    y = "Difference"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = FALSE)

p6



##################################################
### Ross Geller
##################################################

all_data = viz_data %>%
  filter(speaker == "#ALL#")

p7 = ggplot(data = all_data, aes(x = start_letter, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", aes(fill = negative)) +
  scale_x_discrete(drop = FALSE) + 
  scale_y_continuous(limits = c(-0.25, 0.25), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#D6DF23", "#49AEB4")) +
  annotation_raster(all2, xmin = "A", xmax = "D", ymin = 0.10, ymax = 0.25) +
  #annotate("text", x = "E", y = 0.20, label = "Ross' sentences start with\nthe letter 'O', 'W', and 'Y' more\nthan is typical, and 'T' less than\nis typical.", hjust = 0, size = 3) +
  labs(
    x = "",
    y = "Difference"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = FALSE)

p7




##################################################
### Create final plot
##################################################

# Load library
library(patchwork)


# Layout final plot
final = (p2 | p3 | p1)  / (p4 | p5 | p6) / p7 + 
  plot_annotation(
  title = "Comparison of the Beginning Letter of Friends' Sentences Relative to the English Language", 
  subtitle = "In general, Friends' sentences start with the letter 'O' (oh, okay), 'W' (well, what), and 'Y' (you, yeah, yes) more than is typical, and 'T' far less than is typical. When they all speak together sentences that start with 'H' (hey, hi) are also more than typical and those that start\nwith 'I' are less typical.",
  caption = "Based on the Brown University Standard Corpus of Present-Day American English"
) 


# Save pic
ggsave(final, filename = "~/Desktop/frinds-plot.png", width = 20, height = 12, units = "in")
