# Load libraries
library(tidyverse)
library(stringr)

# Import data
beer_awards <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv" )


#
str_which(string = c("aaa", "bbb"), pattern = "a")    # Equivalent to grep()
str_detect(string = c("aaa", "bba"), pattern = "a")   # Equivalent to grepl()
str_extract(string = c("aaa", "bbb"), pattern = "a")  # Equivalent to grep( , value = TRUE)


# Get state abbreviations
state.abb


# Compare state abbreviations to what is in the beer state column
c(state.abb, "DC") %in% unique(toupper(beer_awards$state))


# Determine which state is missing
state.abb[48]


