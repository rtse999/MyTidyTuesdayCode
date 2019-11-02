# Description -------------------------------------------------------------
#
# Tidy Tuesday: 29 Oct 2019: NYC Squirrel Census
#
# Link: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-29
#
# Location: \Dropbox\Analysis\Visualisation\MyTidyTuesdayCode\code\tidytuesday_20191029_squirrelcensus.r
# First created: 13:35 - Saturday 2 November 2019
# Last modified: 13:36 - Saturday 2 November 2019


# System time -------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")


# Install Packages --------------------------------------------------------
library(conflicted)
library(rcartocolor)
library(scales)
library(skimr)
library(tidyverse)


# Read data ---------------------------------------------------------------
nyc_squirrels <- readr::read_csv("data/2019-10-29/nyc_squirrels.csv")


# Plot long-lat -----------------------------------------------------------
ggplot(nyc_squirrels, aes(x = long, y = lat)) +
  geom_point()

ggplot(nyc_squirrels, aes(x = long, y = lat, colour = ..count..)) +
  geom_hex() +
  coord_fixed() + # makes the plot square
  labs(title = paste("Number of Squirrels Counted in Central Park, NYC"),
       x = NULL, y = NULL) +
  scale_x_continuous(labels = unit_format(accuracy = 0.01, sep = "", unit = "°W"), position = "top") + 
  scale_y_continuous(labels = unit_format(accuracy = 0.01, sep = "", unit = "°N")) +
  scale_color_carto_c(palette = "SunsetDark",
                      guide = guide_colourbar(direction = "horizontal")) + 
  scale_fill_carto_c(palette = "SunsetDark") +
  theme(legend.position="bottom")
  
  
  

  
