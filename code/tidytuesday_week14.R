# ------------------------------------------------------------------------
# Tidyverse: Week 14: Life Expectancy
# Link: https://ourworldindata.org/life-expectancy
#
# Location: /Users/raymondtse/Dropbox/Analysis/Visualisation/tidytuesday/rcode/tidytuesday_week14.r
# First created: 11:53 - Sunday 8 July 2018
# Last modified: 11:53 - Sunday 8 July 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(conflicted)
library(ggrepel)
library(readr)
library(skimr)
library(tidyverse)

# ------------------------------------------------------------------------
# Session Info
# ------------------------------------------------------------------------
devtools::session_info()

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
week14 <- read_csv("data/week14_global_life_expectancy.csv")

# ------------------------------------------------------------------------
# EDA
# ------------------------------------------------------------------------
skim(week14)

# 4 groupings with a code including OWID_WRL
week14 %>% 
  dplyr::filter(str_detect(code, 'OWID')) %>% 
  select(country, code) %>% 
  distinct() %>% 
  View()

# 202 countries: excluding the OWID_WRL world group
week14 %>% 
  dplyr::filter(is.na(code) == FALSE) %>% 
  select(country, code) %>% 
  distinct() %>% 
  View()

# 38 regions / groupings - with no code
week14 %>% 
  dplyr::filter(is.na(code) == TRUE) %>% 
  select(country) %>% 
  distinct() %>% 
  View()

# Data for UK goes back to 1543
week14 %>%
  dplyr::filter(year == min(year))

# ------------------------------------------------------------------------
# Graph: Life expectancy globally and by world regions since 1770
# ------------------------------------------------------------------------
regions <- c("Europe",
             "Oceania",
             "Northern America",
             "Asia",
             "Africa",
             "World")
week14 %>% 
  dplyr::filter(country %in% regions) %>% 
  mutate(label= if_else(year == max(year), as.character(country), NA_character_)) %>% 
  ggplot(aes(x = year,
             y = life_expectancy,
             colour = country)) +
  geom_line() +
  theme_minimal() +
  labs(x= NULL,
       y = NULL,
       title = "Life expectancy globally and by world regions since 1770") +
  scale_x_continuous(labels = seq(1950, 2010, 10),
                     breaks = seq(1950, 2010, 10)) +
  scale_y_continuous(labels = seq(0, 80, 10),
                     breaks = seq(0, 80, 10)) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE)







