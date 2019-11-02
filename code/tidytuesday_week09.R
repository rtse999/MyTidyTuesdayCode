# ------------------------------------------------------------------------
# Description
# Link:
#
# Location:
# First created:
# Last modified:
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(conflicted)
library(tidyverse)
library(skimr)

# ------------------------------------------------------------------------
# Session Info
# ------------------------------------------------------------------------
devtools::session_info()


# ------------------------------------------------------------------------
# Read Week 9 Data
# ------------------------------------------------------------------------
week9_comic_characters <- read_csv("data/2018-05-29/week9_comic_characters.csv")
glimpse(week9_comic_characters)
skim(week9_comic_characters)

# ------------------------------------------------------------------------
# EDA
# ------------------------------------------------------------------------

# Summary by publisher
week9_comic_characters %>% 
  group_by(publisher) %>% 
  summarise(no_of_characters = n())

# New characters by year
week9_comic_characters %>% 
  ggplot(aes(year, colour = publisher)) +
  geom_histogram(binwidth = 1, show.legend = FALSE) +
  labs(title = paste("Number of New Comic Characters Introduced by Year"),
       x = NULL, y = NULL) +
  scale_y_continuous(labels = seq(0, 500, 100),
                     breaks = seq(0, 500, 100)) +
  scale_x_continuous(labels = c(1940, "'60", "'80",2000),
                     breaks = seq(1940, 2000, 20)) +
  scale_fill_manual(values = c("#4fb6f3", "#ed2b17")) + 
  scale_color_manual(values = c("#0c6ea7", "#d52410")) + 
  facet_wrap(~publisher)


# Gender ratio
annual_percent <- 
  week9_comic_characters %>% 
  select(publisher, sex, year) %>% 
  group_by(publisher, year) %>% 
  mutate(new_characters_by_year = n()) %>% 
  group_by(publisher, year, sex) %>% 
  mutate(number_by_sex = n(),
         percent_by_sex = number_by_sex / new_characters_by_year) %>% 
  ungroup() %>% 
  distinct() %>% 
  dplyr::filter(sex == "Female Characters") %>% 
  arrange(publisher, year) %>% 
  group_by(publisher) %>% 
  mutate(cumnum_female = cumsum(number_by_sex),
         cumsum_new_characters = cumsum(new_characters_by_year),
         cumulative_percentage = cumnum_female / cumsum_new_characters)


ggplot(annual_percent) +
  geom_line(aes(x = year, y = cumulative_percentage, colour = publisher))

annual_percent %>% 
  dplyr::filter(year >= 1980 & year <= 2011) %>% 
  ggplot() +
  geom_line(aes(x = year, y = percent_by_sex, colour = publisher)) +
  scale_x_continuous(labels = c(1980, "'90", 2000, "'10"),
                     breaks = seq(1980, 2010, 10)) +
  scale_y_continuous(labels = seq(0, 1, 0.1),
                     breaks = seq(0, 1, 0.1))
  
# Publisher x Gender x Align
week9_comic_characters %>% 
  group_by(publisher, sex) %>% 
  mutate(characters_by_sex = n()) %>% 
  group_by(publisher, sex, align) %>% 
  mutate(proportion = n() / characters_by_sex) %>% 
  ggplot(aes(x = sex)) +
  geom_bar()
  




