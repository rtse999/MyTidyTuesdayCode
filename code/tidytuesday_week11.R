# ------------------------------------------------------------------------
# Tidy Tuesday Week 11
# Link: https://github.com/rfordatascience/tidytuesday
#
# Location: /Users/raymondtse/Dropbox/Analysis/Visualisation/tidytuesday/rcode/tidytuesday_week11.r
# First created: 16:41 - Tuesday 12 Jun 2018
# Last modified: 20:53 - Friday 15 June 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(conflicted)
library(kableExtra)
library(knitr)
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
audience <- read_csv("data/week11_fifa_audience.csv")
glimpse(audience)
skim(audience)

# ------------------------------------------------------------------------
# Create Country data table
# ------------------------------------------------------------------------
audience %>% 
  dplyr::filter(X1 <= 38) %>% 
  kableExtra::kable(format = "html",
                    col.names = c("", "COUNTRY", "BODY",
                                  "GLOBAL POPULATION %",
                                  "WORLD CUP TV AUDIENCE %",
                                  "GDP-WEIGHTED POPULATION %")) %>% 
  add_header_above(c(" " = 3, "IN 2010, SHARE OF ..." = 3)) %>% 
  column_spec(6, background = "#F5F5F5") %>% 
  kable_styling()

# ------------------------------------------------------------------------
# Create Confederation data table
# ------------------------------------------------------------------------
n_countries <- nrow(audience)

p <- audience %>% 
  group_by(confederation) %>% 
  mutate(fifa_members = n(),
         fifa_pct = round(fifa_members / n_countries * 100, 1),
         total_population = sum(population_share),
         total_audience = sum(tv_audience_share),
         total_gdp_audience = sum(gdp_weighted_share)) %>% 
  select(confederation, fifa_members, fifa_pct, contains("total")) %>% 
  distinct() %>% 
  arrange(desc(total_gdp_audience))
  
p %>% 
  kableExtra::kable(format = "html",
                    col.names = c("CONFEDERATION",
                                  "FIFA MEMBERS",
                                  "FIFA MEMBERS %",
                                  "GLOBAL POPULATION %",
                                  "WORLD CUP TV AUDIENCE %",
                                  "GDP-WEIGHTED POPULATION %")) %>% 
  add_header_above(c(" " = 3, "IN 2010, SHARE OF ..." = 3)) %>% 
  kable_styling()

audience %>% 
  ggplot(aes(x = "")) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = confederation),
           show.legend = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste("% of FIFA members by Confederation"),
       x = NULL, y = NULL) +
  coord_flip()

