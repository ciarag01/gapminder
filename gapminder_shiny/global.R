##############################################################################
# Tidy gapminder data before use in app
# Ciara Gribben
# March 2022
##############################################################################

### 1 - Housekeeping ----
# Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(rvest)
library(janitor)
library(gapminder)
library(magrittr)

# Read in data
gap <- gapminder
mortality <- readxl::read_excel("data/global_mortality.xlsx")
life_exp <- readr::read_csv("data/life_expectancy_years.csv")


### 2 - Wrangling ----
# Life Expectancy data
life_exp %<>%
  pivot_longer(-country, names_to = "year", values_to = "life_exp") %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 1990 & year <= 2016)

# Mortality Data - top 10 causes of death
cod <- mortality %>% 
  left_join(gap %>% select(country, continent), by = "country") %>% 
  pivot_longer(cols = `Cardiovascular diseases (%)`:`Terrorism (%)`, names_to = "cod", 
               values_to = "perc") %>% 
  mutate(perc = format(perc, scientific = FALSE)) %>% 
  arrange(continent, desc(perc)) %>% 
  mutate(perc_n = as.numeric(perc)) %>% 
  group_by(year, continent, cod) %>% 
  summarise(tot = sum(perc_n, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(year, continent, desc(tot)) %>% 
  group_by(year, continent) %>% 
  mutate(rank = row_number()) %>%
  mutate(perc = (tot/sum(tot))*100) %>% 
  ungroup() %>% 
  filter(rank <= 10)