# merging vk

# loading packages
library(tidyverse)
library(fuzzyjoin)

# get data
vk_ru <- read_csv("data/russia_vk_cities.csv")
pop_ru <- read_csv("data/russia_pop_sizes.csv")
vk_ua <- read_csv("data/ukraine_vk_cities.csv")
pop_ua <- read_csv("data/ukraine_pop_sizes.csv")

# next steps:
# create empty df with vk cities
vk_ru_cities <- vk_ru %>% 
  select(city = city.title) %>% 
  filter(!is.na(city))

# join "closest" towns to empty df
joined_vk_cities <- vk_ru_cities %>%
  stringdist_inner_join(pop_ru, 
                        by = c(city = "Name"),
                        max_dist = 1,
                        distance_col = "distance") %>% 
  select(vk_city = city.x, pop_city = Name, distance)



# write function to join closest cities to tbl