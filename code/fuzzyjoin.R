# merging vk

# loading packages
library(tidyverse)
library(magrittr)
library(fuzzyjoin)
library(readxl)

# new joining function: taking into account the already existing alternative names

## the function assumes a tibble in wide format like the one we have here
## the city's "real" name is in the first column, called "real_name"
## the alternative names are in the subsequent ones, called "alternative_notion_1", "alternative_notion_2", etc.
## for the merge, we need one tibble with one column which is called "city"
## 
  
join_it <- function(df_population_data, df_vk_data, distance = 1) {  
  temp_list <- df_population_data %>% 
    map(as.character) %>% 
    transpose() %>% 
    map(as.character) %>% 
    map(enframe) %>% 
    map(~mutate(.x, value = replace_na(value, "xyz12345"))) %>%
    map(~{
      stringdist_left_join(.x, df_vk_data,
                           by = c(value = "city"),
                           max_dist = distance,
                           distance_col = "distance") %>% 
      wrangle_it()
    }) 
  max_length <- temp_list %>% 
    map_dbl(length) %>% 
    max()
  names <- c("real_name",
             "alternative_notion_1", 
             "alternative_notion_2",
             "alternative_notion_3",
             "alternative_notion_4",
             paste0("match_", 1:(max_length-5)))
  temp_list %<>% 
    map(`length<-`, max_length) %>% 
    map(set_names, names)
  do.call(bind_rows, temp_list)
}

# auxiliary functions

## for join_it()

wrangle_it <- function(my_tibble) {
  real_names <- my_tibble %>% 
    filter(value != "xyz12345") %>% 
    distinct(value) %>% 
    pull(value)
  
  vk_names <- my_tibble %>%
    select(-value, -name, -distance) %>% 
    pmap(c) %>% 
    reduce(c) %>% 
    unname() 
  vk_names <- vk_names[!is.na(vk_names)]
  vk_names <- unique(vk_names)
  
  length(real_names) <- 5
  
  c(real_names, vk_names)
}

## for preprocessing the (already matched) population data for join_it()

preprocess_population_tbl <- function(population_tbl) {
  population_tbl %>% select(real_name = 1, 
         alternative_notion_1 = 6, 
         alternative_notion_2 = 7,
         alternative_notion_3 = 8, 
         alternative_notion_4 = 9)
}

## for preprocessing the vk data

preprocess_vk_tbl <- function(vk_tbl) {
  vk_tbl %>% 
    select(city = city.title) %>% 
    filter(!is.na(city))
}

