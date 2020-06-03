# merging vk

# loading packages
library(tidyverse)
library(fuzzyjoin)

# get data
vk_ru <- read_csv("data/russia_vk_cities_all.csv")
pop_ru <- read_csv("data/russia_pop_sizes.csv")
vk_ua <- read_csv("data/ukraine_vk_cities_all.csv")
pop_ua <- read_csv("data/ukraine_pop_sizes.csv")

# write function to join closest cities to tbl

join_iteratively <- function(real_cities_vec, false_cities_vec, max_distance = 3) {
  
  real_cities_tbl <- real_cities_vec %>% # create tibble
    enframe() %>% 
    select(real_city = value) %>% 
    filter(!is.na(real_city))
  
  false_cities_tbl <- false_cities_vec %>% # create tibble
    enframe() %>% 
    select(false_city = value) %>% 
    filter(!is.na(false_city))
  
  i <- 0 # set index for while loop
  output <- vector("list", length = max_distance) # create list to fill
  
  while (i < max_distance) {
    i <- i + 1
    output[[i]] <- real_cities_tbl %>% # fill list
      stringdist_inner_join(false_cities_tbl, # fuzzy join
                        by = c(real_city = "false_city"),
                        max_dist = i, # distance 1...max_distance(defaults to 3)
                        distance_col = "distance") %>% 
      select(real_city, false_city) %>% # get rid of distance column
      group_by(real_city) %>% 
      mutate(id = 1,
             id = cumsum(id)) %>% # ids for spreading
      pivot_wider(names_from = id, values_from = false_city) # spread it
  
  }
  return(output)
}

output_ru <- join_iteratively(pop_ru$Name, vk_ru$city.title, max_distance = 3)

output_merged <- vector("list", length = 3)
for (i in 1:length(output_ru)) {
  output_merged[[i]] <- pop_ru %>% 
    right_join(output_ru[[i]], by = c("Name" = "real_city")) %>% # merge original tibble
    arrange(desc(Pop2019))
}

# write csvs for checking in Excel

for (i in 1:length(output_merged)) {
  name <- paste0("data/cities-distance", i, ".csv")
  write_csv(output_merged[[i]], name)
}
