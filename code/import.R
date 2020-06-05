# get data
vk_ru <- read_csv("russia_vk_cities_all.csv") %>% 
  preprocess_vk_tbl()
pop_ru <- "matched.xlsx" %>% 
  read_excel(sheet = "russia") %>% 
  preprocess_population_tbl()

vk_ua <- read_csv("ukraine_vk_cities_all.csv") %>% 
  preprocess_vk_tbl()

pop_ua <- "matched.xlsx" %>% 
  read_excel(sheet = "ukraine") %>% 
  preprocess_population_tbl()

