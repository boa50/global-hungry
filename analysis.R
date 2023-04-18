library(dplyr)
library(janitor)

df_hunger <- read.csv("data/global-hunger-index.csv", 
                      check.names = FALSE) %>% 
  clean_names()
df_hunger_gdp <- read.csv("data/global-hunger-index-vs-gdp-per-capita.csv", 
                          check.names = FALSE) %>% 
  clean_names()


df_hunger_gdp <- df_hunger_gdp %>% 
  filter(year >= 1900)

### Check the minimum value of the index
min(df_hunger$global_hunger_index_2021)
max(df_hunger$global_hunger_index_2021)

##################################### CLEANING
### Creating a dataframe that shows only data from countries that have a hungry index
n_distinct(df_hunger$entity)

# Countries continents
df_continents <- df_hunger_gdp %>% 
  filter(continent != "") %>% 
  select(code, continent)

# Countries last GDP per capita
df_gdp <- df_hunger_gdp %>% 
  filter(year <= 2021) %>%
  filter(!is.na(gdp_per_capita_ppp_constant_2017_international)) %>% 
  arrange(entity, year) %>% 
  group_by(code) %>% 
  summarise(gdp_per_capita = last(gdp_per_capita_ppp_constant_2017_international))

df_cleaned <- df_hunger_gdp %>% 
  filter(year == 2021) %>%
  filter(!is.na(global_hunger_index_2021)) %>% 
  select(entity, 
         code,
         global_hunger_index_2021,
         population_historical_estimates)

df_cleaned %>% 
  merge(y = df_continents, by = "code", all.x = TRUE) %>% 
  merge(y = df_gdp, by = "code", all.x = TRUE) %>% 
  filter(is.na(gdp_per_capita))
### Check if there is some relationship between gdp per capita and hungry index

### Check if there is some relationship between global gdp and hungry index
### Check where (continent) the hungry is still strong (maybe Africa)
### Check how this indexes are evolving through years

### Check why some countries don't have a hungry index
# According to the origin https://www.globalhungerindex.org/pdf/en/2021.pdf, some
# countries don't have theses values because data to foment the index is not
# collected (mainly in high-income countries)