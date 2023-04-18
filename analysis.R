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

# Check why some countries don't have a hungry index
# Check the minimum value of the index
# Check if there is some relationship between gdp per capita and hungry index
# Check where (continent) the hungry is still strong