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