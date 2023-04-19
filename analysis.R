library(dplyr)
library(janitor)
library(ggplot2)
library(scales)

app_colours <- list(
  title = "#616161",
  axis = "#9e9e9e",
  main = "#1976d2",
  no_emphasis = "#757575",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  line_complementary = "#78909c"
)

theme_minimalistic <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = app_colours$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = app_colours$axis),
          axis.ticks = element_line(colour = app_colours$axis),
          axis.text = element_text(colour = app_colours$axis),
          axis.title = element_text(colour = app_colours$axis),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)
    )
}

theme_set(theme_minimalistic())

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
         population_historical_estimates) %>% 
  merge(y = df_continents, by = "code", all.x = TRUE) %>% 
  merge(y = df_gdp, by = "code", all.x = TRUE)

### There are some countries that don't have value in GDP column
countries_without_gdp <- df_cleaned %>%
  filter(is.na(gdp_per_capita)) %>%
  select(code, entity)

### Check if there is some correlation between gdp per capita and hungry index
legend_circle_breaks <- as.integer(
  quantile(df_cleaned$population_historical_estimates, 
           prob = c(0.5, 0.98, 0.99))
) %>% 
  signif(1)

df_cleaned %>% 
  filter(!is.na(gdp_per_capita)) %>%
  ggplot(aes(x = gdp_per_capita, y = global_hunger_index_2021)) +
  geom_point(aes(size = population_historical_estimates, colour = continent),
             alpha = 0.8) +
  ### Creating a linear model to show the correlation
  geom_smooth(method = "lm", 
              se = FALSE, 
              colour = "#555555",
              linetype = "longdash") +
  labs(x = "GDP Per Capita", 
       y = "Hunger Index") +
  scale_size_continuous(breaks = legend_circle_breaks, 
                        range = c(1, 17), 
                        labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     limits = c(0, 50)) +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  guides(size = guide_legend(title = "Population",
                             order = 1,
                             override.aes = list(alpha = 0.25)),
         colour = guide_legend(title = "Continent",
                               order = 2,
                               override.aes = list(size = 4,
                                                   alpha = 0.75)))

### Check if there is some relationship between global gdp and hungry index
### Check where (continent) the hungry is still strong (maybe Africa)
### Check how this indexes are evolving through years

### Check why some countries don't have a hungry index
# According to the origin https://www.globalhungerindex.org/pdf/en/2021.pdf, some
# countries don't have theses values because data to foment the index is not
# collected (mainly in high-income countries)