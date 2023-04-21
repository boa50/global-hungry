library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)
library(scales)
library(ggtext)
library(showtext)
showtext_auto()

app_colours <- list(
  title = "#474747",
  axis = "#757575",
  legend_title = "#474747",
  legend_text = "#757575",
  subtitle = "#757575",
  caption = "#8f8f8f",
  main = "#08386b",
  no_emphasis = "#8f8f8f",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  line_complementary = "#78909c",
  correlation_line = "#555555",
  ### Define the standard colours to all continents
  continent = c("Africa" = "#f0828a",
                "Asia" = "#549ec4",
                "Europe" = "#a86464",
                "North America" = "#369482",
                "South America" = "#5ce094",
                "Oceania" = "#5945a1")
)

theme_minimalistic <- function() {
  theme_classic() +
    theme(text = element_text(family = "montserrat"),
          plot.title = element_text(hjust = 0, 
                                    size = 20,
                                    colour = app_colours$title,
                                    family = "bebas-neue"),
          plot.title.position = "plot",
          axis.line = element_line(colour = app_colours$axis),
          axis.ticks = element_line(colour = app_colours$axis),
          axis.text = element_text(colour = app_colours$axis),
          axis.title = element_text(colour = app_colours$axis),
          legend.title = element_text(colour = app_colours$legend_title),
          legend.text = element_text(colour = app_colours$legend_text),
          plot.subtitle = element_textbox_simple(colour = app_colours$subtitle,
                                                 margin = margin(b = 15)),
          plot.caption = element_text(colour = app_colours$caption),
          plot.caption.position = "plot",
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

### Check the minimum value of the index
min(df_hunger$global_hunger_index_2021) # 2.5
max(df_hunger$global_hunger_index_2021) # 65.1

##################################### CLEANING
### Creating a dataframe that shows only data from countries 
### that have a hungry index

# Countries continents
df_continents <- df_hunger_gdp %>% 
  filter(continent != "") %>% 
  select(code, continent)

df_cleaned <- df_hunger_gdp %>% 
  rename(country = entity,
         hunger_index = global_hunger_index_2021,
         population = population_historical_estimates,
         gdp_per_capita = gdp_per_capita_ppp_constant_2017_international) %>% 
  select(country, 
         code,
         year,
         hunger_index,
         population,
         gdp_per_capita) %>% 
  arrange(code, year) %>% 
  group_by(code) %>% 
  ### Filling years where some countries don't have the value of their
  ### gdp per capita
  fill(gdp_per_capita, .direction = "downup") %>%
  filter(!is.na(hunger_index)) %>%
  merge(y = df_continents, by = "code", all.x = TRUE) %>%
  mutate(gdp = population * gdp_per_capita)

### Actual scenario
df_cleaned %>% 
  filter(year %in% c(2000, 2021)) %>%
  group_by(year) %>% 
  summarise(hunger_index = mean(hunger_index))

### Check where (continent) the hungry is still strong (maybe Africa)
### and check how this indexes are evolving through years

# Remove the countries that only have one year
sporadic_countries <- df_cleaned %>% 
  group_by(code) %>% 
  summarize(n = n()) %>% 
  filter(n < 4) %>% 
  pull(code)

# Get the average hunger index for each country
df_cleaned %>% 
  # filter(!(code %in% sporadic_countries)) %>%
  group_by(continent, year) %>% 
  summarise(hunger_index = mean(hunger_index)) %>% 
  ggplot(aes(x = year, 
             y = hunger_index, 
             colour = continent,
             label = label_number(accuracy = 1)(hunger_index))) +
  geom_line(key_glyph = "point") +
  ### Put a world line to show the world tendency
  # geom_smooth(data = df_cleaned,
  #             method = "lm",
  #             se = FALSE,
  #             colour = app_colours$correlation_line,
  #             linetype = "longdash") +
  geom_point(data = . %>% filter(year %in% c(min(.$year), max(.$year))),
             size = 3) +
  geom_text(data = . %>% filter(year == min(.$year)), 
            nudge_x = -1,
            key_glyph = "point") +
  geom_text(data = . %>% filter(year == max(.$year)), 
            nudge_x = 1,
            key_glyph = "point") +
  labs(title = "Global Hunger Index changes throughout the years",
       subtitle = "The Global Hunger Index ranges from 0 to 100, with 0 representing no hunger.",
       x = "Year", 
       y = "Global Hunger Index",
       caption = "Data source: https://ourworldindata.org") +
  scale_x_continuous(breaks = unique(df_cleaned$year)) +
  scale_colour_manual(breaks = names(app_colours$continent),
                      values = app_colours$continent) +
  guides(colour = guide_legend(title = "Continent",
                               override.aes = list(size = 4,
                                                   shape = 15)))


df_cleaned %>% 
  # filter(!(code %in% sporadic_countries)) %>%
  group_by(continent, year) %>% 
  summarise(hunger_index = mean(hunger_index)) %>% 
  ggplot(aes(x = year, 
             y = hunger_index, 
             colour = continent,
             label = label_number(accuracy = 1)(hunger_index))) +
  geom_line(key_glyph = "point") +
  ### Put a world line to show the world tendency
  geom_smooth(data = df_cleaned,
              method = "lm",
              se = FALSE,
              colour = app_colours$main,
              linetype = "longdash") +
  labs(title = "Global Hunger Index world trend",
       subtitle = "The Global Hunger Index ranges from 0 to 100, with 0 representing no hunger.",
       x = "Year", 
       y = "Global Hunger Index",
       caption = "Data source: https://ourworldindata.org") +
  scale_x_continuous(breaks = unique(df_cleaned$year)) +
  scale_colour_manual(breaks = names(app_colours$continent),
                      values = rep(app_colours$no_emphasis, 6)) +
  guides(colour = guide_legend(title = "Continent",
                               override.aes = list(size = 4,
                                                   shape = 15)))

### There are some countries that don't have value in GDP column
countries_without_gdp <- df_cleaned %>%
  filter(is.na(gdp_per_capita)) %>%
  select(code, country)

### Check if there is some correlation between gdp per capita and hungry index
legend_circle_breaks <- as.integer(
  quantile(df_cleaned$population, prob = c(0.5, 0.98, 0.99))
) %>% 
  signif(1)

df_cleaned %>% 
  filter(year == 2021) %>% 
  # filter(continent == "Africa") %>%
  filter(!is.na(gdp_per_capita)) %>%
  ggplot(aes(x = gdp_per_capita, y = hunger_index)) +
  geom_point(aes(size = population, colour = continent),
             alpha = 0.8) +
  ### Creating a linear model to show the correlation
  # geom_smooth(method = "lm", 
  #             se = FALSE, 
  #             colour = app_colours$correlation_line,
  #             linetype = "longdash") +
  labs(title = "Global Hunger Index correlation with GDP Per Capita in 2021",
       subtitle = "The Global Hunger Index ranges from 0 to 100, with 0 representing no hunger. 
       GDP is measured using constant international-dollars, accounting for cross-country price differences and inflation.",
       x = "GDP Per Capita", 
       y = "Global Hunger Index",
       caption = "Data source: https://ourworldindata.org") +
  scale_size_continuous(breaks = legend_circle_breaks, 
                        range = c(1, 17), 
                        labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0)),
                     limits = c(0, 70)) +
  scale_x_continuous(labels = label_dollar(),
                     limits = c(0, 60000)) +
  scale_colour_manual(breaks = names(app_colours$continent),
                      values = app_colours$continent) +
  # scale_colour_manual(breaks = names(app_colours$continent),
  #                     values = c(app_colours$continent[1],
  #                                rep(app_colours$no_emphasis, 5))) +
  guides(size = guide_legend(title = "Population",
                             order = 1,
                             override.aes = list(shape = 1,
                                                 colour = app_colours$no_emphasis)),
         colour = guide_legend(title = "Continent",
                               order = 2,
                               override.aes = list(size = 4,
                                                   shape = 15)))

### Check why some countries don't have a hungry index
# According to the origin https://www.globalhungerindex.org/pdf/en/2021.pdf, some
# countries don't have theses values because data to foment the index is not
# collected (mainly in high-income countries)

### Check if there is some relationship between total gdp and hungry index
### Maybe not very useful
# df_cleaned %>% 
#   filter(year == 2021) %>% 
#   filter(!is.na(gdp_per_capita)) %>%
#   ggplot(aes(x = gdp, y = hunger_index)) +
#   geom_point(aes(size = population, colour = continent),
#              alpha = 0.8) +
#   ### Creating a linear model to show the correlation
#   geom_smooth(method = "lm", 
#               se = FALSE, 
#               colour = "#555555",
#               linetype = "longdash") +
#   labs(x = "GDP", 
#        y = "Hunger Index") +
#   scale_size_continuous(breaks = legend_circle_breaks, 
#                         range = c(1, 17), 
#                         labels = label_number(scale_cut = cut_short_scale())) +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
#                      limits = c(0, 50)) +
#   scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
#   scale_colour_manual(values = app_colours$continent) +
#   guides(size = guide_legend(title = "Population",
#                              order = 1,
#                              override.aes = list(alpha = 0.25)),
#          colour = guide_legend(title = "Continent",
#                                order = 2,
#                                override.aes = list(size = 4,
#                                                    alpha = 0.75)))