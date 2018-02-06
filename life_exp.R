setwd("~/Desktop/Opendata/Life_exp")

library(rvest)
library(ggplot2)
library(ggmap)
library(dplyr)
library(plotly)
library(RColorBrewer)

#getting webpage
webpage <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy")

#getting list of table from html nodes
tbls <- html_nodes(webpage, "table")
tbls

#selected node
life_exp <- webpage %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/    table[1]') %>%
  .[1] %>%
  html_table(fill = TRUE)

head(life_exp)

#data frame the nodes
life_exp <- as.data.frame(life_exp)

#rename the variable
names(life_exp)
names(life_exp) <- make.names(names(life_exp))
colnames(life_exp) <- c("country","Both_sexes_rank", "Both_sexes_life_exp", "Female_rank","Female_life_exp", "Male_rank","Male_life_exp", "Both_sexes_rank(HALE)", "Both_life_exp(HALE)")

#checking for null value
sum(is.na(life_exp))

#getting world maps
world_map <- map_data("world")

#see the difference of code between two object
setdiff(life_exp$country, world_map$region)

#recode the object 
life_exp$country <- recode(life_exp$country, 
                           'United States' = 'USA',
                           'United Kingdom' = 'UK',
                           'Congo' = 'Republic of Congo',
                           'DR Congo' = 'Democratic Republic of the Congo',
                           "Cote d'Ivoire" = 'Ivory Coast')

#join the two object
map.world.joined <- left_join(world_map, life_exp, by = c('region' = 'country'))

#visualize the life expectancy of both sexes
a = ggplot(map.world.joined, aes(x = long, y = lat, group = group, fill = Both_sexes_life_exp)) +
  geom_polygon() + 
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = 'Life Expectancy by World Health Organization in 2015', fill = 'Life Expectancy of Both Sexes')+
  theme_void()

ggplotly(a)

#visualise female life expectancy
b = ggplot(map.world.joined, aes(x = long, y = lat, group = group, fill = map.world.joined$Female_life_exp)) +
  geom_polygon() + 
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = 'Female Life Expectancy by World Health Organization in 2015', fill = 'Female life Expectancy')+
  theme_void()

ggplotly(b)

#visualize male life expectancy
c = ggplot(map.world.joined, aes(x = long, y = lat, group = group, fill = map.world.joined$Male_life_exp)) +
  geom_polygon() + 
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = 'Female Life Expectancy by World Health Organization in 2015', fill = 'Female life Expectancy')+
  theme_void()

ggplotly(c)
