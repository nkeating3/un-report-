# loading the tidyverse
library(tidyverse)
library(ggplot2)
# read data in the csv file
gapminder_1997 <- read_csv(file = "data/gapminder_1997.csv")

# practice with adding and naming
add_two <- 2+2
cat_name <- "Charlie"
cat_name <- "Marvin"

# functions that don't need arguments
Sys.Date()
getwd()
?read_csv()

# how to round
sum(5,6)
round(x = 3.1415)
round(x = 3.1415,digits = 3)

# Using ggplot to make a plot
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, 
      y = lifeExp, 
      color = continent, 
      size = pop/1000000) +
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       title = "Do people in wealthy countries live longer?", 
       size = "Population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set3") 

# List all colors
RColorBrewer::display.brewer.all()


#Load in full gapminder dataset 
gapminder_data <- read_csv("data/gapminder_data.csv")
dim(gapminder_data)
head(gapminder_data)
glimpse(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = year, 
      y = lifeExp, 
      color = continent, 
      group = country) +
  geom_line()

ggplot(data = gapminder_data) +
  aes(x = year, 
      y = pop,
      color = continent,
      group = country) +
  geom_line()




