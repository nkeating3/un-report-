## Setup Section 

# loading the tidyverse
library(tidyverse)
library(ggplot2)

#######################################
## Day 1 work: Intro to R and Plotting
#######################################

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
ggsave("gdpPerCap_lifeExp.png")

########################################
## Day 2 Work: Data Manipulation and Cleaning
########################################

gapminder_data <- read_csv("data/gapminder_data.csv")
glimpse(gapminder_data)
View(gapminder_data)

# without a pipe
summarize(gapminder_data, avgLifeExp = mean(lifeExp))
summarize(gapminder_data, maxLifeExp = max(lifeExp))

## pipes
# %>%
# means "and then do this thing"
gapminder_data %>% summarize(avgLifeExp = mean(lifeExp))

# save the summary table 
gapminder_data_summary <- gapminder_data %>% 
  summarize(avgLifeExp = mean(lifeExp))

## Exercises
gapminder_data %>% filter(year == 2007) %>% 
  summarize(avgLifeExp07 = mean(lifeExp))

#earliest year in the data using summarize() and min()
gapminder_data %>% summarize(earliestYear = min(year))

#filter data to the 1952 only and find avg GDP per capita
gapminder_data %>% filter(year == 1952) %>%
  summarize(avgGDPpercap = mean(gdpPercap))

#calculate life expectancy by year
gapminder_data %>% 
  group_by(year) %>% 
  summarize(avg = mean(lifeExp))

#calculate life expectancy by continent
gapminder_data %>%
  group_by(continent) %>%
  summarize(avg = mean(lifeExp), 
             min = min(lifeExp), 
             max = max(lifeExp))

#adding columns to data set using mutate
gapminder_data %>% mutate(gdp = gdpPercap*pop)

#we have a column called pop, use mutate to create
#column for pop in millions
gapminder_data %>% mutate(popinmil = pop/1000000)

#filter to select rows, select to select columns
gapminder_data %>% select(pop, year)
gapminder_data %>% select(-continent)

#print a data frame with country, continent, year and life exp
gapminder_data %>% select(country, continent, year, lifeExp)
gapminder_data %>% select(year, starts_with("c"))

#print data frame of al cols that end in letter "p"
gapminder_data %>% select(ends_with("p"))

## changing the shape of the data
gapminder_data %>% select(country,continent,year,lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# save a data frame that contains only the Americas in 2007
#logical symbols: & is for AND, | is for OR
gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)

## How to clean data, above is manipulation
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2, 
         col_names = c("region_number", "country", "year", 
                       "series", "value", "footnotes", "source"))
glimpse(co2_emissions_dirty)

#select the country, year, series, and value columns
co2_emissions_dirty %>% select(country, year, series, value) %>%
  print(n=50)

#cleaning up the table to have more accurate labels 
co2_emissions_dirty %>% select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value)

#exercise: filter out data from 2005 and drop the year column 
co2_emissions <- co2_emissions_dirty %>% select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)

# previously we created gapminder for 2007
# now we have co2 emissions for 2005
# now let's join them together
inner_join(gapminder_data, co2_emissions, by = "country")

anti_join(gapminder_data, co2_emissions, by = "country")
view(gapminder_data)

co2_emissions <- read_csv("data/co2-un-data.csv", skip=2,
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, 
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of" = "Venezuela"))
anti_join(gapminder_data, co2_emissions, by = "country")  

# address Puerto Rico 
gapminder_data <- gapminder_data %>% mutate(country = recode(country, 
                                           "Puerto Rico" = "United States"))
 
gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by = "country")

glimpse(gapminder_co2)

gapminder_co2 %>% group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp))

gapminder_co2 %>%
  filter(continent == "Americas") %>%
  mutate(region = ifelse(country == "United States" | country == "Canada" | country == "Mexico", "north", "south"))

# I want to focus on the Americas in 2007
gapminder_co2 <- gapminder_co2 %>%
  filter(continent == "Americas" & year == "2007") 

# write out new clean dataset as csv
write_csv(gapminder_co2, "data/gapminder_co2.csv")
