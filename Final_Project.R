
install.packages('ropenaq')
library(ropenaq)
library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)
library(tidytext)
library(knitr)
library(choroplethr)
library(choroplethrMaps)
library(repurrrsive)
library(broom)

countries = aq_countries()
## A test, download latest data in US with the pm25 measurement
US_latest = aq_latest(country = 'US', parameter = 'pm25')
glimpse(US_latest)

latest = aq_latest(parameter = 'pm25')

processed = latest %>% select(location, country, parameter, value, lastUpdated) %>%
  filter(year(lastUpdated) >= 2017) 

## Find 10 countries with most records
most = processed %>% group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(n = 20)

## Try plot boxplot with the 10 countries which have most records
boxplot = processed %>%
  select(country, value) %>%
  filter(country %in% most$country)
ggplot(data = boxplot) + geom_boxplot(aes(x = country , y = value)) +
  ylim(0, 200)

## Create choroplethrMaps on average pm25 and its deviation based on latest data
avg_sd = processed %>% select(country, value) %>%
  group_by(country) %>%
  summarise(mean = mean(value), sd = sd(value, na.rm = FALSE))

## create the country_name, avg and country_name, sd tables
## First import the region name and code 
data("country.regions")
region = country.regions
colnames(region) = c('name', 'country')

## Joint the two tables to fit the choropleth format
combined = as_tibble(left_join(region, avg_sd, by = "country" ))
mean_table = select(combined, region = name, value = mean)
mean_table[is.na(mean_table['value']), 2] = 0

## Create the average choropleth map
country_choropleth(mean_table, num_colors = 9)

sd_table = select(combined, region = name, value = sd)
sd_table[is.na(sd_table['value']), 2]  = 0

## Create the standard deviation choropleth map
country_choropleth(sd_table, num_colors = 9)

## Next stage try build linear regression model with several variables

## Download world population data
pop.html = "https://www.indexmundi.com/g/r.aspx?t=0&v=21&l=en" %>% read_html()
population = pop.html %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE)
pop = as_tibble(population[[3]][,c(2,3)])

## Switch the character format to double
pop['Population'] = pop['Population'] %>%
  unlist() %>%
  str_remove_all(',') %>% 
  as.numeric()/1000000
pop['Country'] = pop['Country'] %>%
  unlist() %>%
  tolower()
colnames(pop)[1] = 'region'
## join the pm25 data with population
value_pop = left_join(mean_table, pop, by = "region") %>% na.exclude()

## Download world human development data
hdi.html = "https://countryeconomy.com/hdi" %>% read_html()
hd = hdi.html %>%
  html_nodes("table[id = tb1]") %>% 
  html_table()
hdi = hd[[1]][,c(1,2)]
hdi['Countries'] = hdi['Countries'] %>% 
  unlist() %>%
  str_remove_all(' \\[\\+\\]') %>%
  tolower()

hdi['HDI'] = -1/log(hdi['HDI'])
colnames(hdi)[1] = 'region'
pop_hdi = na.exclude(left_join(value_pop, hdi, by = "region"))
  