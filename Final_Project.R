
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
processed %>% select(country, value) %>%
  group_by(country) %>%
  summarise(mean = mean(value), sd = sd(value, na.rm = TRUE))
