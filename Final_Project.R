## To use the API functions one should first install the ropenaq package

## It may take a while to download the data using aq_latest()

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
## A test, download latest data in Mongolia with the pm25 measurement
MN_latest = aq_latest(country = 'MN', parameter = 'pm25')
glimpse(MN_latest)

latest = aq_latest(parameter = 'pm25')

processed = latest %>% select(location, country, parameter, value, lastUpdated) %>%
  filter(year(lastUpdated) >= 2017) 

## Find 10 countries with most records
most = processed %>% group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(n = 10) %>% ungroup()

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
mean_table1 = select(combined, region = name, value = mean)

## Create the average choropleth map
country_choropleth(mean_table, num_colors = 9, title = 'PM2.5 Average Concentration Map')

sd_table = select(combined, region = name, value = sd)
sd_table[is.na(sd_table['value']), 2]  = 0

## Create the standard deviation choropleth map
country_choropleth(sd_table, num_colors = 9, title = 'PM2.5 Standard Deviation Concentration Map')

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
value_pop = left_join(mean_table1, pop, by = "region") %>% na.exclude()

## Download world human development data (HDI)
hdi.html = "https://countryeconomy.com/hdi" %>% read_html()
hd = hdi.html %>%
  html_nodes("table[id = tb1]") %>% 
  html_table()
hdi = hd[[1]][,c(1,2)]
hdi['Countries'] = hdi['Countries'] %>% 
  unlist() %>%
  str_remove_all(' \\[\\+\\]') %>%
  tolower()

## Scaling the HDI by taking 1-/log(hdi)
hdi['HDI'] = -1/log(hdi['HDI'])
colnames(hdi)[1] = 'region'

## Combine the hdi data with population and PM2.5 concentration data
pop_hdi = na.exclude(left_join(value_pop, hdi, by = "region"))
pop_hdi$value[pop_hdi$value < 0] = 0

## Label the popualtion and HDI based on the value
q_pop = quantile(pop_hdi$Population, seq(0.1, 1, 0.1), names = FALSE)
q_hdi = quantile(c(min(pop_hdi['HDI']),max(pop_hdi['HDI'])), seq(0.1, 1, 0.1), names = FALSE)

## Tag the country with the quantile of population and hdi
pop_hdi_new = pop_hdi %>% 
  mutate(pop_quantile = case_when(Population <= q_pop[1] ~ '10%',
                                       Population <= q_pop[2] & Population > q_pop[1] ~ '20%',
                                       Population <= q_pop[3] & Population > q_pop[2] ~ '30%',
                                       Population <= q_pop[4] & Population > q_pop[3] ~ '40%',
                                       Population <= q_pop[5] & Population > q_pop[4] ~ '50%',
                                       Population <= q_pop[6] & Population > q_pop[5] ~ '60%',
                                       Population <= q_pop[7] & Population > q_pop[6] ~ '70%',
                                       Population <= q_pop[8] & Population > q_pop[7] ~ '80%',
                                       Population <= q_pop[9] & Population > q_pop[8] ~ '90%',
                                       Population > q_pop[9] ~ '100%')) %>%
  mutate(hdi_quantile = case_when(HDI <= q_hdi[1] ~ '10%',
                             HDI <= q_hdi[2] & HDI > q_hdi[1] ~ '20%',
                             HDI <= q_hdi[3] & HDI > q_hdi[2] ~ '30%',
                             HDI <= q_hdi[4] & HDI > q_hdi[3] ~ '40%',
                             HDI <= q_hdi[5] & HDI > q_hdi[4] ~ '50%',
                             HDI <= q_hdi[6] & HDI > q_hdi[5] ~ '60%',
                             HDI <= q_hdi[7] & HDI > q_hdi[6] ~ '70%',
                             HDI <= q_hdi[8] & HDI > q_hdi[7] ~ '80%',
                             HDI <= q_hdi[9] & HDI > q_hdi[8] ~ '90%',
                             HDI > q_hdi[9] ~ '100%'))

tile = pop_hdi_new %>% group_by(pop_quantile, hdi_quantile) %>%
  summarise(mean = sqrt(mean(value)))
tile[is.na(tile['mean']),3] = 0

## Use quantile as the tag to create heatmap and transform them to ordered factor format
tile$pop_quantile = factor(tile$pop_quantile, levels=c('10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%'), ordered=TRUE)
tile$hdi_quantile = factor(tile$hdi_quantile, levels=c('10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%'), ordered=TRUE)

## Create heat map
heatmap = ggplot(tile, aes(pop_quantile, hdi_quantile), fill = mean) + 
  geom_tile(aes(fill = mean)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "population_quantile", y = "hdi_quantile", title = "Heatmap: population and hdi")
heatmap

## Try linear regression with HDI as predictor
hdi.fit = lm(value ~ HDI, data = pop_hdi)
summary(hdi.fit)
  