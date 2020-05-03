
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
