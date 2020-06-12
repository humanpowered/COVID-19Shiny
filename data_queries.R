library(httr)
library(tidyverse)
library(jsonlite)
library(data.table)

res <- GET("https://covidtracking.com/api/v1/states/daily.json")
states <- fromJSON(rawToChar(res$content))
states <- setorder(states, state, date)
states$granularity <- 'State'
states$date <- as.character(states$date)
states$date <- as.Date(states$date, '%Y%m%d')

states <- states %>% rename(new_confirmed_cases = positiveIncrease)
states <- states %>% rename(new_deaths = deathIncrease)

county <- read_csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv")
county <- setorder(county, county, date)
county$granularity <- 'County'

data <- bind_rows(states, county)

write_csv(county, 'ca_counties_daily.csv')
write_csv(states, 'states_daily.csv')
write_csv(data, 'combined_state_county.csv')

