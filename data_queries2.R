library(httr)
library(tidyverse)
library(jsonlite)
library(data.table)
library(seasonal)
library(forecast)
library(shiny)

rus <- GET("https://covidtracking.com/api/v1/us/daily.json")
us <- fromJSON(rawToChar(rus$content))
us <- setorder(us, date)
us$granularity <- 'Nation'
us$state <- 'N/A'
us$county <- 'N/A'

rstate <- GET("https://covidtracking.com/api/v1/states/daily.json")
states <- fromJSON(rawToChar(rstate$content))
states <- setorder(states, state, date)
states$granularity <- 'State'
states$county <- 'N/A'

ns <- bind_rows(us, states)
ns$date <- as.character(ns$date)
ns$date <- as.Date(ns$date, '%Y%m%d')
ns <- ns %>% rename(total_confirmed_cases = positive)
ns <- ns %>% rename(total_deaths = death)
ns <- ns %>% rename(new_confirmed_cases = positiveIncrease)
ns <- ns %>% rename(new_deaths = deathIncrease)
ns <- ns %>% rename(new_hospitalized = hospitalizedIncrease)

county <- read_csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv")
county <- setorder(county, county, date)
county$granularity <- 'County'
county$state <- 'CA'
county <- county %>% rename(total_confirmed_cases = confirmed_cases)
county <- county %>% rename(total_deaths = deaths)
county$dataQualityGrade <- 'N/A'
county$new_confirmed_cases[is.na(county$new_confirmed_cases)] <- 0
county$new_deaths[is.na(county$new_deaths)] <- 0

data <- bind_rows(ns, county)
data$updated <- Sys.time()
data2 <- data[, c('date', 'granularity', 'state', 'county', 'fips', 'updated', 'dataQualityGrade', 'total_confirmed_cases', 'new_confirmed_cases', 'total_deaths', 'new_deaths')]
data2[, c('total_confirmed_cases', 'total_deaths')][is.na(data2[, c('total_confirmed_cases', 'total_deaths')])] <- 0

abbrlkp <- read_csv('state_territory_lookup.csv')
data3 <- left_join(data2, abbrlkp, by = 'state')

names(data3) <- c('Date', 'Nation/State/County', 'state', 'County', 'fips', 'Last Update', 'Data Quality Grade', 'Total Cases', 'New Cases', 'Total Deaths', 'New Deaths', 'State')

data3 <- data3[, c(c('Date', 'Nation/State/County', 'State', 'state', 'County', 'fips', 'Last Update', 'Data Quality Grade', 'Total Cases', 'New Cases', 'Total Deaths', 'New Deaths'))]

data3$State[is.na(data3$State)] <- 'N/A'

cases <- data3[, c(1:10)]
cases2 <- cases[cases$`Total Cases` >= 2,]
casesL <- cases2 %>%
  pivot_longer(-c(Date, `Nation/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
               names_to = 'Metric',
               values_to = 'Value')


deaths <- data3[, c(1:8, 11, 12)]
deaths2 <- deaths[deaths$`Total Deaths` >= 1,]
deathsL <- deaths2 %>%
  pivot_longer(-c(Date, `Nation/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
               names_to = 'Metric',
               values_to = 'Value')

dataL <- bind_rows(casesL, deathsL)

write_csv(county, 'ca_counties_daily.csv')
write_csv(states, 'states_daily.csv')
write_csv(us, 'Nation_daily.csv')
write_csv(data3, 'combined_state_county.csv')
write_csv(dataL, 'combined_state_county_long_w_adj.csv')
