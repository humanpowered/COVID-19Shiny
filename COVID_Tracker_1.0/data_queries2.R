library(httr)
library(tidyverse)
library(jsonlite)
library(data.table)
library(seasonal)
library(forecast)
library(shiny)
library(tidycensus)
# setwd("~/R Projects/COVID19Shiny/COVID_Tracker_1.0")

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
ns <- ns %>% rename(total_hospitalized = hospitalizedCumulative)

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
data$updated <- Sys.Date()
data2 <- data[, c('date', 'granularity', 'state', 'county', 'fips', 'updated', 'dataQualityGrade', 'total_confirmed_cases', 'new_confirmed_cases', 'total_hospitalized', 'new_hospitalized', 'total_deaths', 'new_deaths')]


abbrlkp <- read_csv('state_territory_lookup.csv')
data3 <- left_join(data2, abbrlkp, by = 'state')

names(data3) <- c('Date', 'Nation/State/County', 'state', 'County', 'fips', 'Last Update', 'Data Quality Grade', 'Cases Cumulative', 'New Cases', 'Hospitalized Cumulative*', 'New Hospitalized*', 'Deaths Cumulative', 'New Deaths', 'State')

data3 <- data3[, c(c('Date', 'Nation/State/County', 'State', 'state', 'County', 'fips', 'Last Update', 'Data Quality Grade', 'Cases Cumulative', 'New Cases', 'Hospitalized Cumulative*', 'New Hospitalized*', 'Deaths Cumulative', 'New Deaths'))]

data3$State[is.na(data3$State)] <- 'N/A'
 
cases <- data3[, c(1:10)]
cases2 <- cases[cases$`Cases Cumulative` >= 2,]
casesL <- cases2 %>%
  pivot_longer(-c(Date, `Nation/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
               names_to = 'Metric',
               values_to = 'Value')

hosp <- data3[, c(1:8, 11, 12)]
hosp2 <- hosp[hosp$`Hospitalized Cumulative*` >= 1,]
hospL <- hosp2 %>%
  pivot_longer(-c(Date, `Nation/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
               names_to = 'Metric',
               values_to = 'Value')


deaths <- data3[, c(1:8, 13, 14)]
deaths2 <- deaths[deaths$`Deaths Cumulative` >= 1,]
deathsL <- deaths2 %>%
  pivot_longer(-c(Date, `Nation/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
               names_to = 'Metric',
               values_to = 'Value')

dataL <- bind_rows(casesL, hospL, deathsL)

dataL$`Nation/State/County` <- gsub('County', 'County (California Only)', dataL$`Nation/State/County`, fixed = T)

dataL2 <- dataL[which(dataL$`Nation/State/County` != 'NA'),]

saveRDS(dataL2, 'combined_long.rds')
