library(httr)
library(tidyverse)
library(jsonlite)
library(data.table)
library(seasonal)
library(forecast)
library(shiny)
library(tidycensus)
# setwd("~/R Projects/COVID19Shiny/COVID_Tracker_2.0")

abbrlkp <- read_csv('state_territory_lookup.csv')
pop <- readRDS('population_data.rds')

rus <- GET("https://covidtracking.com/api/v1/us/daily.json")
us <- data.table(fromJSON(rawToChar(rus$content)))
us <- setorder(us, date)
us$granularity <- 'Nation'
us$state <- 'N/A'
us$county <- 'N/A'
us$State <- 'N/A'

rstate <- GET("https://covidtracking.com/api/v1/states/daily.json")
states <- data.table(fromJSON(rawToChar(rstate$content)))
states <- setorder(states, state, date)
states$granularity <- 'State'
states$county <- 'N/A'

states <- left_join(states, abbrlkp, by = 'state')

ns <- bind_rows(us, states)
ns$date <- as.character(ns$date)
ns$date <- as.Date(ns$date, '%Y%m%d')
ns <- ns %>% rename(total_confirmed_cases = positive)
ns <- ns %>% rename(total_deaths = death)
ns <- ns %>% rename(new_confirmed_cases = positiveIncrease)
ns <- ns %>% rename(new_deaths = deathIncrease)
ns <- ns %>% rename(new_hospitalized = hospitalizedIncrease)
ns <- ns %>% rename(total_hospitalized = hospitalizedCumulative)
ns$fips <- as.integer(ns$fips)

county <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
county$date <- as.Date(county$date)
county <- setorder(county, state, county, date)
county$granularity <- 'County'
# county$state <- 'CA'
county <- county %>%
  group_by(state, county) %>%
  mutate(new_confirmed_cases = cases - lag(cases, default = cases[1]))
county <- county %>%
  group_by(state, county) %>%
  mutate(new_deaths = deaths - lag(deaths, default = deaths[1]))

county <- county %>% rename(total_confirmed_cases = cases)
county <- county %>% rename(total_deaths = deaths)
county <- county %>% rename(State = state)
county$dataQualityGrade <- 'N/A'
county$total_hospitalized <- NA
county$new_hospitalized <- NA
county <- county[which(county$fips != 'NA'),]
county <- left_join(county, abbrlkp, by = 'State')
# county$new_confirmed_cases[is.na(county$new_confirmed_cases)] <- 0
# county$new_deaths[is.na(county$new_deaths)] <- 0
county$county <- gsub('DoÃ±a Ana', 'Doña Ana', county$county, fixed = T)

data <- bind_rows(ns, county)
data$updated <- Sys.Date()
data2 <- data[, c('date', 'granularity','State', 'state', 'county', 'fips', 'updated', 'dataQualityGrade', 'total_confirmed_cases', 'new_confirmed_cases', 'total_hospitalized', 'new_hospitalized', 'total_deaths', 'new_deaths')]

names(data2) <- c('Date', 'Nation/State/County', 'State', 'state', 'County', 'fips', 'Last Update', 'Data Quality Grade', 'Cases Cumulative', 'New Cases', 'Hospitalized Cumulative*', 'New Hospitalized*', 'Deaths Cumulative', 'New Deaths')

data3 <- left_join(data2, pop, by = c('Nation/State/County', 'State', 'County'))
data3 <- data3[which(data3$Population != 'NA'),]

data3$`% Positive` <- data3$`Cases Cumulative` / data3$Population * 100
data3$Population <- NULL

############# check for missing counties -------------
# data3$Population[is.na(data3$Population)] <- 'missing'
# nomatch <- unique(data3[, c('State', 'County')][data3$Population == 'missing'])

cases <- data3[, c(1:10, 15)]
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

# dataL$`Nation/State/County` <- gsub('County', 'County (California Only)', dataL$`Nation/State/County`, fixed = T)

# dataL2 <- dataL[which(dataL$`Nation/State/County` != 'NA'),]

# dataL <- data3 %>%
#   pivot_longer(-c(Date, `Nation/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
#                names_to = 'Metric',
#                values_to = 'Value')


#################### find states that don't report on hospitalization data -----------------
# hosp <- aggregate(`New Hospitalized*` ~ State, data3, sum)
# states_no_hosp <- unique(hosp$State[hosp$`New Hospitalized*` == 0])
# saveRDS(states_no_hosp, 'states_no_hosp_data.rds')

saveRDS(dataL, 'combined_long.rds')
