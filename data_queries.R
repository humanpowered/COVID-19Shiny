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
us$granularity <- 'National'
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

names(data3) <- c('Date', 'National/State/County', 'state', 'County', 'fips', 'Last Update', 'Data Quality Grade', 'Total Cases', 'New Cases', 'Total Deaths', 'New Deaths', 'State')

data3 <- data3[, c(c('Date', 'National/State/County', 'State', 'state', 'County', 'fips', 'Last Update', 'Data Quality Grade', 'Total Cases', 'New Cases', 'Total Deaths', 'New Deaths'))]

data3$State[is.na(data3$State)] <- 'N/A'

cases <- data3[, c(1:10)]
cases2 <- cases[cases$`Total Cases` >= 2,]
casesL <- cases2 %>%
  pivot_longer(-c(Date, `National/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
               names_to = 'Metric',
               values_to = 'Value')


deaths <- data3[, c(1:8, 11, 12)]
deaths2 <- deaths[deaths$`Total Deaths` >= 1,]
deathsL <- deaths2 %>%
  pivot_longer(-c(Date, `National/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
               names_to = 'Metric',
               values_to = 'Value')

data4 <- bind_rows(casesL, deathsL)

# data4 <- data3 %>%
#   pivot_longer(-c(Date, `National/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
#                names_to = 'Metric',
#                values_to = 'Value')





################################# time series ----------------------------------------------
# dts <- data4
# dts$lookup <- paste(dts$`National/State/County`, dts$state, dts$County, dts$Metric, sep = "_")
# 
# ts <- list()
# for(i in unique(dts$lookup)){
#   ts[[i]] <- ts(dts$Value[dts$lookup == i], frequency = 7)
# }
# 
# seasons <- list()
# for(i in unique(dts$lookup)){
#   seasons[[i]]<- ets(ts[[i]]
#   )
# }
# 
# seasls <- list()
# for(i in unique(dts$lookup)){
#   seasls[[i]] <- data.table(seasons[[i]]$fitted)
#   seasls[[i]]$Actual <- dts$Value[dts$lookup == i]
#   seasls[[i]]$Date <- dts$Date[dts$lookup == i]
#   seasls[[i]]$lookup <- dts$lookup[dts$lookup == i]
# }
# 
# seas_table <- bind_rows(seasls[1:length(seasls)])
# seas_table$V1 <- round(seas_table$V1, 0)
# seas_table2 <- left_join(seas_table, dts[, c('Date', 'National/State/County', 'State', 'state', 'County', 'lookup', 'Metric')], by = c('Date', 'lookup'))
# seas_table2$lookup <- NULL
# seas_table2$Metric <- paste(seas_table2$Metric, '(adj.)', sep = ' ')
# seas_table2 <- seas_table2 %>% rename(Value = V1)
# seas_tableW <- seas_table2 %>%
#   pivot_wider(names_from = Metric,
#               values_from = Value,
#               id_cols = c(Date, `National/State/County`, State, state, County)
#   )

# arima <- list()
# for(i in unique(dts$lookup)){
#   arima[[i]]<- auto.arima(ts[[i]], approximation=FALSE,trace=FALSE
#   )
# }
# 
# arimals <- list()
# for(i in unique(dts$lookup)){
#   arimals[[i]] <- data.table(arima[[i]]$fitted)
#   arimals[[i]]$Actual <- dts$Value[dts$lookup == i]
#   arimals[[i]]$Date <- dts$Date[dts$lookup == i]
#   arimals[[i]]$lookup <- dts$lookup[dts$lookup == i]
# }
# 
# arima_table <- bind_rows(arimals[1:length(arimals)])
# arima_table$V1 <- round(arima_table$V1, 0)
# arima_table2 <- left_join(arima_table, dts[, c('Date', 'National/State/County', 'State', 'state', 'County', 'lookup', 'Metric')], by = c('Date', 'lookup'))
# arima_table2$lookup <- NULL
# arima_table2$Metric <- paste(arima_table2$Metric, '(adj.)', sep = ' ')
# arima_table2 <- arima_table2 %>% rename(Value = V1)
# arima_tableW <- arima_table2 %>%
#   pivot_wider(names_from = Metric,
#               values_from = Value,
#               id_cols = c(Date, `National/State/County`, State, state, County)
#   )

dataW <- left_join(data3, seas_tableW, by = c('Date', 'National/State/County', 'State', 'state', 'County'))
dataL <- dataW %>%
  pivot_longer(-c(Date, `National/State/County`, State, state, County, fips, `Last Update`, `Data Quality Grade`),
                            names_to = 'Metric',
                            values_to = 'Value')

############## create metric pairs for actual and adjusted ---------------------
dataL$MetricType <- ifelse(dataL$Metric == 'Total Cases (adj.)', 'Total Cases',
                           ifelse(dataL$Metric == 'New Cases (adj.)', 'New Cases',
                                  ifelse(dataL$Metric == 'Total Deaths (adj.)', 'Total Deaths',
                                         ifelse(dataL$Metric == 'New Deaths (adj.)', 'New Deaths', dataL$Metric
                                         )
                                  )
                           )
)



write_csv(county, 'ca_counties_daily.csv')
write_csv(states, 'states_daily.csv')
write_csv(us, 'National_daily.csv')
write_csv(data3, 'combined_state_county.csv')
write_csv(dataW, 'combined_state_county_long_w_adj.csv')
