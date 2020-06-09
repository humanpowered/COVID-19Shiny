library(httr)
library(tidyverse)
library(jsonlite)

res <- GET("https://covidtracking.com/api/v1/states/daily.json")
states <- fromJSON(rawToChar(res$content))

county <- read_csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv")

