library(tidyverse)
library(data.table)
library(ggplot2)
library(ggseas)
library(ggthemes)
library(scales)
library(fpp)
library(forecast)
library(shiny)
library(beepr)
library(extrafont)
library(plotly)
library(hrbrthemes)
library(openintro)
source('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/custom_theme2.R')

dataL <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID-19_Tracker/combined_long.rds')

Granularity <- 'County'
State <- 'California'
County <- 'Los Angeles'
Metric <- 'New Cases'


p <- dataL[dataL$`Nation/State/County` == Granularity & dataL$State == State & dataL$County == County & dataL$Metric == Metric,] %>%
  ggplot( aes(x = Date, y = Value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#A269B3", alpha = 0.5, size = 0.5) +
  ylab(Metric) +
  ylim(0, max(dataL$Value[dataL$`Nation/State/County` == Granularity & dataL$State == State & dataL$County == County & dataL$Metric == Metric]) * 1.05) +
  theme_Custom(); p <- ggplotly(p); p


 ggplotly(dataL[dataL$`Nation/State/County` == Granularity & dataL$State == State & dataL$County == County & dataL$Metric == Metric,] %>%
  ggplot( aes(x = Date, y = Value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#A269B3", alpha = 0.5, size = 0.5) +
  ylab(Metric) +
  ylim(0, max(dataL$Value[dataL$`Nation/State/County` == Granularity & dataL$State == State & dataL$County == County & dataL$Metric == Metric]) * 1.05) +
  theme_Custom())

 
 df <- dataL[dataL$gran_num == 2 &
              dataL$statenum == 6 &
              dataL$countynum == 1 & dataL$metricnum == 1, ]
 
 ggplotly(
            ggplot(df, aes(x = Date, y = Value)) +
            geom_area(fill="#69b3a2", alpha=0.5) +
            geom_line(color="#69b3a2") +
            geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#A269B3", alpha = 0.5, size = 0.5) +
            ylab(Metric) +
            ylim(0, max(dataL$Value[dataL$`Nation/State/County` == Granularity & dataL$State == State & dataL$County == County & dataL$Metric == Metric]) * 1.05) +
            theme_Custom())

