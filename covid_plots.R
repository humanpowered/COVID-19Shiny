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

dataL <- read_csv('combined_state_county_long_w_adj.csv')

Granularity <- 'County'
State <- 'California'
County <- 'Los Angeles'
Metric <- 'New Cases'


p <- data4[data4$`Nation/State/County` == Granularity & data4$State == State & data4$County == County & data4$Metric == Metric,] %>%
  ggplot( aes(x = Date, y = Value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#A269B3", alpha = 0.5, size = 0.5) +
  ylab(Metric) +
  ylim(0, max(data4$Value[data4$`Nation/State/County` == Granularity & data4$State == State & data4$County == County & data4$Metric == Metric]) * 1.05) +
  theme_Custom(); p <- ggplotly(p); p


