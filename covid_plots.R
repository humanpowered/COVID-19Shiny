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
library(formattable)
library(plotly)
library(hrbrthemes)
source('C:/Users/craig/OneDrive/Documents/NPA Report/custom_theme.R')

data3 <- read_csv('combined_state_county.csv')
data4 <- data3 %>%
  pivot_longer(-c(Date, `National/State/County`, State, County, fips, `Last Update`, `Data Quality Grade`),
                      names_to = 'Metric',
                      values_to = 'Value')

Granularity <- 'County'
County <- 'Los Angeles'
Metric <- 'New Cases'


p <- data4[data4$County == County & data4$Metric == Metric,] %>%
  ggplot( aes(x = Date, y = Value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab(Metric) +
  theme_ipsum()
p <- ggplotly(p)
p
