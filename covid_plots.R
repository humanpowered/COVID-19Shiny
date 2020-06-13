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


p <- data4[data4$`National/State/County` == Granularity & data4$State == State & data4$County == County & data4$Metric == Metric,] %>%
  ggplot( aes(x = Date, y = Value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#A269B3", alpha = 0.5, size = 0.5) +
  ylab(Metric) +
  ylim(0, max(data4$Value[data4$`National/State/County` == Granularity & data4$State == State & data4$County == County & data4$Metric == Metric])) +
  theme_Custom(); p <- ggplotly(p); p


# metric.colors <- c(
#   `Total Cases` = "#69b3a2",
#   `New Cases` = "#69b3a2",
#   `Total Deaths` = "#69b3a2",
#   `New Deaths` = "#69b3a2",
#   `Total Cases (adj.)` = "#A269B3",
#   `New Cases (adj.)` = "#A269B3",
#   `Total Deaths (adj.)` = "#A269B3",
#   `New Deaths (adj.)` = "#A269B3"
# )
# 
# metric.fill <- c(
#   `Total Cases` = "#69b3a2",
#   `New Cases` = "#69b3a2",
#   `Total Deaths` = "#69b3a2",
#   `New Deaths` = "#69b3a2",
#   `Total Cases (adj.)` = "",
#   `New Cases (adj.)` = "00",
#   `Total Deaths (adj.)` = "#ffffff00",
#   `New Deaths (adj.)` = "#ffffff00"
# )
# 
# metric.alpha <- c(
#   `Total Cases` = 0.5,
#   `New Cases` = 0.5,
#   `Total Deaths` = 0.5,
#   `New Deaths` = 0.5,
#   `Total Cases (adj.)` = 0.5,
#   `New Cases (adj.)` = 0.5,
#   `Total Deaths (adj.)` = 0.5,
#   `New Deaths (adj.)` = 0.5
# )
# p <- dataL[dataL$`National/State/County` == Granularity & dataL$State == State & dataL$County == County & dataL$MetricType == Metric,] %>%
#   ggplot(
#     aes(
#       x = Date,
#       color = Metric,
#       # fill = Metric,
#       alpha = Metric
#     )) +
#   # geom_area(aes(y = Value)) +
#   geom_line(aes(y = Value)) +
#   # scale_fill_manual(values = metric.fill) +
#   scale_color_manual(values = metric.colors) +
#   scale_alpha_manual(values = metric.alpha) +
#   ylab(Metric) +
#   theme_ipsum(); p <- ggplotly(p); p


