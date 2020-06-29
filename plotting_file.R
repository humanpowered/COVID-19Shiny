library(shiny)
library(ggplot2)
library(ggthemes)
library(scales)
library(fpp)
library(shiny)
library(extrafont)
library(plotly)
library(hrbrthemes)
library(openintro)
library(maps)
library(mapproj)
library(shinythemes)
library(shinyBS)
library(scales)
source('custom_theme2.R')

dataL <- readRDS('combined_long.rds')
map_file <- readRDS('combined_map_files.rds')
selection.fill <- c("1" = "#69b3a2",
                    "0" = "Gray95")



granularity <- 'State'
state <- 'Texas'
county <- 'N/A'
metric <- 'New Cases'

df <- dataL[dataL$`Nation/State/County` == granularity &
              dataL$State == state &
              dataL$County == county & 
              dataL$Metric == metric,]


ggplotly(
  ggplot(df, aes(x = Date, y = Value)) +
    geom_area(fill = "#69b3a3", alpha = 0.5, show.legend = F) +
    geom_line(aes(color = "Daily Trend"), show.legend = T) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      aes(colour = "Smoothed Trend"),
      alpha = 0.5,
      size = 0.5,
      span = 1
    ) +
    scale_color_manual(name = "Legend",
                         breaks = c("Daily Trend", "Smoothed Trend"),
                         values = c("Daily Trend" = "#69b3a2", "Smoothed Trend" = "#A269B3")
                       ) +
    ggtitle(paste(
      metric,
      "-",
      ifelse(
        granularity == 'Nation',
        'Nation',
        ifelse(granularity == 'State',
               state,
               county)
      ),
      sep = " "
    )) +
    ylab(metric) +
    ylim(0, max(df$Value) * 1.05) +
    theme_Custom(),
  dynamicTicks = T,
  showspikes = T,
  legend = F
)
################################ mapping  --------------------------

### selected region fill ----------



a <- map_file[map_file$granularity == granularity, ]
a$selection <- ifelse(
  a$granularity == 'Nation',
  "1",
  ifelse(
    a$granularity == 'State' & a$state == tolower(state),
    "1",
    ifelse(a$granularity == 'County' &
             a$county == tolower(county),
           "1",
           "0")
  )
)
  
ggplot(data = a,
       aes(
         x = long,
         y = lat,
         group = group,
         fill = selection
       )) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers",
            lat0 = 39,
            lat1 = 45) +
  scale_fill_manual(values = selection.fill) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none',
    plot.margin = margin(0, 0, 0, 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", color = "#4e5d6c"),
    plot.background = element_rect(fill = "transparent", color = "#4e5d6c")
  )


################## infection rate index ------------------- 


nat <- dataL[dataL$`Nation/State/County` == 'Nation' & dataL$Metric == '% Positive',]
nat$selected <- 'Nation'
reg <- dataL[dataL$`Nation/State/County` == granularity & dataL$State == state & dataL$County == county & dataL$Metric == '% Positive',]
reg$selected <- ifelse(granularity == 'Nation', 'Nation',
                    ifelse(granularity == 'State', reg$State, paste0(reg$County, ", ", reg$state)
                           )
)

infrate <- unique(bind_rows(nat, reg))
# infrate$Value <- percent_format(scale = 100)(infrate$Value)

# Region <- unique(reg$selected)


group.colors <- c("#69b3a2", "#A269B3")

ggplotly(
  ggplot(infrate, aes(x = Date, color = selected)) +
    # geom_area(fill = "#69b3a2", alpha = 0.5) +
    geom_line(aes(y = Value)) +
    scale_color_manual(values = group.colors) +
    ggtitle("Infection Rate vs National Average") +
    ylab(metric) +
    ylim(0, max(infrate$Value) * 1.05) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
    theme_Custom(),
  dynamicTicks = T,
  showspikes = T,
  showlegend = F
)
  