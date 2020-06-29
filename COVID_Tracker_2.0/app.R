library(shiny)
library(ggplot2)
library(tidyverse)
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
source('custom_theme2.R')

dataL <- readRDS('combined_long.rds')

if (max(dataL$`Last Update`) < Sys.Date()) {
  source('data_queries3.R')
  dataL <- readRDS('combined_long.rds')
}

map_file <- readRDS('combined_map_files.rds')
statesS <- c(unique(dataL$State[dataL$State != 'N/A']))
statesN <- c('N/A')
statesC <- c('California')

countiesSN <- c('N/A')
metricC <- c('Cases Cumulative', 'New Cases', 'Deaths Cumulative', 'New Deaths')
metricSN <- c('Cases Cumulative', 'New Cases', 'Hospitalized Cumulative*', 'New Hospitalized*', 'Deaths Cumulative', 'New Deaths')

# Define UI for application that draws a histogram
ui <- fluidPage(
  title = 'COVID-19 Trend Tracker',
  theme = shinytheme("superhero"),
  titlePanel(h1("COVID-19 Trend Tracker", align = 'center')),
  sidebarPanel(
    p(h4(
      "Select Region and Metric to Display", align = 'center'
    )),
    bsPopover(
      c("granularity", "state"),
      "Instructions",
      'Begin by selecting the region you&#39;d like to display from the 	<b>Region</b> menu	&#44; for example if you&#39;d like to display Arizona select <b>State</b> from the <b>Region</b> menu&#46; You will then be able to select Arizona from the <b>State&#47;Territory</b> menu',
      placement = "right",
      options = list(container = "body")
    ),
    uiOutput("granularity"),
    uiOutput("state"),
    uiOutput("county"),
    uiOutput("metric"),
    plotOutput("map")
    
    
  ),
  mainPanel(
    plotlyOutput("COVIDplot", inline = F),
    br(),
    br(),
    plotlyOutput("InfectionRate", inline = F),
    br(),
    br(),
    bsPopover(
      "downloadData",
      NULL,
      'Download data as csv',
      placement = "right",
      options = list(container = "body")
    ),
    downloadButton("downloadData", "Download"),
    
    br(),
    br(),
    p(" ")
  ),
  
  tags$footer(
    tags$small(
      tags$sup("\U2020"),
      "Infection Rate is defined as the cumulative number of positive cases for the selected region divided by the population of the selected region",
      tags$br(),
      "* Hospitalization reporting is not consistent across states and currently not available for California counties. National hospitalization data will not include counts from states that are not reporting hospitalization data, use caution when interpreting these data",
      tags$br(),
      tags$br(),
      "Sources:",
      tags$br(),
      "National & State COVID-19 Data: The Covid Tracking Project - ",
      tags$a(href = "https://covidtracking.com/api", "https://covidtracking.com/api"),
      tags$br(),
      "County COVID-19 Data: NY Times Coronavirus (Covid-19) Data in the United States - ",
      tags$a(href = "https://github.com/nytimes/covid-19-data", "https://github.com/nytimes/covid-19-data"),
      tags$br(),
      "Population Data: United States Census Bureau County Population Total 2010 - 2019 - ",
      tags$a(
        href = "https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html",
        "https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html"
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$granularity <- renderUI({
    selectInput(
      inputId = "granularity",
      label = h5("Region"),
      choices = c("Nation",
                  "State",
                  "County"),
      selected = 'Nation'
    )
  })
  output$state <- renderUI({
    statelist <- if (input$granularity == 'Nation') {
      statesN
    } else
      statesS
    
    selectInput(
      inputId = "state",
      label = h5("State"),
      choices = statelist,
      selected = NULL
    )
  })
  
  output$county <- renderUI({
    countylist <- if (input$granularity == 'Nation') {
      countiesSN
    } else if (input$granularity == 'State') {
      countiesSN
    } else
      c(unique(dataL$County[dataL$County != 'N/A' & dataL$State == input$state]))
    
    selectInput(
      inputId = "county",
      label = h5("County"),
      choices = countylist,
      selected = NULL
    )
  })
  output$metric <- renderUI({
    metriclist <- if (input$granularity == 'County') {
      metricC
    } else
      metricSN
    
    selectInput(
      inputId = "metric",
      label = h5("Metric"),
      choices = metriclist,
      selected = "New Cases"
    )
  })
  
  
  ############# COVID Trend Plots ---------------------
  
  output$COVIDplot <- renderPlotly({
    
    df <- dataL[dataL$`Nation/State/County` == input$granularity &
                  dataL$State == input$state &
                  dataL$County == input$county &
                  dataL$Metric == input$metric, ]
    shiny::validate(
      need(input$granularity != '', message = "Updating Plot..."),
      need(input$state != '', message = "Updating Plot..."),
      need(input$county != '', message = "Updating Plot..."),
      need(input$metric != '', message = "Updating Plot..."),
      need((input$metric != 'Hospitalized Cumulative*' | input$metric != 'New Hospitalized*') & nrow(df) > 0, 'Hospitalization data not available for this region')
    )
    
    
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
          span = 0.3
        ) +
        scale_color_manual(name = "Legend",
                           breaks = c("Daily Trend", "Smoothed Trend"),
                           values = c("Daily Trend" = "#69b3a2", "Smoothed Trend" = "#A269B3")
        ) +
        ggtitle(paste(
          input$metric,
          "-",
          ifelse(
            input$granularity == 'Nation',
            'Nation',
            ifelse(input$granularity == 'State',
                   input$state,
                   input$county)
          ),
          sep = " "
        )) +
        ylab(input$metric) +
        ylim(0, max(df$Value) * 1.05) +
        theme_Custom(),
      dynamicTicks = T,
      showspikes = T,
      showlegend = T
    )
    
  })
  ################################ mapping  --------------------------
  
  ### selected region fill ----------
  selection.fill <- c("1" = "#69b3a2",
                      "0" = "Gray95")
  
  output$map <- renderPlot({
    a <- map_file[map_file$granularity == input$granularity,]
    a$selection <- ifelse(
      a$granularity == 'Nation',
      "1",
      ifelse(
        a$granularity == 'State' & a$state == tolower(input$state),
        "1",
        ifelse(
          a$granularity == 'County' &
            a$state == tolower(input$state) &
            a$county == tolower(input$county),
          "1",
          "0"
        )
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
    
  }, bg = "transparent")
  
  
  
  ############# COVID Infection Rate Plot ---------------------
  
  output$InfectionRate <- renderPlotly({
    nat <- dataL[dataL$`Nation/State/County` == 'Nation' &
                   dataL$Metric == '% Positive', ]
    nat$selected <- 'Nation'
    reg <- dataL[dataL$`Nation/State/County` == input$granularity &
                   dataL$State == input$state &
                   dataL$County == input$county & dataL$Metric == '% Positive', ]
    reg$selected <- ifelse(input$granularity == 'Nation', 'Nation',
                           ifelse(input$granularity == 'State', reg$State,
                             paste0(reg$County, ", ", reg$state)
                           )
    )
    
    infrate <- unique(bind_rows(nat, reg))
    infrate$Value <- round(infrate$Value, 5)
    selection <- infrate$selected[infrate$selected != 'Nation']
    selection2 <- list(r1 = 'Nation',
                       r2 = unique(infrate$selected[infrate$selected != 'Nation']))
    
    ggplotly(
      ggplot(infrate, aes(x = Date, y = Value)) +
        geom_line(aes(color = selected)) +
        ggtitle(paste0(unique(selection), " Infection Rate", "<sup>&#10013;</sup>")) +
        ylab('Infection Rate') +
        ylim(0, max(infrate$Value) * 1.05) +
        scale_color_manual(name = "Legend",
                           breaks = c(selection2),
                           values = c("#A269B3", "#69b3a2")
        ) +
        theme_Custom(),
      dynamicTicks = T) %>%
      layout(yaxis = list(ticksuffix = "%"))
  })
    
  
  
  ############# Export Data ---------------------------
  output$downloadData <- downloadHandler(
    filename = 
      paste('COVID_Tracker_Data_', Sys.Date(), ".csv", sep = "")
    ,
    content = function(file) {
      write.csv(dataL, file, row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
