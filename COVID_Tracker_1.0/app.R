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
source('custom_theme2.R')



dataL <- readRDS('combined_long.rds')

if (max(dataL$`Last Update`) < Sys.Date()) {
    source('data_queries2.R')
    dataL <- readRDS('combined_long.rds')
}

map_file <- readRDS('combined_map_files.rds')
statesS <- c(unique(dataL$State[dataL$State != 'N/A']))
statesN <- c('N/A')
statesC <- c('California')
countiesC <- c(unique(dataL$County[dataL$County != 'N/A']))
countiesSN <- c('N/A')
metricC <- c('Cases Cumulative', 'New Cases', 'Deaths Cumulative', 'New Deaths')
metricSN <- c(unique(dataL$Metric))

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
      "* Hospitalization reporting is not consistent across states and currently not available for California counties. National hospitalization data will not include counts from states that are not reporting hospitalization data, use caution when interpreting these data",
      tags$br(),
      tags$br(),
      "Sources:",
      tags$br(),
      "National & State Data: The Covid Tracking Project ",
      tags$a(href = "https://covidtracking.com/api", "https://covidtracking.com/api"),
      tags$br(),
      "California County Data: LA Times - Tracking the cornavirus in California ",
      tags$a(
        href = "https://github.com/datadesk/california-coronavirus-data",
        "https://github.com/datadesk/california-coronavirus-data"
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
                        "County (California Only)"),
            selected = 'Nation'
        )
    })
    output$state <- renderUI({
        statelist <- if (input$granularity == 'Nation') {
            statesN
        } else if (input$granularity == 'State') {
            statesS
        } else
            statesC
        
        selectInput(
            inputId = "state",
            label = h5("State/Territory"),
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
            countiesC
        
        selectInput(
            inputId = "county",
            label = h5("County (California Only)"),
            choices = countylist,
            selected = NULL
        )
    })
    output$metric <- renderUI({
      metriclist <- if (input$granularity == 'County (California Only)') {
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
                geom_area(fill = "#69b3a2", alpha = 0.5) +
                geom_line(color = "#69b3a2", show.legend = T) +
                geom_smooth(
                    method = "loess",
                    formula = y ~ x,
                    se = FALSE,
                    colour = "#A269B3",
                    alpha = 0.5,
                    size = 0.5,
                    show.legend = T
                ) +
                ggtitle(paste(
                    input$metric,
                    "-",
                    ifelse(
                        input$granularity == 'Nation',
                        'Nation',
                        ifelse(
                            input$granularity == 'State',
                            input$state,
                            input$county
                        )
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
                    a$granularity == 'County (California Only)' &
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
    
    ################# Export Table of selected dataset ----
    # datasetInput <- reactive({
    #   
    # })
    
    # selectedRegion <- reactive({
    #   paste(input$granularity, "_", input$state, "_", input$county, "_", input$metric, sep = "")
    # })
    # 
    # datasetInput <- reactive({
    #   dataL[dataL$`Nation/State/County` == input$granularity &
    #           dataL$State == input$state &
    #           dataL$County == input$county &
    #           dataL$Metric == input$metric, ]
    # })
    
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
