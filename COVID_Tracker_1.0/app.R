library(shiny)
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
library(maps)
library(mapproj)
source('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/custom_theme2.R')

dataL <- readRDS('combined_long.rds')
map_file <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID_Tracker_1.0/combined_map_files.rds')
# nation_map <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID_Tracker_1.0/nation_map.rds')
# ca_counties_map <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID_Tracker_1.0/ca_counties_maps.rds')
# states <- c(unique(dataL$State))
statesS <- c(unique(dataL$State[dataL$State != 'N/A']))
statesN <- c('N/A')
statesC <- c('California')
countiesC <- c(unique(dataL$County[dataL$County != 'N/A']))
countiesSN <- c('N/A')


# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("COVID-19 Trend Tracker"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("granularity"),
            uiOutput("state"),
            uiOutput("county"),
        
        selectInput(
            inputId = "metric",
            label = h5("Choose Metric to Display"),
            choices = c(unique(dataL$Metric)),
            selected = 'New Cases'
            )
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("map"),
            plotlyOutput("COVIDplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Drop-down selection box for which data set
    output$granularity <- renderUI({
        selectInput(
            inputId = "granularity",
            label = h5("Choose Regional Level to Display"),
            choices = c(
                "Nation",
                "State",
                "County (California Only)"),
            selected = 'Nation')
        })
    
    output$state <- renderUI({
        statelist <- if (input$granularity == 'Nation') {
            statesN
        } else if (input$granularity == 'State') {
            statesS
        } else statesC
        
        selectInput(
            inputId = "state",
            label = h5("Choose State/Territory to Display"),
            choices = statelist,
            selected = NULL
            )
    })
    
    output$county <- renderUI({
        countylist <- if (input$granularity == 'Nation') {
            countiesSN
        } else if (input$granularity == 'State') {
            countiesSN
        } else countiesC
        
        selectInput(
            inputId = "county",
            label = h5("Choose County to Display"),
            choices = countylist,
            selected = NULL
        )
    })
    
    ############# COVID Trend Plots ---------------------
    output$COVIDplot <- renderPlotly({
        df <- dataL[dataL$`Nation/State/County` == input$granularity &
                        dataL$State == input$state &
                        dataL$County == input$county &
                        dataL$Metric == input$metric,]
        
        ggplotly(
            ggplot(df, aes(x = Date, y = Value)) +
                geom_area(fill = "#69b3a2", alpha = 0.5) +
                geom_line(color = "#69b3a2") +
                geom_smooth(
                    method = "loess",
                    formula = y ~ x,
                    se = FALSE,
                    colour = "#A269B3",
                    alpha = 0.5,
                    size = 0.5
                ) +
                ylab(Metric) +
                ylim(0, max(df$Value) * 1.05) +
                theme_Custom()
        )
        
    })
    ################################ mapping  --------------------------
    
    ####### select region ----------------------
    
    
    

    ### selected region fill ----------
    selection.fill <- c("1" = "#69b3a2",
                        "0" = "Gray95")
    
    output$map <- renderPlot({
        a <- map_file[map_file$granularity == input$granularity, ]
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
            theme(
                axis.title = element_blank(),
                axis.line = element_blank(),
                panel.background = element_blank(),
                panel.grid = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position = 'none',
                plot.margin = margin(0, 0, 0, 0)
            ) +
            scale_fill_manual(values = selection.fill)
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
