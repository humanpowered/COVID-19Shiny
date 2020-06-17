#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
state_maps <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID-19_Tracker/state_maps.rds')
nation_map <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID-19_Tracker/nation_map.rds')
ca_counties_map <- readRDS('C:/Users/craig/OneDrive/Documents/R Projects/COVID19Shiny/COVID-19_Tracker/ca_counties_maps.rds')
states <- c(unique(dataL$State))
statesS <- c(unique(dataL$State[which(dataL$State != 'N/A')]))
statesN <- c('N/A')
stateC <- c('California')
countiesC <- c(unique(dataL$County))
countiesSN <- 'N/A'


# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("COVID-19 Trend Tracker"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "gran_select",
                label = h5("Choose Regional Level to Display"),
                choices = c(
                    "Nation",
                    "State",
                    "County"
                ),
                selected = "Nation"),
            selectInput(
                inputId = "state",
                label = h5("Choose State/Territory to Display"),
                choices = states),
                # selected = 'N/A'),
            selectInput(
                inputId = "county",
                label = h5("Choose County to Display (California only)"),
                choices = counties),
                # selected = 'N/A'),
            selectInput(
                inputId = "metric",
                label = h5("Choose Metric to Display"),
                choices = c(unique(dataL$Metric)),
                selected = 'New Cases')
            ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("cp"),
            plotlyOutput("COVIDplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observe({
        # states2 <- if (input$gran_select == 'Nation') {
        #     "N/A"
        # } else if (input$gran_select == 'County') {
        #     "California"
        # } else
        #     states
        # 
        # counties2 <- if (input$gran_select == 'Nation') {
        #     "N/A"
        # } else if (input$gran_select == 'State') {
        #     "N/A"
        # } else
        #     counties
        
        updateSelectInput(session,
                          inputId = "state",
                          choices = if (input$gran_select == 'State') {
                              statesS
                          }
                          else if (input$gran_select == 'Nation') {
                              states[1]
                          }
                          else 
                              states[6],
                          selected = states
        )
        
        updateSelectInput(session,
                          inputId = "county",
                          choices = countiesC)
        
        state_maps$selection <-
            if (input$gran_select == 'State' &
                state_maps$region == tolower(input$state)) {
                "1"
            } else {
                "0"
            }
        
        ca_counties_map$selection <-
            if (input$gran_select == 'County' & 
                ca_counties_map$subregion == tolower(input$county)) {
                "1"
            } else {
                "0"
            }
        
        map <- if (input$gran_select == 'Nation') {
            "np"
        } else if (input$gran_select == 'State') {
            "sp"
        } else {
            "cp"
        }
        
    })
    
    
    output$COVIDplot <- renderPlotly({
        df <- dataL[dataL$`Nation/State/County` == input$gran_select &
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
    
    
    ################################ mapping
    
    selection.fill <- c("1" = "#69b3a2",
                        "0" = "Gray95")
    
    nation_map$selection <- "1"
    
    
    ################## National Map -------------------------
    # output$np <- renderPlot({
    #     ggplot(data = nation_map,
    #            aes(
    #                x = long,
    #                y = lat,
    #                group = group,
    #                fill = selected
    #            )) +
    #         geom_polygon(color = "gray90", size = 0.1) +
    #         coord_map(
    #             projection = "albers",
    #             lat0 = 39,
    #             lat1 = 45
    #         ) +
    #         theme(
    #             axis.title = element_blank(),
    #             axis.line = element_blank(),
    #             panel.background = element_blank(),
    #             panel.grid = element_blank(),
    #             axis.text = element_blank(),
    #             axis.ticks = element_blank(),
    #             legend.position = 'none',
    #             plot.margin = margin(0, 0, 0, 0)
    #         ) +
    #         scale_fill_manual(values = selection.fill)
    # })
    # 
    # 
    # ################## State Map ------------------------------
    # output$sp <- renderPlot({
    #     ggplot(data = state_maps,
    #            aes(
    #                x = long,
    #                y = lat,
    #                group = group,
    #                fill = selection
    #            )) +
    #         geom_polygon(color = "gray90", size = 0.1) +
    #         coord_map(
    #             projection = "albers",
    #             lat0 = 39,
    #             lat1 = 45
    #         ) +
    #         theme(
    #             axis.title = element_blank(),
    #             axis.line = element_blank(),
    #             panel.background = element_blank(),
    #             panel.grid = element_blank(),
    #             axis.text = element_blank(),
    #             axis.ticks = element_blank(),
    #             legend.position = 'none',
    #             plot.margin = margin(0, 0, 0, 0)
    #         ) +
    #         scale_fill_manual(values = selection.fill)
    # })
    # 
    # ################### County Map ---------------------------
    # output$cp <- renderPlot({
    #     ggplot(data = ca_counties_map,
    #            aes(
    #                x = long,
    #                y = lat,
    #                group = group,
    #                fill = selection
    #            )) +
    #         geom_polygon(color = "gray90", size = 0.1) +
    #         coord_map(
    #             projection = "albers",
    #             lat0 = 39,
    #             lat1 = 45
    #         ) +
    #         theme(
    #             axis.title = element_blank(),
    #             axis.line = element_blank(),
    #             panel.background = element_blank(),
    #             panel.grid = element_blank(),
    #             axis.text = element_blank(),
    #             axis.ticks = element_blank(),
    #             legend.position = 'none',
    #             plot.margin = margin(0, 0, 0, 0)
    #         ) +
    #         scale_fill_manual(values = selection.fill)
    # })
    # 
}

# Run the application 
shinyApp(ui = ui, server = server)
