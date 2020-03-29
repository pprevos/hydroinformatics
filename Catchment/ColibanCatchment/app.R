##
## Coliban Water - Reservoir Levels
##

library(shiny)
library(readr)
library(shinydashboard)
library(stringr)
library(tidyverse)
library(DT)
library(leaflet)

rain <- read_csv("Catchment/ColibanCatchment/rain.csv")
streamflows <- read_csv("Catchment/ColibanCatchment/streamflows.csv")
reservoirs <- read_csv("Catchment/ColibanCatchment/reservoirs.csv")
stations <- read.csv("Catchment/ColibanCatchment/stations.csv")

## User Interface
ui <- dashboardPage(
    dashboardHeader(title = "Coliban Water Catchment Data Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
            menuItem("Meteorology", tabName = "meteo", icon = icon("cloud")),
            menuItem("Flows", tabName = "flows", icon = icon("stream")),
            menuItem("Reservoirs", tabName = "res", icon = icon("bath"))
        ),
        dateRangeInput('daterange',
                       label = "Date Range",
                       start = max(reservoirs$Date) - 28, max(reservoirs$Date),
                       max = max(reservoirs$Date)
                       )
    ),
    dashboardBody(
        tags$head(tags$style(".sidebar-menu li { margin-bottom: 20px; }")),
        tabItems(
            tabItem("overview",
                    h2("Overview"),
                    tags$div(p("This webiste presents data about rainfall, inflows and reservoir levels in the Coliban Water catchment."),
                             p("Click on the menu items on the left for more detailed information."),
                             p("Use the date selector on the left to change the reporting period."),
                             p("You can download data bu clicking the button below each graph."),
                             p("Data source: Department of Environment, Land, Water & Planning (DELWP)"),
                             tags$a(href = "http://data.water.vic.gov.au/", "data.water.vic.gov.au"),
                             p()),
                    valueBoxOutput("lauriston_today"),
                    valueBoxOutput("ucoliban_today"),
                    valueBoxOutput("malmsbury_today"),
                    p("Click on the dot to show the name of the measruement station."),
                    leafletOutput("map")
                    ),
            tabItem("meteo",
                    h2("Meteorology"),
                    checkboxGroupInput("rain_select", 
                                       label = "Select locations to Visualise",
                                       choiceNames = unique(rain$Reservoir),
                                       choiceValue = unique(rain$Reservoir),
                                       selected = rain$Reservoir,
                                       inline = TRUE),
                    radioButtons("rain_cum", "Daily or cumulative rainfall",
                                 choices = c("Daily", "Cumulative"),
                                 selected = "Daily"),
                    plotOutput("rainfall_graph"),
                    downloadButton("download_rain", label = "Download data"),
                    dataTableOutput("tbl_rain")
                    ),
            tabItem("flows",
                    h2("Stream Flows"),
                    checkboxGroupInput("flow_select", 
                                       label = "Select locations to Visualise",
                                       choiceNames = unique(streamflows$CWName),
                                       choiceValue = unique(streamflows$CWName),
                                       selected = streamflows$CWName,
                                       inline = TRUE),
                    radioButtons("flow_cum", "Daily or cumulative streamflows",
                                 choices = c("Daily", "Cumulative"),
                                 selected = "Cumulative"),
                    plotOutput("flow_graph"),
                    downloadButton("download_flow", label = "Download data"),
                    dataTableOutput("tbl_flow")
                    ),
            tabItem("res",
                    h2("Reservoir Levels and Volumes"),
                    checkboxGroupInput("reservoir_select", 
                                       label = "Select Reservoir(s) to Visualise",
                                       choiceNames = unique(reservoirs$CWName),
                                       choiceValue = unique(reservoirs$StationFID),
                                       selected = "406222A",
                                       inline = TRUE),
                    radioButtons("vol_lev", "Volumes or levels?",
                                 choices = c("Volumes", "Levels"),
                                 selected = "Levels"),
                    plotOutput("reservoir_level_graph"),
                    downloadButton("download_res", label = "Download data"),
                    dataTableOutput("tbl_res")
                    )
        )
    )
)

server <- function(input, output) {
                                        # MAP
    output$map <- renderLeaflet({
        pal <- colorFactor(c("darkgreen", "blue"), domain = c("StreamFlow", "Reservoir"))
        leaflet(stations) %>%
            addTiles() %>%
            addCircleMarkers(~Longitude, ~Lattitude, 
                             color = NA,
                             fillColor = ~pal(SiteType),
                             fillOpacity = 0.8,
                             popup = ~CWName)
    })
    
                                        # RAINFALL
    rain_select <- reactive({
        dates <- input$daterange
        df <- filter(rain, Reservoir %in% input$rain_select) %>%
            arrange(Date) %>%
            filter(Date >= dates[1] & Date <= dates[2])
        if(input$rain_cum == "Cumulative") {
            df <- group_by(df, Reservoir) %>% 
                mutate(Cumulative_Rainfall = cumsum(Rainfall)) %>%
                ungroup()
        } 
        return(df)
    })
    
    current_rain <- reactive({
        rain %>%
            group_by(Reservoir, Date) %>%
            summarise(Rainfall = sum(Rainfall)) %>%
            filter(Date == max(Date))
    })
    
    output$rainfall_graph <- renderPlot({
        df <- rain_select()
        if(input$rain_cum == "Cumulative"){
            ggplot(df, aes(Date, Cumulative_Rainfall, col = Reservoir)) + 
                geom_line(size = 1)
        } else {
            ggplot(df, aes(Date, Rainfall, fill = Reservoir)) + 
                geom_col(position = "dodge")
        }
        
    })
    
    output$tbl_rain <- renderDataTable(rownames= FALSE, {
        rain_select()
    })
    
    output$download_rain <- downloadHandler(
        filename = function() {
            paste('rainfall_data', Sys.Date(), '.csv', sep = "_")
        },
        content = function(con) {
            df <- rain_select()
            write.csv(df, con, row.names = FALSE)
        }
    )
    
                                        # STREAMFLOWS
    flow_select <- reactive({
        dates <- input$daterange
        df <- filter(streamflows, CWName %in% input$flow_select) %>%
            arrange(Date) %>%
            filter(Date >= dates[1] & Date <= dates[2])
        
        if(input$flow_cum == "Cumulative") {
            df <- group_by(df, CWName) %>% 
                mutate(Cumulative_Flow = cumsum(Flow)) %>%
                ungroup()
        } 
        return(df)
    })
    
    output$flow_graph <- renderPlot({
        df <- flow_select()
        if(input$flow_cum == "Cumulative"){
            ggplot(df, aes(Date, Cumulative_Flow, col = CWName)) + 
                geom_line(size = 1)
        } else {
            ggplot(df, aes(Date, Flow, fill = CWName)) + 
                geom_col(position = "dodge")
        }
        
    })
    
    output$tbl_flow <- renderDataTable(rownames= FALSE, {
        flow_select()
    })
    
    output$download_flow <- downloadHandler(
        filename = function() {
            paste('streamflow_data', Sys.Date(), '.csv', sep = "_")
        },
        content = function(con) {
            df <- rain_select()
            write.csv(df, con, row.names = FALSE)
        }
    )
    
                                        # RESERVOIRS
    reservoir_select <- reactive({
        dates <- input$daterange
        if (input$vol_lev == "Levels") {
            filter(reservoirs, StationFID %in% input$reservoir_select) %>%
                arrange(Date) %>%
                filter(Date >= dates[1] & Date <= dates[2])
        } else {
            filter(reservoirs, StationFID %in% input$reservoir_select) %>%
                group_by(Date) %>%
                summarise(Volume = sum(Volume, na.rm = TRUE)) %>%
                arrange(desc(Date))  %>%
                filter(Date >= dates[1] & Date <= dates[2])
        }
    })
    
    current_res <- reactive({
        reservoirs %>%
            group_by(CWName) %>%
            filter(Date == max(Date))
    })
    
    output$reservoir_level_graph <- renderPlot({
        df <- reservoir_select()
        if (input$vol_lev == "Levels") {
            ggplot(df, aes(Date, Level, col = CWName)) + 
                geom_line(size = 1) + 
                geom_line(aes(Date, FSL, col = CWName), linetype = "dashed")
        } else {
            ggplot(df, aes(Date, Volume)) + 
                geom_line(size = 1) 
        }
    })
    
    output$tbl_res <- renderDataTable(rownames= FALSE, {
        reservoir_select()
    })
    
    output$download_res <- downloadHandler(
        filename = function() {
            paste('reservoir_data', Sys.Date(), '.csv', sep = "_")
        },
        content = function(con) {
            df <- reservoir_select()
            write.csv(df, con, row.names = FALSE)
        }
    )
    
    output$lauriston_today <- renderValueBox({
        df <- current_res()
        df <- filter(df, CWName == "Lauriston")
        valueBox(trunc(df$Volume/df$FSLVolume * 100),
                 "Lauriston Reservoir", 
                 icon = icon("percent"), href = "#res")
    })
    
    output$ucoliban_today <- renderValueBox({
        df <- current_res()
        df <- filter(df, CWName == "Upper Coliban")
        valueBox(trunc(df$Volume/df$FSLVolume * 100),
                 "Upper Coliban Reservoir", 
                 icon = icon("percent"), href = "#res")
    })
    
    output$malmsbury_today <- renderValueBox({
        df <- current_res()
        df <- filter(df, CWName == "Malmsbury")
        valueBox(trunc(df$Volume/df$FSLVolume * 100),
                 "Malmsbury Reservoir", 
                 icon = icon("percent"), href = "#res")
    })
}
options(browser = "firefox")
shinyApp(ui, server)
