#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(lubridate)
library(dygraphs)
library(DT)

library(plotly)


source("./global.R", local=T)

# Define UI for application that draws a histogram
ui <- fluidPage(
    br(),
    fluidRow(
        #  column 1 ------
        column(
            width = 3,
            # building meter selector
            fluidRow(
                column(
                    width = 6,
                    selectizeInput(
                        inputId = "buildingType",
                        label = "Building Type",
                        choices = isolate(unique(rv$anomaly_log$primary_use)),
                        selected = isolate(unique(rv$anomaly_log$primary_use)[1]),
                        width = "100%", 
                        options = list(maxOptions = 5000)
                    )
                ),
                column(
                    width = 6,
                    selectizeInput(
                        inputId = "buildingMeter",
                        label = "Building-Meter",
                        choices = isolate(rv$anomaly_log$building_meter),
                        selected = isolate(rv$anomaly_log$building_meter[rv$selected_row]),
                        width = "100%",
                        options = list(maxOptions = 5000)
                    )
                )
                
            ),
            # previous and next buttons
            fluidRow(
                column(
                    width = 6,
                    selectizeInput(
                        inputId = "meterType",
                        label = "Meter Type",
                        choices = isolate(unique(rv$anomaly_log$meter)),
                        selected = isolate(unique(rv$anomaly_log$meter)[1]),
                        width = "100%",
                        options = list(maxOptions = 5000)
                    )
                ),
                column(
                    width = 3,
                    actionButton(
                        inputId = "previousButton",
                        label = "Prev",
                        icon = icon("arrow-alt-circle-left"),
                        class = "btn-normal",
                        width = "100%"
                    )
                ),
                column(
                    width = 3,
                    actionButton(
                        inputId = "nextButton",
                        label = "Next",
                        icon = icon("arrow-alt-circle-right"),
                        class = "btn-normal",
                        width = "100%"
                    )
                )                
            )
        ),
        #  column 2 ------
        column(
            width = 3,
            column(
                width = 6,
                numericInput(
                    inputId = "startIndex",
                    label = "Start Index",
                    value = NA,
                    min = 0L, 
                    step = 1L
                )       
            ),
            column(
                width = 6,
                numericInput(
                    inputId =  "stopIndex",
                    label = "Stop Index",
                    value = NA,
                    min = 0L,
                    step = 1L
                )
            ),
            column(
                width = 6,
                radioButtons(
                    inputId = "isOutage",
                    label = "Is Outage",
                    choices = c(T, F),
                    selected = F,
                    inline = T
                )
            ),
            column(
                width = 6,
                actionButton(
                    inputId = "resetButton",
                    label = "Reset",
                    width = "100%"
                    #style = "color: #fff; background-color: #808080; border-color: #2e6da4"
                )
            )
            # column(
            #     width = 12,
            #     actionButton(
            #         inputId = "submitButton",
            #         label = "Submit",
            #         width = "100%",
            #         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            #     )
            # )
        ),
        #  column 3 ------
        column(
            width = 1,
            # anomaly slider 
            fluidRow(
                column(
                    width = 12,
                    numericInput(
                        inputId = "upperLimit",
                        label = "Upper Limit",
                        min = 0, 
                        max = isolate(rv$anomaly_log$max_usage[rv$selected_row]+1),
                        value = isolate(rv$anomaly_log$max_usage[rv$selected_row]+1),
                        step = 1,
                        width = "100%"
                    )
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    numericInput(
                        inputId = "lowerLimit",
                        label = "Lower Limit",
                        min = -1, 
                        max = isolate(rv$anomaly_log$max_usage[rv$selected_row]+1),
                        value = -1,
                        step = 1,
                        width = "100%"
                    )
                )
            )
        ),
        #  column 4 ------
        column(
            width = 2,
            column(
                width = 12,
                radioButtons(
                    inputId = "removeConstants",
                    label = "Remove Contants",
                    choices = c(T, F),
                    selected = F, inline = T
                )
            ),
            column(
                width = 12,
                radioButtons(
                    inputId = "xaxisType",
                    label = "x-axis",
                    choices = c("Timestamp", "Index"),
                    selected = "Index", inline = T
                )
            )
        ),             
        #  column 5 ------
        column(
            width = 2,
            align = "center",
            fluidRow(
                column(
                    width = 12,
                    tableOutput("statsTable")
                )
            )
        )
    ),
    br(),
    fluidRow(
        column(
            width = 12,
            dygraphOutput("timeSeriesPlot", height="400px")
        )
    ),
    hr(),
    fluidRow(
        column(
            width = 2,
            actionButton(
                inputId = "saveButton",
                label = "Save",
                width = "100%",
                icon = icon("save"),
                #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                class = "btn-primary btn-lg"
            )
        ),
        
        column(
            width = 2,
            actionButton(
                inputId = "normalButton",
                label = "Normal",
                width = "100%",
                #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                class = "btn-success btn-lg"
            )
        ),
        column(
            width = 2,
            actionButton(
                inputId = "anomalyButton",
                label = "Anomaly",
                width = "100%",
                icon = icon("exclamation-triangle"),
                #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                class = "btn-danger btn-lg"
            )
        )
    ),
    hr(),
    div(strong("From: "), textOutput("from", inline = TRUE), 
        strong("To: "), textOutput("to", inline = TRUE),
        strong("Index: "), textOutput("clicked", inline = TRUE)),
    br(),
    plotlyOutput("timeSeriesPlot_plotly"),
    verbatimTextOutput("hover"),
    verbatimTextOutput("click"),
    verbatimTextOutput("brushing"),
    verbatimTextOutput("selecting"),
    verbatimTextOutput("brushed"),
    verbatimTextOutput("selected")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$buildingType, {

        log = rv$anomaly_log
        log = subset(log, primary_use == input$buildingType)
        log = subset(log, meter       == input$meterType)

        updateSelectInput(
            session = session,
            inputId = "buildingMeter",
            choices = log$building_meter)
        
        rv$selected_row <- rv$anomaly_log[, which(building_meter == log$building_meter[1])]
        update_settings(rv$anomaly_log[rv$selected_row])
        
    })
    
    
    observeEvent(input$meterType, {
        
        log = rv$anomaly_log
        log = subset(log, primary_use == input$buildingType)
        log = subset(log, meter       == input$meterType)
        
        updateSelectInput(
            session = session,
            inputId = "buildingMeter",
            choices = log$building_meter)
        
        rv$selected_row <- rv$anomaly_log[, which(building_meter == log$building_meter[1])]
        update_settings(rv$anomaly_log[rv$selected_row])
        
    })
    
    
    # observers    
    source("./server-observers.R", local=T)
    
    # display table
    output$statsTable <- renderTable({
        select_cols <- c( "site_id", "square_feet", "floor_count", "year_built")
        stats <- t(rv$anomaly_log[building_meter == input$buildingMeter, ..select_cols])
        data.table(feature=rownames(stats), value=stats[,1])
    })
    
    meterData <- reactive({
        cat(paste("meterData", rv$selected_row, "\n"))
        load_series(params = rv$anomaly_log[rv$selected_row],
                    xaxis_type = input$xaxisType)
    })
    
    # time series plot
    output$timeSeriesPlot <- renderDygraph({
        input$submitButton
        input$normalButton
        rv$update_count
        #m = meterData()
        #assign("m", m, envir = .GlobalEnv)
        #plot_timeseries(m)
        plot_timeseries(load_series(
            params = rv$anomaly_log[rv$selected_row],
            xaxis_type = input$xaxisType),
            xaxis_type = input$xaxisType)
    })
    
    output$timeSeriesPlot_plotly <- renderPlotly({
        input$submitButton
        rv$update_count
        series = load_series(params = rv$anomaly_log[rv$selected_row],
                             xaxis_type = input$xaxisType)
        assign("series", series, envir = .GlobalEnv)
        s1 = series[, .(timestamp, meter_reading, clean_meter_reading)]
        
        plot_ly(s1, x = ~timestamp, y = ~meter_reading)
        
        fig <- plot_ly(s1, x = ~timestamp)
        fig <- fig %>% add_lines(y = ~meter_reading, name = "meter_reading")
        fig <- fig %>% add_lines(y = ~clean_meter_reading, name = "clean_meter_reading")
        #fig <- fig %>% layout(
        #    xaxis = list(rangeslider = list(type = "date"))
        #)
        fig
    })
    
    
    ## plotly events
    output$hover <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d)) "Hover events appear here (unhover to clear)" else d
    })
    
    output$click <- renderPrint({
        d <- event_data("plotly_click")
        if (is.null(d)) "plotly_click Click events appear here (double-click to clear)" else d
    })
    
    output$brushing <- renderPrint({
        d <- event_data("plotly_brushing")
        if (is.null(d)) "plotly_brushing Brush extents appear here (double-click to clear)" else d
    })
    
    output$selecting <- renderPrint({
        d <- event_data("plotly_selecting")
        if (is.null(d)) "plotly_selecting Brush points appear here (double-click to clear)" else d
    })
    
    output$brushed <- renderPrint({
        d <- event_data("plotly_brushed")
        if (is.null(d)) "plotly_brushed Brush extents appear here (double-click to clear)" else d
    })
    
    output$selected <- renderPrint({
        d <- event_data("plotly_selected")
        if (is.null(d)) "plotly_selected Brushed points appear here (double-click to clear)" else d
    })
    
    
    #### dygraph    
    output$from <- renderText({
        assign("click", input$timeSeriesPlot_click, envir = .GlobalEnv)
        assign("date_window", input$timeSeriesPlot_date_window, envir = .GlobalEnv)
        
        req(input$timeSeriesPlot_date_window)
        
        if(input$xaxisType == "Index") {
            from = ceiling(as.numeric(input$timeSeriesPlot_date_window[1]))
            cat(paste(from), "\n")
        } else {
            from = strptime(input$timeSeriesPlot_date_window[1], 
                            format = "%Y-%m-%dT%H:%M:%S", tz="UTC")
            cat(paste(from), "\n")
        }
        as.character(from)
    })
    output$to <- renderText({
        req(input$timeSeriesPlot_date_window)
        if(input$xaxisType == "Index") {
            from = floor(as.numeric(input$timeSeriesPlot_date_window[2]))
            cat(paste(from), "\n")
        } else {
            from = strptime(input$timeSeriesPlot_date_window[2], 
                            format = "%Y-%m-%dT%H:%M:%S", tz="UTC")
            cat(paste(from), "\n")
        }
        as.character(from)
    })
    
    output$clicked <- renderText({
        #strftime(req(input$timeSeriesPlot_click$x), "%d %b %Y")
        req(input$timeSeriesPlot_click$x)
    })

    observeEvent(input$timeSeriesPlot_click, {
        
        m = paste0('X = ', req(input$timeSeriesPlot_click$x_closest_point), 
                   '; Y = ', req(input$timeSeriesPlot_click$y_closest_point),
                   '\n')
        cat(m)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
