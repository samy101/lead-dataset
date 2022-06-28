library(shiny)
library(data.table)
library(lubridate)
library(dygraphs)
library(DT)

library(plotly)

# cleaning function
clean_data <- function(series, params) {
    
    series[, clean_meter_reading := meter_reading]
    
    if (params$remove_constants)
        series[(is_constant), clean_meter_reading := NA]
    
    if (!is.na(params$lower_limit))
        series[clean_meter_reading <= params$lower_limit, clean_meter_reading := NA]
    
    if (!is.na(params$upper_limit))
        series[clean_meter_reading >= params$upper_limit, clean_meter_reading := NA]
    
    if (length(params$outage_indices[[1]]) > 0)
        series[params$outage_indices[[1]], clean_meter_reading := NA]    
    
    if (length(params$signal_indices[[1]]) > 0)
        series[params$signal_indices[[1]], clean_meter_reading := meter_reading]
    
    return(series)
}


anomaly_log_file <- "data/anomaly_log_120422.rds"

# initialize values
rv <- reactiveValues(
    selected_row = 1,
    update_count = 0,
    #anomaly_log = readRDS("data/anomaly_log.rds")
    anomaly_log = readRDS(anomaly_log_file)
)

# data loading
load_series <- function(params, xaxis_type) {
    series <- readRDS(sprintf("data/building_meter_series/%s.rds", params$building_meter))
    if (xaxis_type == "Index") series[, timestamp := .I] 
    clean_data(series=series, params=params)
}

# plot dygraph
plot_timeseries <- function(series, xaxis_type) {
    
    # base plot -----
    dg <- dygraph(series[, .(timestamp, meter_reading, clean_meter_reading)]) %>%
        dyRangeSelector() %>% dyUnzoom() %>% dyRibbon()
    #%>% dyCallbacks(zoomCallback = zoomCB)
    
    if(xaxis_type == "Index") {
        adj = 0.5
    } else {
        adj = 30*60 # 30-minutes
    }
    
    # add outage shading -----
    shade <- series[, rle(is.na(clean_meter_reading))]    
    if (length(shade$lengths)==1) {
        if (shade$values[1])
            dg <- dg %>% dyShading(
                from=series[1, timestamp],
                to=series[.N, timestamp],
                color="#FFB6D5"
            )              
    } else if (length(shade$lengths) > 1) {
        i <- 1
        for (k in 1:length(shade$lengths)) {
            if (shade$values[k])
                dg <- dg %>% dyShading(
                    from=series[i, timestamp] - adj,
                    to=series[min(i + shade$lengths[k], .N), timestamp] - adj,
                    color="#FFB6D5"
                )  
            i <- i + shade$lengths[k]
        }
    }
    
    return(dg)
}




