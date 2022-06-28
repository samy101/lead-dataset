# update ui components

update_settings <- function(params) {
    updateSelectInput(
        session = session,
        inputId = "buildingMeter",
        selected = rv$anomaly_log[rv$selected_row, building_meter]
    )
    updateSelectInput(
        session = session,
        inputId = "removeConstants",
        selected = params$remove_constants
    )
    updateNumericInput(
        session = session,
        inputId = "lowerLimit",
        value = params$lower_limit,
        max = params$max_usage+1
    )
    updateNumericInput(
        session = session,
        inputId = "upperLimit",
        value = params$upper_limit,
        max = params$max_usage+1
    )
    updateNumericInput(
        session = session,
        inputId = "startIndex",
        value = NA,
    )
    updateNumericInput(
        session = session,
        inputId = "stopIndex",
        value = NA,
    )    
    updateRadioButtons(
        session = session,
        inputId = "isOutage",
        selected = F,
    )
}

# -------- column 1 --------
# building meter selector
observeEvent(input$buildingMeter, {
    rv$selected_row <- rv$anomaly_log[, which(building_meter == input$buildingMeter)]
    update_settings(rv$anomaly_log[rv$selected_row])
}, priority = 10)

# previous button
observeEvent(input$previousButton, {
    #rv$selected_row <- if (rv$selected_row==1) nrow(rv$anomaly_log) else rv$selected_row-1 
    
    log <- rv$anomaly_log
    log <- subset(log, primary_use == isolate(input$buildingType))
    log <- subset(log, meter       == isolate(input$meterType))
    
    cur <- which(log$building_meter == input$buildingMeter)
    prv <- if (cur == 1) nrow(log) else cur-1
    prv_bm <- log$building_meter[prv]
    
    rv$selected_row <- rv$anomaly_log[, which(building_meter == prv_bm)]
    update_settings(rv$anomaly_log[rv$selected_row])
})

# next button
observeEvent(input$nextButton, {
    
    log <- rv$anomaly_log
    log <- subset(log, primary_use == isolate(input$buildingType))
    log <- subset(log, meter       == isolate(input$meterType))
    
    cur <- which(log$building_meter == input$buildingMeter)
    nxt <- if (cur == nrow(log)) 1 else cur+1
    nxt_bm <- log$building_meter[nxt]
    #cat(paste(cur, nxt, nxt_bm, "\n"))
    rv$selected_row <- rv$anomaly_log[, which(building_meter == nxt_bm)]
    update_settings(rv$anomaly_log[rv$selected_row])
})

# save button
observeEvent(input$saveButton, {
    #save_name <- sprintf("%s/iii/building_meter_series/anomaly_log.rds", DATA_PATH)
    #saveRDS(rv$anomaly_log, save_name)
    
    #save_name <- sprintf("./data/anomaly_log.rds", DATA_PATH)
    #save_name <- "./data/anomaly_log.rds"
    saveRDS(rv$anomaly_log, anomaly_log_file)    
    
    cat("saved\n")
})

# -------- column 2 --------
# reset button
observeEvent(input$resetButton, {
    rv$anomaly_log[rv$selected_row, outage_indices := .(.(NULL))]
    rv$anomaly_log[rv$selected_row, signal_indices := .(.(NULL))]
    rv$update_count <- rv$update_count+1
    cat("reset\n")
})

# submit button
observeEvent(input$submitButton, {
    if (is.na(input$startIndex)) 
        return()
    if (is.na(input$stopIndex)) 
        return()
    if (input$stopIndex < input$startIndex)
        return()
    
    start <- max(input$startIndex, 1)
    stop <- min(input$stopIndex, rv$anomaly_log[rv$selected_row, n_observations])
    indices <- start:stop
    if (input$isOutage) {
        rv$anomaly_log[rv$selected_row, outage_indices := .(.(union(outage_indices[[1]], indices)))]
        rv$anomaly_log[rv$selected_row, signal_indices := .(.(setdiff(signal_indices[[1]], indices)))]
    } else {
        rv$anomaly_log[rv$selected_row, outage_indices := .(.(setdiff(outage_indices[[1]], indices)))]
        rv$anomaly_log[rv$selected_row, signal_indices := .(.(union(signal_indices[[1]], indices)))]
    }
    rv$update_count <- rv$update_count+1
    
}, priority=1)



# anomaly button
observeEvent(input$anomalyButton, {
    
    if (input$xaxisType == "Timestamp") {
        cat(paste("xaxisType", input$xaxisType))
        return()
    }
    
    series = meterData()
    #assign("m1", m, envir = .GlobalEnv)
    #assign("win", input$timeSeriesPlot_date_window, envir = .GlobalEnv)
    # 
    # dt = as.POSIXct(strptime(input$timeSeriesPlot_date_window, 
    #                          "%Y-%m-%dT%H:%M:%S"), tz="UTC")
    # 
    # s1 = series
    # s1 = s1[s1$timestamp >= dt[1], ]
    # s1 = s1[s1$timestamp <= dt[2], ]
    # 
    # st = s1$timestamp[1]
    # en = s1$timestamp[nrow(s1)]
    # 
    # assign("s1", s1, envir = .GlobalEnv)
    # 
    # start = series[, which(timestamp == st)]
    # stop  = series[, which(timestamp == en)]
    # cat(paste("anomalyButton", dt, start, stop, "\n"))
    
    start = round(input$timeSeriesPlot_date_window[1])
    stop = round(input$timeSeriesPlot_date_window[2])
    cat(paste("anomalyButton", start, stop, rv$selected_row, "\n"))
    indices <- start:stop
    
    cat(paste("anomalyButton 0", indices, "\n"))
    
    cat(paste("anomalyButton 1", rv$anomaly_log[rv$selected_row, ]$outage_indices[[1]], "\n"))
    
    
    rv$anomaly_log[rv$selected_row, outage_indices := .(.(union(outage_indices[[1]], indices)))]
    rv$anomaly_log[rv$selected_row, signal_indices := .(.(setdiff(signal_indices[[1]], indices)))]
    
    cat(paste("anomalyButton 2", rv$anomaly_log[rv$selected_row, ]$outage_indices[[1]], "\n"))
    
    rv$update_count <- rv$update_count+1
    
}, priority=1)


# normal button
observeEvent(input$normalButton, {
    
    if (input$xaxisType == "Timestamp") {
        cat(paste("xaxisType", input$xaxisType))
        return()
    }
    
    #series = meterData()
    #assign("m1", m, envir = .GlobalEnv)
    #assign("win", input$timeSeriesPlot_date_window, envir = .GlobalEnv)
    # 
    # dt = as.POSIXct(strptime(input$timeSeriesPlot_date_window, 
    #                          "%Y-%m-%dT%H:%M:%S"), tz="UTC")
    # 
    # s1 = series
    # s1 = s1[s1$timestamp >= dt[1], ]
    # s1 = s1[s1$timestamp <= dt[2], ]
    # 
    # st = s1$timestamp[1]
    # en = s1$timestamp[nrow(s1)]
    # 
    # assign("s1", s1, envir = .GlobalEnv)
    # 
    # start = series[, which(timestamp == st)]
    # stop  = series[, which(timestamp == en)]
    # cat(paste("anomalyButton", dt, start, stop, "\n"))
    
    start = round(input$timeSeriesPlot_date_window[1])
    stop = round(input$timeSeriesPlot_date_window[2])
    cat(paste("anomalyButton", start, stop, rv$selected_row, "\n"))
    indices <- start:stop
    
    cat(paste("anomalyButton 0", indices, "\n"))
    
    cat(paste("anomalyButton 1", rv$anomaly_log[rv$selected_row, ]$outage_indices[[1]], "\n"))
    
    rv$anomaly_log[rv$selected_row, outage_indices := .(.(setdiff(outage_indices[[1]], indices)))]
    rv$anomaly_log[rv$selected_row, signal_indices := .(.(union(signal_indices[[1]], indices)))]
    
    cat(paste("anomalyButton 2", rv$anomaly_log[rv$selected_row, ]$outage_indices[[1]], "\n"))
    
    rv$update_count <- rv$update_count+1
    
}, priority=1)





# -------- column 3 --------
# upperLimit input
observeEvent(input$upperLimit, {
    rv$anomaly_log[rv$selected_row, upper_limit := input$upperLimit]
    rv$update_count <- rv$update_count+1
})

# lowerLimit input
observeEvent(input$lowerLimit, {
    rv$anomaly_log[rv$selected_row, lower_limit := input$lowerLimit]
    rv$update_count <- rv$update_count+1
})

# -------- column 4 --------
# remove constants buttonr
observeEvent(input$removeConstants, {
    rv$anomaly_log[rv$selected_row, remove_constants := as.logical(input$removeConstants)]
    rv$update_count <- rv$update_count+1
})

