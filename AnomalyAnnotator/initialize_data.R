library(data.table)
library(lubridate)

# load data
series <- fread("data/train.csv")
meta <- fread("data/building_metadata.csv")
series <- merge(series, meta, "building_id", all.x=T, sort=F)

# add features
series$is_constant <- fread("data/is_constant.csv", select="is_constant_6")
series[, timestamp := ymd_hms(timestamp)]
series[, building_meter := paste(building_id, meter, sep="-")]

# save series
dir.create("data/building_meter_series", F)
for (bm in unique(series$building_meter)) {
    DT <- series[building_meter==bm, .(timestamp, meter_reading, is_constant)]
    saveRDS(DT, sprintf("data/building_meter_series/%s.rds", bm))
}

# create notes
anomaly_log <- series[, .(
    n_observations = .N,
    building_meter=building_meter[1],
    primary_use=primary_use[1],
    max_usage = max(meter_reading),
    site_id=site_id[1],
    square_feet=square_feet[1],
    floor_count=floor_count[1],
    year_built=year_built[1]),
    by=.(building_id, meter)]

setorder(anomaly_log, building_id, meter)

anomaly_log[, lower_limit := ifelse(meter==0, 0, -1)]
anomaly_log[, upper_limit := max_usage+1]
anomaly_log[, remove_constants := F]
anomaly_log[, outage_indices := lapply(.I, function(i) NULL)]
anomaly_log[, signal_indices := lapply(.I, function(i) NULL)]

# hardcode site 0
anomaly_log[(site_id == 0) & (meter == 0), remove_constants := T]

# save logs
log_file <- "data/anomaly_log.rds"
if (file.exists(log_file)) {
    response <- readline("File already exists.  Do you want to overwrite it? y/[n]")    
    if (tolower(response) == 'y') {
        saveRDS(anomaly_log, log_file)
    } else {
        print("Skipping write")
    }    
} else {
    saveRDS(anomaly_log, log_file)
}

