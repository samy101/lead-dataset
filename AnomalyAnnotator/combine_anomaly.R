library(data.table)
library(lubridate)
library(stringr)

## Run this script after annotating the anomalies
## This script will combine all electricity meter data readings
## and their annotated lables into a single file.

# cleaning function
clean_data <- function(series, params) {
    
    series[, clean_meter_reading := meter_reading]
    
    if (params$remove_constants)
        series[(is_constant), clean_meter_reading := NA]
    
    if (!is.na(params$lower_limit))
        series[clean_meter_reading <= params$lower_limit, clean_meter_reading := NA]
    
    if (!is.na(params$upper_limit))
        series[clean_meter_reading >= params$upper_limit, clean_meter_reading := NA]
    
    #if (length(params$outage_indices[[1]]) > 0)
    #    series[params$outage_indices[[1]], clean_meter_reading := NA]    
    
    if (length(params$signal_indices[[1]]) > 0)
        series[params$signal_indices[[1]], clean_meter_reading := meter_reading]
    
    return(series)
}

anomaly_log = readRDS("data/anomaly_log_120422.rds")

data_path = "data/building_meter_series/"
files = list.files(path=data_path, pattern = "*.rds")

df_elec = NULL
for(filename in files) {
    
    ff = str_replace(filename, ".rds", "" )
    b_id = as.integer(str_split(ff, "-")[[1]][1])
    m_id = as.integer(str_split(ff, "-")[[1]][2])
    
    if(m_id != 0)  ## process only the electricity meter data
        next
    
    log = anomaly_log[ (building_id == b_id) & (meter == m_id), ]
    
    dat = readRDS(paste0(data_path, filename))
    dat = clean_data(dat, log)
    
    dat[, building_id := b_id]
    #dat[, meter := m_id]
    dat[, anomaly := F]
    dat[log$outage_indices[[1]], anomaly := T]
    df_elec = rbind(df_elec, dat)
    
    print(paste(filename, b_id, m_id, nrow(dat), nrow(df_elec)))
    #break
}

df_elec1 = df_elec

df_elec$anomaly = as.integer(df_elec$anomaly)
df_elec$is_constant = as.integer(df_elec$is_constant)

write.csv(df_elec, "data/electricity_annotated_120422.csv", row.names = F, na="")

anom = df_elec[, 'anomaly']
anom$anomaly = as.integer(anom$anomaly)
write.csv(anom, "data/anomaly_labels_new.csv", row.names = F)

m1 = df_elec[df_elec$building_id == 678, ]




### split by building type
meta <- fread("data/building_metadata.csv")
for(type in unique(meta$primary_use)) {
    m1 = meta[meta$primary_use == type, ]
    e1 = df_elec[df_elec$building_id %in% m1$building_id, ]
    
    type = str_replace(type, "/", "_")
    type = str_replace_all(type, " ", "_")
    
    fname = paste0("data/annotated/electricity_annotated_", type, ".csv")
    
    ss = table(e1$anomaly) / nrow(e1) * 100
    ss = round(ss, 2)
    #print(paste(type, nrow(e1), ss[1], ss[2]))
    print(sprintf("%40s : %8d - %6.2f  %6.2f", type, nrow(e1), ss[1], ss[2]))
    write.csv(e1, fname, row.names = F)
}

### split by site id
meta <- fread("data/building_metadata.csv")
for(site in unique(meta$site_id)) {
    m1 = meta[meta$site_id == site, ]
    e1 = df_elec[df_elec$building_id %in% m1$building_id, ]
    
    fname = paste0("data/annotated/electricity_annotated_site_", site, ".csv")
    
    ss = table(e1$anomaly) / nrow(e1) * 100
    ss = round(ss, 2)
    #print(paste(type, nrow(e1), ss[1], ss[2]))
    print(sprintf("%2s : %8d - %6.2f  %6.2f", site, nrow(e1), ss[1], ss[2]))
    write.csv(e1, fname, row.names = F)
}


nrow(df_elec)
series <- fread("data/train.csv")
nrow(series[meter ==0, ])