#' @title Smooth all time series necessary for CRNS data processing
#'
#' @description test
#'
#' @param ts_* data.frame, each with columns [datetime, value]; datetime has to be in POSIXct format.
#' @param outlierCorretion default = True. If an extreme values cutoff is desired.
#' @param smoothingMethod options: mean or median.
#' @param windowWidth number of hours for smoothing window width. Default = 12 hours.
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CRNS_smoothTS = function(
        ts_ncounts,
        ts_pressure,
        ts_relhum,
        ts_temp,
        outlierCorretion = T,
        smoothingMethod = "mean",
        windowWidth = 12
){
    ##########
    ## debugging
    ts_ncounts = dplyr::select(crns_counts, datetime, value)
    ts_pressure = dplyr::select(crns_pressure, datetime, value)
    ts_relhum = dplyr::select(climate_relHum, datetime, value)
    ts_temp = dplyr::select(climate_temperature, datetime, value)
    outlierCorretion = T
    smoothingMethod = "mean"
    windowWidth = 12
    ##########
    ## create hourly values
    ncounts_hourly = aggTS(ts_ncounts, newPeriod = "hourly", fun = "sum") 
    pressure_hourly = aggTS(ts_pressure, newPeriod = "hourly", fun = "mean") 
    relhum_hourly = aggTS(ts_relhum, newPeriod = "hourly", fun = "mean") 
    temp_hourly = aggTS(ts_temp, newPeriod = "hourly", fun = "mean") 
    # set columns names from "value" to <variable name>
    colnames(ncounts_hourly)[2] = "ncounts"
    colnames(pressure_hourly)[2] = "pressure"
    colnames(relhum_hourly)[2] = "relhum"
    colnames(temp_hourly)[2] = "temp"
    ## create maximum time series time span
    ts_start_min = min(min(ncounts_hourly$datetime), min(pressure_hourly$datetime), min(relhum_hourly$datetime), min(temp_hourly$datetime))
    ts_start_max = max(max(ncounts_hourly$datetime), max(pressure_hourly$datetime), max(relhum_hourly$datetime), max(temp_hourly$datetime))
    ts_complete = data.frame(datetime = seq(ts_start_min, ts_start_max, by ="hour"))
    # merge all timeseries into one data.frame
    ts_complete = ts_complete %>%
        dplyr::left_join(ncounts_hourly) %>%
        dplyr::left_join(pressure_hourly) %>%
        dplyr::left_join(relhum_hourly) %>%
        dplyr::left_join(temp_hourly)
    ## remove extreme outliers
    if(outlierCorretion == T){
        outlier_lowerBoundary = mean(ncounts_hourly$ncounts, na.rm = T) - 4 * sd(ncounts_hourly$ncounts, na.rm = T)
        outlier_upperBoundary = mean(ncounts_hourly$ncounts, na.rm = T) + 4 * sd(ncounts_hourly$ncounts, na.rm = T)
        ts_complete = ts_complete %>%
            dplyr::filter(ncounts >= outlier_lowerBoundary & ncounts <= outlier_upperBoundary)
    }
    ## smooting counts with 12 hour window
    # choose method
    switch(smoothingMethod,
           mean= {
               # ncounts_12hourSmoothed = rollmean(ts_complete$ncounts, k = windowWidth + 1, fill = NA)
               ncounts_12hourSmoothed = stats::filter(x = ts_complete$ncounts, filter = rep(1, windowWidth)) / windowWidth
           },
           median= {
               # ncounts_12hourSmoothed = rollmedian(ts_complete$ncounts, k = windowWidth + 1, fill = NA)
           }
    )
    # replace column in ts_complete with smoothed count data
    ts_complete$ncounts = ncounts_12hourSmoothed
    ## return smoothed dataset
    return(ts_complete)
}

