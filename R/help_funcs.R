#' @title Convert zoo-object to data frame
#'
#' @description Converting zoo-object to data frame with proper indexing of time column and column names
#'
#' @param value must be a zoo-object
#' @references Marvin Reich (2017), mreich@@posteo.de
# ' @examples examples MISSING
#' 
zootodf <- function(value) {
    df <- as.data.frame(value)
    df$time <- index(value) #create a Date column
    rownames(df) <- NULL #so row names not filled with dates
    df <- df[,c(ncol(df), 1:(ncol(df)-1))] #reorder columns so Date first
    return(df)
}

#' @title Calculate number of decimal places in a number
#'
#' @description 
#'
#' @param x Numeric, the number to check decimal places.
#' @details missing
#' @references Marvin Reich (2017), mreich@@posteo.de
#' @examples missing

decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
        return(0)
    }
}

#' @title Normalize data 
#'
#' @description Normalize data in subtracting the mean and dividing by the standard deviation.
#'
#' @param x input dataset
#' @references Marvin Reich (2015), mreich@@gfz-potsdam.de
#' @examples example MISSING
#' @export
#' 

normalize = function(x){
	data_norm = (x - mean(x, na.rm=T))/sd(x,na.rm=T)
	return(data_norm)
}

#' @title Normalize data only sbutracting mean
#'
#' @description Normalize data in subtracting the mean and dividing by the standard deviation.
#'
#' @param x input dataset
#' @references Marvin Reich (2015), mreich@@gfz-potsdam.de
#' @examples example MISSING
#' @export
#' 

normalize_mean = function(x){
	data_norm = (x - mean(x, na.rm=T))
	return(data_norm)
}

#' @title RMSE of simulated and observed vectors
#'
#' @description test
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @export
#' @examples missing

rmse = function(
    ts_sim,
    ts_obs
){
  ts_df = data.frame(ts_sim,ts_obs)
  ts_df$sqr_dif = (ts_df$ts_sim - ts_df$ts_obs)^2
  rmse_val = sqrt(sum(ts_df$sqr_dif) / length(ts_sim))
  return(rmse_val)
}

#' @title Aggregate data.frame time series to other time periods
#'
#' @description test
#'
#' @param timeseries_data data.frame, formatted with columns [datetime, values].
#' @param newPeriod charater string, options are "hourly", "daily", "weekly".
#' @param fun character string, indicating aggregation function.
#' @param time_offset integer, indicating the offset to include in the aggregation.
#' Units are with respect to declared new period.
#' 
#' @return test
#' 
#' @details Convention for setting dates corresponding to new period: POSIXct is
#' always used from the LAST hour, day, etc. of the aggregation period.
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @export
#' @import dplyr
#' @examples missing

aggTS = function(
    timeseries_data,
    newPeriod,
    fun = "sum",
    time_offset = 0,
    conserve_columns = NA
){
    ## DEBUGGING
    # timeseries_data = as.data.frame(ET_hourly)
    # timeseries_data = as.data.frame(data_radar_mod)
    # timeseries_data = data_new
    # newPeriod = "hourly"
    # fun = "sum"
    # timeseries_data = data_new
    # newPeriod = "min"
    # fun = "mean"
    # time_offset = 0
    # conserve_columns = "cell_index"
    # conserve_columns = c("x", "y")
    ####################
    ##
    ## force input to be a data.frame
    timeseries_data = as.data.frame(timeseries_data)
    # construct list of columns to conserve during summarizing
    if(!is.na(conserve_columns)){
    columns = as.list(c("datetime", conserve_columns))
    }else{
    columns = as.list(c("datetime"))
    }
    # format after new period
    # and include offset
    switch(newPeriod,
           daily = {
        ts_newPeriod = timeseries_data %>%
        dplyr::mutate(datetime = as.POSIXct(trunc(datetime, "days"))) %>%
        dplyr::mutate(datetime = datetime + 3600 * 24 * time_offset)
           },
           min = {
        ts_newPeriod = timeseries_data %>%
        dplyr::mutate(datetime = as.POSIXct(trunc(datetime, "min"))) %>%
        dplyr::mutate(datetime = datetime + 60 * time_offset)
           },
           hourly = {
        ts_newPeriod = timeseries_data %>%
        dplyr::mutate(datetime = as.POSIXct(trunc(datetime, "hours"))) %>%
        dplyr::mutate(datetime = datetime + 3600 * time_offset)
           },
           weekly = {
               ## still not sure how to do this !! ?
               ## now, grouping should be over "datetime_weeks"
        # week needed for correct time setting:
        # convention: end of week
        week = 3600 * 24 * 7
        ts_newPeriod = timeseries_data %>%
        dplyr::mutate(datetime_weeks = paste0(format(datetime, "%Y-%W"), "-0")) %>%
        dplyr::mutate(datetime = as.POSIXct(strptime(datetime_weeks, "%Y-%W-%w")) + week) %>%
        dplyr::mutate(datetime = datetime + 3600 * 24 * 7 * time_offset) %>%
        dplyr::select(-datetime_weeks)
           }
           )
    #
    # format after aggregation function
    switch(fun,
           sum = {
        ts_newPeriod = ts_newPeriod %>%
        # dplyr::group_by(datetime) %>%
        dplyr::group_by_(.dots = columns) %>%
        # dplyr::summarize(observed = sum(value, na.rm = T))
        dplyr::summarize(value = sum(value, na.rm = T))
           },
           mean = {
        ts_newPeriod = ts_newPeriod %>%
        # dplyr::group_by(datetime) %>%
        dplyr::group_by_(.dots = columns) %>%
        # dplyr::summarize(observed = mean(value, na.rm = T))
        dplyr::summarize(value = mean(value, na.rm = T))
           }
           )
    #
    # convert time pack to POSIXct
    ts_newPeriod$datetime = as.POSIXct(ts_newPeriod$datetime)
    ## bug fix
    # exclude LAST value: no idea why it is always 'NA'
    ts_newPeriod = ts_newPeriod[-length(ts_newPeriod$datetime),]
    # return new time series data
    return(ts_newPeriod)
}

#' @title Interpolate missing time steps of data time series
#'
#' @description test
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @export
#' @examples missing

interpTS = function(
    data_in,
    freq_in,
    freq_out = NA,
    aggFunc = "mean",
    data_col_name
){
    ## debugging
    # data_in = influx_latest
    # freq_in = "hourly"
    # freq_in = "min"
    # freq_out = "min"
    # aggFunc = "mean"
    # data_col_name = "value"
    ####################
    # get start and end time stamps
    ts_start = min(data_in$datetime)
    ts_end = max(data_in$datetime)
    # catch bug and maintain old code in catching this:
    # if(freq_in == "hourly") freq_in = "hour"
    # construct time series dates: hourly
    datetime_new = data.frame(datetime = seq(ts_start, ts_end, by=freq_in))
    # select data column
    gw_selected = data_in %>%
        dplyr::select_("datetime", data_col_name)
    colnames(gw_selected)[2] = "value"
    # join old into new data time stamps
    data_new = datetime_new %>%
        dplyr::left_join(gw_selected)
    # approximate (interpolate, linear) missing data 
    data_new$value = na.approx(data_new$value)
    if(!is.na(freq_out)){
    data_new = aggTS(data_new, freq_out, aggFunc)
    }
    # return data
    return(data_new)
}

