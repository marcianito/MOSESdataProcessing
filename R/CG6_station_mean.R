#' @title Calculate mean values for each survey station
#'
#' @description test
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CG6_station_mean = function(
        data_in,
        add_tides = T,
        cutoff_sd = NA,
        cutoff_tilt = NA,
        add_longTdrift = T
){
    # debugging
    # add_tides = T
    # data_in = data_in_raw
    # cutoff_sd = NA
    # data_in = cg6_data
    # cutoff_sd = 0.050
    # cutoff_sd = 1
    # cutoff_tilt = 12
    # cutoff_tilt = 5000 

    # readd tides, if desired
    if(is.na(cutoff_sd)){
        if(add_tides == T & add_longTdrift == F){
        cg6_addTide = data_in %>%
            reshape2::dcast(Station + datetime + survey + device_id + data_level + site + device + device_type ~ variable, value.var = "value") %>%
            dplyr::mutate(CorrGrav = CorrGrav - TideCorr) %>%
            # dplyr::select(-TideCorr) %>%
            reshape2::melt(id.vars = c("Station", "datetime", "survey", "device_id", "data_level", "site", "device", "device_type")) %>%
            dplyr::arrange(device_id, datetime, survey, Station) %>%
            dplyr::filter(variable != "TideCorr") %>%
            dplyr::filter(variable != "DriftCorr") %>%
            dplyr::filter(variable != "StdDev") %>%
            dplyr::filter(variable != "X") %>%
            dplyr::filter(variable != "Y") %>%
            dplyr::select(Station, datetime, survey, variable, value, device_id, data_level, site, device, device_type)
          data_in = cg6_addTide
        }
        if(add_tides == T & add_longTdrift == T){
        cg6_addTide = data_in %>%
            reshape2::dcast(Station + datetime + survey + device_id + data_level + site + device + device_type ~ variable, value.var = "value") %>%
            dplyr::mutate(CorrGrav = CorrGrav - TideCorr - DriftCorr) %>%
            # dplyr::select(-TideCorr) %>%
            reshape2::melt(id.vars = c("Station", "datetime", "survey", "device_id", "data_level", "site", "device", "device_type")) %>%
            dplyr::arrange(device_id, datetime, survey, Station) %>%
            dplyr::filter(variable != "TideCorr") %>%
            dplyr::filter(variable != "DriftCorr") %>%
            dplyr::filter(variable != "StdDev") %>%
            dplyr::filter(variable != "X") %>%
            dplyr::filter(variable != "Y") %>%
            dplyr::select(Station, datetime, survey, variable, value, device_id, data_level, site, device, device_type)
          data_in = cg6_addTide
        }
        if(add_tides == F & add_longTdrift == F){
          data_in = data_in %>%
            dplyr::arrange(device_id, datetime, survey, Station) %>%
            dplyr::filter(variable != "TideCorr") %>%
            dplyr::filter(variable != "DriftCorr") %>%
            dplyr::filter(variable != "StdDev") %>%
            dplyr::filter(variable != "X") %>%
            dplyr::filter(variable != "Y")
        }
    }else{
        if(add_tides == T & add_longTdrift == F){
        cg6_addTide = data_in %>%
            reshape2::dcast(Station + datetime + survey + device_id + data_level + site + device + device_type ~ variable, value.var = "value") %>%
            dplyr::mutate(CorrGrav = CorrGrav - TideCorr) %>%
            dplyr::filter(StdDev <= cutoff_sd) %>%
            dplyr::filter(abs(X) <= cutoff_tilt) %>%
            dplyr::filter(abs(Y) <= cutoff_tilt) %>%
            reshape2::melt(id.vars = c("Station", "datetime", "survey", "device_id", "data_level", "site", "device", "device_type")) %>%
            dplyr::arrange(device_id, datetime, survey, Station) %>%
            dplyr::filter(variable != "TideCorr") %>%
            dplyr::filter(variable != "DriftCorr") %>%
            dplyr::filter(variable != "StdDev") %>%
            dplyr::filter(variable != "X") %>%
            dplyr::filter(variable != "Y") %>%
            dplyr::select(Station, datetime, survey, variable, value, device_id, data_level, site, device, device_type)
          data_in = cg6_addTide
        }
        if(add_tides == T & add_longTdrift == T){
        cg6_addTide = data_in %>%
            reshape2::dcast(Station + datetime + survey + device_id + data_level + site + device + device_type ~ variable, value.var = "value") %>%
            dplyr::mutate(CorrGrav = CorrGrav - TideCorr - DriftCorr) %>%
            dplyr::filter(StdDev <= cutoff_sd) %>%
            dplyr::filter(abs(X) <= cutoff_tilt) %>%
            dplyr::filter(abs(Y) <= cutoff_tilt) %>%
            reshape2::melt(id.vars = c("Station", "datetime", "survey", "device_id", "data_level", "site", "device", "device_type")) %>%
            dplyr::arrange(device_id, datetime, survey, Station) %>%
            dplyr::filter(variable != "TideCorr") %>%
            dplyr::filter(variable != "DriftCorr") %>%
            dplyr::filter(variable != "StdDev") %>%
            dplyr::filter(variable != "X") %>%
            dplyr::filter(variable != "Y") %>%
            dplyr::select(Station, datetime, survey, variable, value, device_id, data_level, site, device, device_type)
          data_in = cg6_addTide
        }
        if(add_tides == F & add_longTdrift == F){
          data_in = data_in %>%
            reshape2::dcast(Station + datetime + survey + device_id + data_level + site + device + device_type ~ variable, value.var = "value") %>%
            dplyr::filter(StdDev <= cutoff_sd) %>%
            dplyr::filter(abs(X) <= cutoff_tilt) %>%
            dplyr::filter(abs(Y) <= cutoff_tilt) %>%
            reshape2::melt(id.vars = c("Station", "datetime", "survey", "device_id", "data_level", "site", "device", "device_type")) %>%
            dplyr::arrange(device_id, datetime, survey, Station) %>%
            dplyr::filter(variable != "TideCorr") %>%
            dplyr::filter(variable != "DriftCorr") %>%
            dplyr::filter(variable != "StdDev") %>%
            dplyr::filter(variable != "X") %>%
            dplyr::filter(variable != "Y") %>%
            dplyr::select(Station, datetime, survey, variable, value, device_id, data_level, site, device, device_type)
        }
    }

    # create standard timedif
    # reason: so beginning is also set and first value is not excluded !
    # [min]
    data_in$timedif = 1
    
    # create time differences between measurements
    for(i in 1:(dim(data_in)[1]-1)){
      data_in$timedif[i+1] = as.numeric(difftime(data_in$datetime[i+1], data_in$datetime[i], units = "mins"))
    }
    # get mean time dif (of all measurements)
    # timedif_mean = mean(data_in$timedif, na.rm = T)
    # [min]
    timedif_mean = 4
    # create measurement id
    # to seperate same stations "in time"
    data_in$measure_id = 0
    # create index for setting measure ids
    j = 1
    # setting measure ids
    for(i in 1:dim(data_in)[1]){
      if(data_in$timedif[i] > timedif_mean){
        j = j + 1
        data_in$measure_id[i] = j
      }else{
        data_in$measure_id[i] = j
      }
    }
    # calculating mean gravity and mean timestmap values
    # as.data.frame(data_in)
    ### FIRST THE TIDES HAVE TO BE SUBTRACTED !!!
   data_mean = data_in %>%
      dplyr::group_by(Station, device_id, measure_id) %>%
      dplyr::summarize(
                    value = mean(value, na.rm = T),
                    datetime = mean(datetime - min(datetime)) + min(datetime)
                    ) %>%
      dplyr::mutate(variable = "CorrGrav_mean") %>%
      dplyr::select(-measure_id) %>%
      as.data.frame()
    
    ## cut datetime to minutes
    # so both devices have the same time stamp
    data_mean$datetime = as.POSIXct(trunc(data_mean$datetime, "min"), tz = "UTC")
    # return data
    return(data_mean)
}

