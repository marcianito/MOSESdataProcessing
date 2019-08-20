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
        data_in
){
    # create standard timedif
    # reason: so beginning is also set and first value is not excluded !
    # [min]
    data_in$timedif = 1
    
    # create time differences between measurements
    for(i in 1:(dim(data_in)[1]-1)){
      data_in$timedif[i+1] = as.numeric(data_in$datetime[i+1] - data_in$datetime[i])
    }
    # get mean time dif (of all measurements)
    timedif_mean = mean(data_in$timedif, na.rm = T)
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

