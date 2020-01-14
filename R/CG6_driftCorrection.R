#' @title Drift correction for CG6 Gravity survey data
#'
#' @description test
#'
#' @param test test
#' @param gravRef Station of referene gravity values. Options are: "TERENO", "BOECHNCHEN", "AQG". Values of reference gravity have to be in nm/s².
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CG6_driftCorrection = function(
      data_in_raw,
      base_station,
      gravRef = NA,
      cut_sd = NA,
      cut_tilt = NA,
      org_units = "mGal",
      readd_longTdrift = FALSE
){
    ## debugging
    # data_in_raw = as.data.frame(cg6_data)
    # data_in_raw = cg6_data
    # base_station = "BOERNCHEN"
    # base_station = "PILLARGPHONE"
    # cut_sd = 0.05
    # cut_tilt = 12
    # use_gPhone = F
    # dev_id = "58"
    # # i=2
    # data_in_raw = data_survey_cleaned
    # base_station = "PILLARFG5"
    # cut_sd = NA
    # cut_tilt = NA
    # gravRef = "TERENO"
    # dev_id = "58"
    # org_units = "mGal"
    # readd_longTdrift = T
    ##########
    ## gravRef
    # files or connections if a reference gravimmeter should be used, additionally
    if(gravRef == "TERENO"){
      file_gravityResidual = "/home/mreich/server/hygra/DataTERENO/Gravity/ProcData/GFZ/localResData/gravity_residual_until2019-09-30.rData"
    }
    if(gravRef == "BOERNCHEN"){
      file_gravityResidual = "/home/mreich/Dokumente/MOSES/Data/gPhone/gPhone_Boernchen_processed/gravity_residual_until2019-10-01.rData"
    }
    if(gravRef == "AQG"){
      file_gravityResidual = ""
    }
    ##########
    ##########
    ## calculate station mean values
    # first the tide signal is added back to g signal
if(is.na(cut_sd)){
  data_means = CG6_station_mean(
                        data_in = data_in_raw,
                        add_tides = T,
                        add_longTdrift = readd_longTdrift
    )
    # for further processing, the tide information is obsolete
    data_in_raw = data_in_raw %>%
        reshape2::dcast(Station + datetime + survey + device_id + data_level + site + device + device_type ~ variable, value.var = "value") %>%
        # re-add long term drift
        dplyr::mutate(CorrGrav = CorrGrav - DriftCorr) %>%
        reshape2::melt(id.vars = c("Station", "datetime", "survey", "device_id", "data_level", "site", "device", "device_type")) %>%
        dplyr::arrange(datetime) %>%
        dplyr::filter(variable != "TideCorr") %>%
        dplyr::filter(variable != "DriftCorr") %>%
        dplyr::filter(variable != "StdDev") %>%
        dplyr::filter(variable != "X") %>%
        dplyr::filter(variable != "Y") %>%
        dplyr::select(Station, datetime, survey, variable, value, device_id, data_level, site, device, device_type)
    # data_in_raw = data_in_raw %>%
    #     dplyr::filter(variable != "TideCorr") %>%
    #     dplyr::filter(variable != "DriftCorr") %>%
    #     dplyr::filter(variable != "StdDev") %>%
    #     dplyr::filter(variable != "X") %>%
    #     dplyr::filter(variable != "Y")
    }else{
    data_means = CG6_station_mean(
                          data_in = data_in_raw,
                          add_tides = T,
                          add_longTdrift = readd_longTdrift,
                          cutoff_sd = cut_sd,
                          cutoff_tilt = cut_tilt
    )
    # for further processing, the tide information is obsolete
    data_in_raw = data_in_raw %>%
        reshape2::dcast(Station + datetime + survey + device_id + data_level + site + device + device_type ~ variable, value.var = "value") %>%
        dplyr::filter(StdDev <= cut_sd) %>%
        dplyr::filter(abs(X) <= cut_tilt) %>%
        dplyr::filter(abs(Y) <= cut_tilt) %>%
        # re-add long term drift
        dplyr::mutate(CorrGrav = CorrGrav - DriftCorr) %>%
        reshape2::melt(id.vars = c("Station", "datetime", "survey", "device_id", "data_level", "site", "device", "device_type")) %>%
        dplyr::arrange(datetime) %>%
        dplyr::filter(variable != "TideCorr") %>%
        dplyr::filter(variable != "DriftCorr") %>%
        dplyr::filter(variable != "StdDev") %>%
        dplyr::filter(variable != "X") %>%
        dplyr::filter(variable != "Y") %>%
        dplyr::select(Station, datetime, survey, variable, value, device_id, data_level, site, device, device_type)
    }
    
    ## get all different devices
    devices = unique(data_means$device_id)

    # construct result dataframe for all devices
    data_all_devices = data.frame()
    
    # run loop for each device
    for(dev_id in devices){
    # filter data for one device
    # raw data
    data_raw_device = data_in_raw %>%
       dplyr::filter(device_id == dev_id)
    # base station data
    data_basestation = data_means %>%
      dplyr::filter(Station == base_station,
                    device_id == dev_id)
    ## calculate differences in between same base station 
    # time differences in [min]
    data_basestation$timedif = 0
    data_basestation$gravitydif = 0
    for(i in 2:dim(data_basestation)[1]){
        data_basestation$timedif[i] = as.numeric(difftime(data_basestation$datetime[i], data_basestation$datetime[i-1], units="mins"))
        data_basestation$gravitydif[i] = data_basestation$value[i] - data_basestation$value[i-1]
    }
    
    if(!is.na(gravRef)){
      # load residual data
      gravRes = load(file_gravityResidual)
      gravRes = get(gravRes)
      # interpolate hourly time series (residual) to minutely
      gravRes_minutes = interpTS(gravRes, freq_in = "min", freq_out = NA, aggFunc = "mean", data_col_name = "value")
      # change column name, for correct data joinging
      colnames(gravRes_minutes)[2] = "valueGref"
      # !!
      # convert units !!
      # loaded residuals are usually in nm/s²
      if(org_units == "mGal"){
          gravRes_minutes$valueGref = gravRes_minutes$valueGref / 1e4
        }
      if(org_units == "µGal"){
          gravRes_minutes$valueGref = gravRes_minutes$valueGref / 1e1
        }
      if(org_units == "nms-2"){
          # nothing, residuals are usually in nm/s²
        }
      # find data for drift estimation time points
      gravRes_driftPoints = data_basestation %>%
          dplyr::left_join(gravRes_minutes, by = "datetime")
      # calculate difference in gravity of reference gravimeter
      gravRes_driftPoints$gravitydif_gRef = 0
      for(i in 2:dim(gravRes_driftPoints)[1]){
          gravRes_driftPoints$gravitydif_gRef[i] = gravRes_driftPoints$valueGref[i] - gravRes_driftPoints$valueGref[i-1]
      }
      data_basestation = gravRes_driftPoints %>%
          dplyr::mutate(gravitydif = gravitydif + gravitydif_gRef) %>%
          dplyr::select(-gravitydif_gRef, -valueGref)
    }
    
    ## calulate driftrate
    data_basestation = data_basestation %>%
      dplyr::mutate(driftrate = gravitydif / timedif)
    
    ## construct cummulated drift time series
    data_start = min(data_raw_device$datetime)
    data_end = max(data_raw_device$datetime)
    
    ## get in between timestamps
    # from n basestation measurements
    # find number of basestation measurements
    n_basestation = dim(data_basestation)[1]
    
    # create empty dataframe
    drift_TS = data.frame()
    
    for(n in 1:(n_basestation - 1)){
      # get intermediate timestamps
      drift_start = data_basestation$datetime[n]
      drift_end = data_basestation$datetime[n+1] - 1
      drift_rate = data_basestation$driftrate[n+1]
      if(n == 1)
          drift_start = data_start
      if(n == (n_basestation - 1))
          drift_end = data_end
      # construct time sequence and data.frame
      drift_temp = data.frame(
          datetime = seq(drift_start, drift_end, by = "min"),
          drift = drift_rate
          )
      # cumsums over drift rate
      drift_temp$drift = cumsum(drift_temp$drift)
      # combine all drift time periods
      # so different drifts can be in one data.frame
      drift_TS = rbind(drift_TS, drift_temp)
    }
    
    # cut datetime of drift ts to minutes
    drift_TS$datetime = as.POSIXct(trunc(drift_TS$datetime, "min"), tz = "UTC")
    
    data_drift_corrected = data_raw_device %>%
      dplyr::mutate(datetime = as.POSIXct(trunc(datetime, "min"), tz = "UTC")) %>%
      # join with complete drift time series
      dplyr::left_join(drift_TS, by="datetime") %>%
      dplyr::mutate(value = value - drift) %>%
      dplyr::mutate(variable = "gravity_driftCor")


     # as.data.frame(data_raw_device)
     # as.data.frame(data_drift_corrected) 
    ## calculate station mean values
    data_drift_corrected_mean = CG6_station_mean(
                          data_in = data_drift_corrected,
                          add_tides = F,
                          add_longTdrift = F,
                          cutoff_sd = NA,
                          cutoff_tilt = NA
    )
    # rename variable as standard naming from station_mean-function is "CorrGrav_mean"
    # but these are already drift corrected values
    data_drift_corrected_mean$variable = "gravity_driftCor"
    
    # combine data
    data_all_devices = rbind(data_all_devices, data_drift_corrected_mean)
    # end of device for loop
    }
    
    # return data
    return(data_all_devices)
}

