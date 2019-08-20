#' @title Correction of CRNS data for water vapor
#'
#' @description test
#'
#' @param ts_temp Smoothed time series of temperature in [C/h].
#' 
#' @return test
#' 
#' @details after Rosolem et al. (2013)
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CRNS_watervaporCorrection = function(
    ts_temp
){
    es0 = 6.112*exp((17.67*ts_temp)/(243.5+ts_temp))
    e0 = ts_temp*es0
    pv0 = (e0/(461.5*(ts_temp+273.15)))*1000
    pv0Ref = mean(pv0, na.rm = TRUE)
    cor_factor = (1+0.0054*(pv0 - pv0Ref))
    return(cor_factor)
}

