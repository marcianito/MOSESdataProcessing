#' @title Correct CRNS data for pressure
#'
#' @description test
#'
#' @param ts_pressure Smoothed pressure time series [mbar/h].
#' @param L Effective nucleon attenuation length for high energy neutrons [ g cm^-2]. Default = 135.9.
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CRNS_pressureCorrection <- function(
        ts_pressure,
        L=135.9
){
    x0 = mean(ts_pressure*1.0194, na.rm = TRUE)
    cor_factor = exp((ts_pressure - x0)/L)
    return(cor_factor)
}
