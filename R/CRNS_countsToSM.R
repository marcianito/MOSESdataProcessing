#' @title convert neutron counts to soil moisture
#'
#' @description test
#'
#' @param ts_counts data.frame. time series of (corrected) neutron counts with columns (datetime, <value>).
#' @param N0 neutron count reference value.
#' @param bulkdensity average bulk density of soil to use. Default = 1.
#' 
#' @return test
#' 
#' @details after Desiltes et al (2010).
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CRNS_countsToSM = function(
    ts_counts,
    N0,
    bulkdensity = 1
){
    # calculate soil moisture
    soilMoisture = data.frame(
                    datetime = ts_counts$datetime,
                    value = (((0.0808/((ts_counts[,2]/N0)-0.372))-0.115)*bulkdensity)
    )
    return(soilMoisture)
}

