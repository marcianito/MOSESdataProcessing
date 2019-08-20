#' @title Correct CRNS data for neutron influx
#'
#' @description test
#'
#' @param ts_Nnm Neutron counts in [counts 7 sec].
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CRNS_neutroninfluxCorrection = function(
    ts_Nnm,
    interpolate = F,
    f_in = NA,
    f_out = NA,
    data_col = NA
){
    # exclude NAs
    if(interpolate){
        Nnm_interp = interpTS(data_in = ts_Nnm, freq_in = f_in, freq_out = f_out, data_col_name = data_col)
        Nnm_cont = Nnm_interp$value
    }else{
        Nnm_cont = ts_Nnm$value
    }
    Navg = mean(Nnm_cont, na.rm = TRUE)
    cor_factor = Navg/Nnm_cont
    return(cor_factor)
}

