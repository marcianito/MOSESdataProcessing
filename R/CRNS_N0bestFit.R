#' @title Estimate best fitting N0 value
#'
#' @description test
#'
#' @param test test
#' 
#' @return best fitting N0 value is returned.
#' 
#' @details requires a independently measured / estimated soil moisture record time series.
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CRNS_N0bestFit = function(
    ts_counts,
    ts_soilMoisture,
    N0_min = 500,
    N0_max = 1500,
    N0_interval = 0.1,
    bulkdensity = 1,
    fittingCriteria = "rmse"
){
    #

    ## create N0 range
    N0s = seq(N0_min, N0_max, N0_interval)
    N0_results = data.frame()
    # loop through N0s
    for(N0 in N0s){
        sm = CRNS_countsToSM(ts_counts, N0, bulkdensity)
        # evaluate fitting criteria
        switch(fittingCriteria,
               rmse: {
                   fit_val = rmse(data_combined$sm_sim, data_combined$sm_obs)
               },
               nse: {
                   fit_val = nse(data_combined$sm_sim, data_combined$sm_obs)
               },
               kge: {
                   fit_val = kge(data_combined$sm_sim, data_combined$sm_obs)
               }
        )
        # store results
        N0_results = rbind(N0_results,
                     cbind(N0 = N0, fit = fit_val)
                     )
    }
    # estimate best fitting N0 value
        switch(fittingCriteria,
               rmse: {
                N0_bestFit = N0_results$N0[which.min(N0_results$fit)]
               },
               nse: {
                N0_bestFit = N0_results$N0[which.max(N0_results$fit)]
               },
               kge: {
                N0_bestFit = N0_results$N0[which.max(N0_results$fit)]
               }
        )
    # return optimized N0 value
    return(N0_bestFit)
}

