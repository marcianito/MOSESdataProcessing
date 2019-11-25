#' @title Manual correction (exclusion) for CG6 Gravity survey data
#'
#' @description Due to standard devitation and/or tilts
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2018), mreich@@posteo.de
#' @import 
#' @examples missing

CG6_manualCorrection = function(
      correction_file,
      data_raw,
      survey_name,
      reps_per_station
){
    ## debugging
    ##########
    
    ## read correction data
    corrections = read.table(correction_file,
                             header = T,
                             sep = ";",
                             dec = ".",
                             stringsAsFactors = F
                             )
    # filter corrections for corresponding survey
    corrections_survey = corrections %>%
      dplyr::filter(Survey == survey_name)
    # force columns to character-type
    corrections_survey$remove_due_sd = as.character(corrections_survey$remove_due_sd)
    corrections_survey$remove_due_tilt = as.character(corrections_survey$remove_due_tilt)
    
    # corrections due to standard devitation violation
    cor_list = vector()
    for(i in 1:dim(corrections_survey)[1]){
      # get string to numeric vector for station correction
      cor_stdDev_temp = as.numeric(strsplit(corrections_survey$remove_due_sd[i], ",")[[1]])
      cor_tilt_temp = as.numeric(strsplit(corrections_survey$remove_due_tilt[i], ",")[[1]])
      # run only if correction desired
      # standard deviation
      if(cor_stdDev_temp[1] != 0){
      # reference to complete data lengths, considering all repitions per station
      cor_stdDev_temp = cor_stdDev_temp + reps_per_station * (i-1)
      cor_list = c(cor_list, cor_stdDev_temp)
      }
      # tilts
      # run only if correction desired
      if(cor_tilt_temp[1] != 0){
      # reference to complete data lengths, considering all repitions per station
      cor_tilt_temp = cor_tilt_temp + reps_per_station * (i-1)
      cor_list = c(cor_list, cor_tilt_temp)
      }
      # remove possible duplicates
      cor_list = unique(cor_list)
    }
    
    # exclude desired rows from survey data
    data_cleaned = data_raw[-cor_list, ]

    # return data
    return(data_cleaned)
}

