#' @title Read CG6 Gravity survey data
#'
#' @description test
#'
#' @param test test
#' 
#' @return test
#' 
#' @details missing
#' @references Marvin Reich (2019), mreich@@posteo.de
#' @import 
#' @examples missing

CG6_readData = function(
      file_path,
      survey_name,
      device,
      output_cols = c("CorrGrav", "StdDev", "X", "Y", "TideCorr"),
      date_format = c("%Y-%m-%d %H:%M:%S")
){

    # ## debugging
    # file_path = "/home/mreich/Dokumente/TERENO/Data/Gravity/CG6_gradientMessungen/April2019/"
    # survey_name = "TERENO190429"
    # device = "58"
    # output_cols = c("CorrGrav", "StdDev", "X", "Y", "TideCorr"),
    # date_format = c("%Y-%m-%d %H:%M:%S")
    
    ####################
    ## read data
    ####################
    # header_CG6_058 = read.table(file = paste0(dir_CG6_058, "CG-6_0058_TEST_A10_INNEN.dat"),
    #                           skip = 20, nrows = 1, sep = "\t", header = F)
    ####################
    ## CG-6 data
    data_in = read.table(file = paste0(file_path, "CG-6_00", device, "_", survey_name, ".dat"),
                              skip = 20, sep = "\t", dec = ".", header = T)
    ####################
    ## format data
    # rename first column
    colnames(data_in)[1] = "Station"
    # construct one datetime column
    data_in$datetime = as.POSIXct(strptime(paste0(data_in$Date, " ", data_in$Time), format = date_format))
    ####################
    ## select desired columns
    # add datetime column and Station
    output_cols = c("datetime", "Station", output_cols)
    # add column 'device_id'
    data_in_selected = dplyr::select(data_in, output_cols) %>%
        dplyr::mutate(device_id = as.character(device))

    
    ####################
    ## return data
    return(data_in_selected)
    
}

