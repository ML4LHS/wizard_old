#' Date to time
#'
#' Convert the timestamps in the temporal data to Number relative to the admit time or initial time.
#'
#' @param temporal_data temporal df
#' @param fixed_datafixed df
#' @param units units
#' @return data frame
#' @export

date_to_time = function(temporal_data = temporal_data, fixed_data = fixed_data, units)
{

  # if (tolower(class(lookback)[1]) == "period" & tolower(class(lookahead)[1]) == "period" & tolower(class(window_size)[1]) == "period"){
  # print("Got here")
  if(is.null(fixed_data)){
    stop("Please provide a data frame with date of first visit or admit date.")
  }
  else if(class(fixed_data)[1] == "character"){
    fixed_data = disk.frame::csv_to_disk.frame(infile = fixed_data,
                                               shardby = "encounter_id",
                                               backend = "data.table")
    temporal_data = temporal_data %>%
      dplyr::inner_join(fixed_data %>%
                          dplyr::select(encounter_id, admit_time = time),
                        by ="encounter_id")

    #dummy = final_data %>% filter(!is.na(time)) %>% filter(!is.na(admit_time)) %>% head()
  }
  else if(class(fixed_data)[[1]] %in% c("data.frame","tibble","disk.frame","data.table","tbl_df")){
    #print("just in")
    temporal_data = temporal_data %>%
      dplyr::inner_join(fixed_data %>%
                          dplyr::select(encounter_id, admit_time = time),
                        by = "encounter_id")
    #%>% as.disk.frame(shardby = "encounter_id", overwrite = T)
    # final_data %>% head() %>% View()
    #dummy = final_data %>% filter(!is.na(time)) %>% filter(!is.na(admit_time)) %>% head()
  }


  if (tolower(class(temporal_data$admit_time)[1]) %in% c("character","date","posixct") & tolower(class(temporal_data$time)[1]) %in% c("character","date","posixct") ){
    #print("did i")
    if(any(stringr::str_detect(temporal_data$admit_time,stringr::regex("-|/")) == TRUE) |any(stringr::str_detect(temporal_data$time,stringr::regex("-|/")) == TRUE)){

      #final_data = final_data %>%
      temporal_data = temporal_data %>%
        dplyr::mutate(admit_time = lubridate::parse_date_time(admit_time, orders = c("ymd_HMS","dmy_HMS","mdy_HMS","ymd","mdy","dmy"))) %>%
        dplyr::mutate(time = lubridate::parse_date_time(time, orders = c("ymd_HMS","dmy_HMS","mdy_HMS","ymd","mdy","dmy"))) %>%
        #mutate(time = time_length(interval(admit_time,time), period_measure$units)) %>%
        dplyr::mutate(time = lubridate::time_length(lubridate::interval(admit_time,time),units)) %>%
        dplyr::select(-admit_time)

     # print("ran")
    }
    else{
      stop("Please make sure if the data type of the time column in fixed dataframe is similar to that of temporal data frame")
    }
  }
  else if (class(temporal_data$admit_time)[1] %in% c("integer","numeric","double") |class(temporal_data$time)[1] %in% c("integer","numeric","double") ){
    stop("The time variable in fixed data frame and temporal data frame should be a timestamp")
  }
  else{
    print( class(temporal_data$time))
    stop("The fixed data should be a data frame,tibble,disk frame or data table to compute the time lags.")
  }
  # }
  # else if( tolower(class(lookback)[1]) == "numeric" & tolower(class(lookahead)[1]) == "numeric" & tolower(class(window_size)[1]) == "numeric"){
  #   final_data = final_data %>%
  #     mutate(time = as.numeric(time))
  #   if (any(!is.na(final_data$time))){
  #     stop(" The time variable should be of data type numeric")
  #   }
  # }
  # else{
  #   stop("The data type of period parameters and the time variable of the data frame should be the same")
  # }

  temporal_data

}
