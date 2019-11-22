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
    stop("Please provide the fixed data frame to use period objects for lag parameters")
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
  else if(class(fixed_data) %in% c("data.frame","tibble","disk.frame","data.table")){
    print("just in")
    temporal_data = temporal_data %>%
      dplyr::inner_join(fixed_data %>%
                          dplyr::select(encounter_id, admit_time = time),
                        by = "encounter_id")
    #%>% as.disk.frame(shardby = "encounter_id", overwrite = T)
    # final_data %>% head() %>% View()
    #dummy = final_data %>% filter(!is.na(time)) %>% filter(!is.na(admit_time)) %>% head()
  }


  if (tolower(class(temporal_data$admit_time)[1]) %in% c("character","date","posixct") & tolower(class(temporal_data$time)[1]) %in% c("character","date","posixct") ){
    print("did i")
    if(any(stringr::str_detect(temporal_data$admit_time,regex("-|/")) == TRUE) |any(stringr::str_detect(temporal_data$time,regex("-|/")) == TRUE)){

      #final_data = final_data %>%
      temporal_data = temporal_data %>%
        dplyr::mutate(admit_time = lubridate::parse_date_time(admit_time, orders = c("ymd_HMS","dmy_HMS","mdy_HMS","ymd","mdy","dmy"))) %>%
        dplyr::mutate(time = lubridate::parse_date_time(time, orders = c("ymd_HMS","dmy_HMS","mdy_HMS","ymd","mdy","dmy"))) %>%
        #mutate(time = time_length(interval(admit_time,time), period_measure$units)) %>%
        dplyr::mutate(time = lubridate::time_length(lubridate::interval(admit_time,time),units)) %>%
        dplyr::select(-admit_time)

      print("ran")
    }
    else{
      stop("Please provide a valid date object for the time to use the period object")
    }
  }
  else if (class(temporal_data$admit_time)[1] %in% c("integer","numeric","double") |class(temporal_data$time)[1] %in% c("integer","numeric","double") ){
    stop("Please provide a valid date type object for the time to use the period object")
  }
  else{
    print( class(temporal_data$time))
    stop("something went wrong")
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
