#' Hour to number
#'
#' convert the user argument which is a period object to a numeric value
#'
#' @param window_size window size
#' @param lookback lookback
#' @param lookahead lookahead
#' @return ist of values
#' @export

hour_to_number = function(window_size = NULL,lookback =NULL,lookahead = NULL){

  # if( fixed_data %>% distinct(encounter_id)%>% nrow()  !=  temporal_data %>%  distinct(encounter_id) %>% nrow() ){
  #   warning("Mismatch in encounter id's between the fixed and temporal data. You may lose some information")
  # }
  #
  #
  # temporal_data = temporal_data %>%
  #   inner_join(fixed_data %>%
  #                select(encounter_id,admit_time = time),
  #              by = "encounter_id")
  #
  #
  #
  #
  # temporal_data = temporal_data %>%
  #   mutate(admit_time = parse_date_time(admit_time,
  #                                       orders = c("ymd_HMS","dmy_HMS","mdy_HMS","ymd","mdy","dmy"))) %>%
  #   mutate(time = time_length(interval(admit_time,time), units))
  #

  if (!is.null(window_size) & !is.null(lookback) & !is.null(lookahead)){

    object = list(window_size,lookback,lookahead)
    units = ""

    period_list = lapply(object,function(a)
    {  if (tolower(class(a)[1]) == "period"){
      time_list = stringr::str_split(a[[1]], " ")

      for ( x in range(1, length(time_list[[1]])))
      {
        if(as.numeric(stringr::str_extract_all(time_list[[1]][1],"\\d+")) > 0){
          if (length(time_list[[1]]) == 3){
            if(x == 1){
              units = "hours"
              break
            }
            if(x == 2){
              units = "minutes"
              break
            }
            if(x == 3){
              units = "seconds"
              break
            }
          }
          else{
            if(x == 1){
              units = "years"
              break
            }
            if(x == 2){
              units = "months"
              break
            }
            if(x == 3){
              units = "days"
              break
            }
            if(x == 4){
              units = "hours"
              break
            }
            if(x == 5){
              units = "minutes"
              break
            }
            if(x == 6){
              units = "seconds"
              break
            }
          }
        }
      }
      # temporal_data = temporal_data %>%
      #   mutate(admit_time = parse_date_time(admit_time,
      #                                       orders = c("ymd_HMS","dmy_HMS","mdy_HMS"))) %>%
      #   mutate(time = time_length(interval(admit_time,time), units))
    }
      #print(units)
      #}
      units})


    if (period_list %>% unique() %>% length == 1 & period_list[[1]]!=""){
      units = period_list[[1]]
      if(units != ""){

        if (units == "years"){
          window_size = lubridate::period_to_seconds(window_size)/(3600*24*7*365)
          lookback = lubridate::period_to_seconds(lookback)/(3600*24*7*365)
          lookahead = lubridate::period_to_seconds(lookahead)/(3600*24*7*365)
        }

        if (units == "weeks"){
          window_size = lubridate::period_to_seconds(window_size)/(3600*24*7)
          lookback = lubridate::period_to_seconds(lookback)/(3600*24*7)
          lookahead = lubridate::period_to_seconds(lookahead)/(3600*24*7)
        }
        if (units == "days"){
          window_size = lubridate::period_to_seconds(window_size)/(3600*24)
          lookback = lubridate::period_to_seconds(lookback)/(3600*24)
          lookahead = lubridate::period_to_seconds(lookahead)/(3600*24)
        }

        if (units == "hours"){
          window_size = lubridate::period_to_seconds(window_size)/3600
          lookback = lubridate::period_to_seconds(lookback)/(3600)
          lookahead = lubridate::period_to_seconds(lookahead)/(3600)
        }
        if (units == "mins"){
          window_size = lubridate::period_to_seconds(window_size)/60
          lookback = lubridate::period_to_seconds(lookback)/(60)
          lookahead = lubridate::period_to_seconds(lookahead)/(60)
        }
        if (units == "secs"){
          window_size = lubridate::period_to_seconds(window_size)
          lookback = lubridate::period_to_seconds(lookback)
          lookahead = lubridate::period_to_seconds(lookahead)
        }
      }
    }
    else{
      stop(" The period objects provided in the arguments are not of same units.")
    }
  }
  else{
    stop("Please provide the lookback lookahead and the window_size objects")
  }

  return(list("units" = units, "lookback" = lookback, "lookahead" = lookahead , "window_size" = window_size))
}
