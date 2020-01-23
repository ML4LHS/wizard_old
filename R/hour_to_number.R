#' Hour to number
#'
#' convert the user argument which is a period object to a numeric value
#'
#' @param window_size window size
#' @param lookback lookback
#' @param lookahead lookahead
#' @param step step
#' @return ist of values
#' @export

hour_to_number = function(window_size = NULL,lookback =NULL,lookahead = NULL, step = NULL){

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

  if (!is.null(window_size) & !is.null(lookback) & !is.null(lookahead) & !is.null(step) ){

    
    # Identifying the units of step function 
    if( tolower(class(step)[[1]]) == "period")
    {
    
    units = ""
   time_list = stringr::str_split(step," ")
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
     
    }
    else{
      stop(" The step variable should be a valid period object.")
    }
  

    # object = list(window_size,lookback,lookahead)
    # units = ""
    # 
    # period_list = lapply(object,function(a)
    # {  if (tolower(class(a)[1]) == "period"){
    #   time_list = stringr::str_split(a[[1]], " ")
    # 
    #   for ( x in range(1, length(time_list[[1]])))
    #   {
    #     if(as.numeric(stringr::str_extract_all(time_list[[1]][1],"\\d+")) > 0){
    #       if (length(time_list[[1]]) == 3){
    #         if(x == 1){
    #           units = "hours"
    #           break
    #         }
    #         if(x == 2){
    #           units = "minutes"
    #           break
    #         }
    #         if(x == 3){
    #           units = "seconds"
    #           break
    #         }
    #       }
    #       else{
    #         if(x == 1){
    #           units = "years"
    #           break
    #         }
    #         if(x == 2){
    #           units = "months"
    #           break
    #         }
    #         if(x == 3){
    #           units = "days"
    #           break
    #         }
    #         if(x == 4){
    #           units = "hours"
    #           break
    #         }
    #         if(x == 5){
    #           units = "minutes"
    #           break
    #         }
    #         if(x == 6){
    #           units = "seconds"
    #           break
    #         }
    #       }
    #     }
    #   }
    #   # temporal_data = temporal_data %>%
    #   #   mutate(admit_time = parse_date_time(admit_time,
    #   #                                       orders = c("ymd_HMS","dmy_HMS","mdy_HMS"))) %>%
    #   #   mutate(time = time_length(interval(admit_time,time), units))
    # }
    #   #print(units)
    #   #}
    #   units})


  
      if(units != ""){

        if (units == "years"){
          window_size = lapply(window_size,function(x) {lubridate::period_to_seconds(x)/(3600*24*7*365)})
          lookback = lapply(lookback,function(x) {lubridate::period_to_seconds(x)/(3600*24*7*365)})
          lookahead =lubridate::period_to_seconds(lookahead)/(3600*24*7*365)
          step = lubridate::period_to_seconds(step)/(3600*24*7*365)
        }

        if (units == "weeks"){
          window_size = lapply(window_size, function(x) {lubridate::period_to_seconds(x)/(3600*24*7)})
          lookback = lapply(lookback, function(x) {lubridate::period_to_seconds(x)/(3600*24*7)})
          lookahead = lubridate::period_to_seconds(lookahead)/(3600*24*7)
          step = lubridate::period_to_seconds(step)/(3600*24*7)
        }
        if (units == "days"){
          window_size = lapply(window_size,function(x) {lubridate::period_to_seconds(x)/(3600*24)})
          lookback = lapply(lookback, function(x) { lubridate::period_to_seconds(x)/(3600*24)})
          lookahead = lubridate::period_to_seconds(lookahead)/(3600*24)
          step = lubridate::period_to_seconds(step)/(3600*24)
        }

        if (units == "hours"){
          print('Returning hours')
          window_size = lapply(window_size, function(x) {lubridate::period_to_seconds(x)/3600})
          print('Defined window_size. About to convert period to seconds for lookback.')
          lookback = lapply(lookback, function(x) {lubridate::period_to_seconds(x)/(3600)})
          print('Converted period to seconds for lookback.')
          lookahead = lubridate::period_to_seconds(lookahead)/(3600)
          print('Converted period to seconds for lookahead.')
          step = lubridate::period_to_seconds(step)/(3600)
          print('Converted period to seconds for step.')
        }
        if (units == "mins"){
          print('Returning minutes')
          window_size = lapply( window_size, function(x){lubridate::period_to_seconds(x)/60})
          lookback = lapply(lookback, function(x) {lubridate::period_to_seconds(x)/(60)})
          lookahead = lubridate::period_to_seconds(lookahead)/(60)
          step = lubridate::period_to_seconds(step)/(60)
        }
        if (units == "secs"){
          print('Returning seconds')
          window_size = lapply(window_size, function(x) {lubridate::period_to_seconds(x)})
          lookback = lapply(lookback, function(x) {lubridate::period_to_seconds(x)})
          lookahead = lubridate::period_to_seconds(lookahead)
          step = lubridate::period_to_seconds(step)
        }
      }
    else{
      stop("Please validate the data type of the step argument.")
    }
    
  }
  else{
    stop("Please fill in the lookback,lookahead,window size,and step arguments of the function.")
  }

  
  return(list("units" = units, "lookback" = lookback, "lookahead" = lookahead , "window_size" = window_size,"step"=step))
}







