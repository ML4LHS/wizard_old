#' Dummy outcome variable
#'
#' The function does not spread the outcome variable to
#'
#' @param temporal_data temporal df
#' @param outcome_var outcome variable
#' @param outcome_stat outcome statistics
#' @param lookahead lookahead
#' @param window_size window size
#' @return lag frame
#' @export
#'

dummy_outcome_variable =  function( temporal_data,
                                    outcome_var = NULL,
                                    outcome_stat = list("mean"),
                                    lookahead = 30,
                                    window_size = 6
){

  max_time = max(temporal_data %>% dplyr::filter(variable == outcome_var) %>% dplyr::select(time), na.rm = T)
  time_period = floor(max_time/window_size)*window_size
  print(time_period)
  k = dplyr::if_else(min(temporal_data$time, na.rm = T)<0,0,min(temporal_data$time, na.rm = T))
  min_time = k
  outcome_table = NULL

  while(k < time_period)
  {
    
    temporal_data_k = temporal_data %>%
      dplyr::select(time,variable,value,encounter_id) %>%
      dplyr::mutate(time = floor(time/window_size)*window_size,
             time = time - k) %>%
      dplyr::mutate_at(vars(value), as.numeric) %>%
      #group_by(encounter_id,
      # time) %>%
      dplyr::filter((time > 0 & (time <= floor(lookahead/window_size)*window_size - window_size)) & (variable == outcome_var)) %>%
      dplyr::mutate(operation = paste(outcome_stat,collapse = ",")) %>%
      tidyr::separate_rows(operation,sep =",")
    if (nrow(temporal_data_k) >0) {
      temporal_data_k = temporal_data_k %>%
        dplyr::group_by(encounter_id,time) %>%
        dplyr::mutate(value = dplyr::case_when(operation == "mean"~ mean(value,na.rm =TRUE),
                                 operation == "min"~min(value,na.rm =TRUE),
                                 operation == "max" ~ max(value,na.rm =TRUE),
                                 T ~ NA_real_),
               variable = paste("OUTCOME",operation,outcome_var,lookahead,sep = "_")) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(value)) %>%
        #filter(value != -Inf) %>%
        dplyr::mutate(value = case_when( value %in% c(NaN,-Inf,Inf) ~ NA_real_, T ~ value) ) %>%
        dplyr::group_by(encounter_id,time) %>%
        dplyr::filter(!all(is.na(value))) %>%
        # slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(encounter_id,variable) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(encounter_id,variable,key = operation,value) %>%
        #spread(operation,value) %>%
        dplyr::mutate(time = floor(k) , lag = min(temporal_data$time))
      outcome_table = dplyr::bind_rows(outcome_table, temporal_data_k)
    }

    k = k+ window_size
  }

  return(outcome_table)

}
