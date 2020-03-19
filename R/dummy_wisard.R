
# ' Lagged feature
#' @param temporal_data temporal dataframe
#' @param  window_size Window_size
#' @param step Step variable
#' @param lookback lookaback
#' @param impute T/F
#' @export

lagged_feature =  function(temporal_data = NA,
                           obj = obj,
                           window_size = NA,
                           step = NA,
                           lookback = NA,
                           feature_stat = NA,
                           impute = F){
  start = Sys.time()
  first_frame = temporal_data %>% 
    group_by(encounter_id) %>% 
    mutate(max_time = max(time,na.rm = T)) %>% 
    ungroup() %>% 
    group_by(category) %>% 
    group_map(~wizard::check_mapper(.x = .x ,.y = .y,window_size = window_size,step = step,lookback = lookback,feature_stat = feature_stat)) %>% 
    bind_rows() 
  
  #inter <<- first_frame %>% as_tibble()
  
  print("Computed the first frame")
  
  interm <<- first_frame %>% as_tibble()
  
  final_frame = first_frame %>% 
    group_by(category) %>% 
    group_map(~wizard::lagged_feature_generator(.x = .x, .y =.y,lookback = lookback, window_size = window_size,step = step)) %>% 
    bind_rows()
  
  print("Computed the final frame as well!!!")
  
  #inter <<- final_frame %>% as_tibble()
  
  if (impute){
    #print("I'm here")
    final_frame = final_frame %>%
      dplyr::group_by(encounter_id,variable) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(value = zoo::na.locf(value, na.rm = F)) %>%
      dplyr::ungroup()
    
  }
  
  print("Finished imputing data")
  
  obj$wizard_frame = disk.frame::add_chunk(obj$wizard_frame,final_frame)
  rm(final_frame,first_frame)
  
  #final_frame
}

#' final spread data
#' @param temporal_dataframe
#' @param outcome_var
#' @param all_variables_to_create
#' @export




final_spread_data = function(temporal_data,outcome_var,all_variables_to_create,obj){
  
  print(outcome_var)
  
  if(is.na(outcome_var)){
    final_frame = temporal_data %>%
      select(-category) %>% 
      dplyr::mutate(variable = factor(variable, levels = all_variables_to_create )) %>%
      dplyr::filter(!is.na(variable)) %>%
      dplyr::select(-lag) %>%
      dplyr::group_by(encounter_id,variable,time) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr:: group_by(encounter_id,time) %>%
      dplyr::select(-key) %>%
      tidyr::spread(variable, value) %>% #drop= FALSE is removed.
      dplyr::ungroup()
  }
  else{
    
    final_temp <<- temporal_data
    
    final_frame = temporal_data %>%
      select(-category) %>% 
      dplyr::mutate(variable = factor(variable, levels = all_variables_to_create)) %>%
      dplyr::filter(!is.na(variable)) %>%
      dplyr::select(-lag) %>%
      dplyr::group_by(encounter_id,variable,time) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(encounter_id,time) %>%
      dplyr::select(-key) %>%
      tidyr::spread(variable, value, drop = FALSE)  %>%
      dplyr::ungroup() %>%
      dplyr::filter_at(vars(starts_with("OUTCOME_")),all_vars(!is.na(.)))
  }
  
  final_frame = final_frame %>%
    dplyr::arrange(encounter_id,time)
  
  disk.frame::add_chunk(obj$result_frame, final_frame)
  rm(final_frame)
}




