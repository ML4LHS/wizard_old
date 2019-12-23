#' Dummy Wisard
#'
#' @param temporal_data temporal df
#' @param all_variable_to_create vars to create
#' @param lag_variable_to_create lag vars to create
#' @param window_size window size
#' @param outcome_var outcome variable
#' @param outcome_stat outcome stats
#' @param lookahead lookahead
#' @param lookback lookback
#' @param lag T/F create lags
#' @param impute T/F impute
#' @return final_frame
#' @export

dummy_wisard =  function( temporal_data,
                          all_variable_to_create = all_variable_to_create,
                          lag_variable_to_create = lag_variable_to_create,
                          window_size = 6,
                          outcome_var = NULL,
                          outcome_stat = list("mean","min","max"),
                          lookahead = 30,
                          lookback = 48,
                          lag = FALSE,
                          impute = T
)
{

  # Here we create two frame the first frame is initial frame with summary stats for most recent values.
  # The final frame is a combined frame with first frame and the lagged features frame.
  
  print("Entered the dummy wisard function")
  
  first_frame = temporal_data %>% 
    group_by(encounter_id) %>% 
    mutate(max_time = max(time,na.rm = T)) %>% 
    ungroup() %>% 
    group_by(category) %>% 
    group_map(~check_mapper(.x = .x ,.y = .y)) %>% 
    bind_rows() 
  
  print("Computed the first frame")
  
  
  final_frame = first_frame %>% 
    group_by(category) %>% 
    group_map(~lagged_features(.x = .x, .y =.y,recent_only = T)) %>% 
    bind_rows()

  print("Computed the final frame as well!!!")
  
  # first_frame = temporal_data %>%
  #   dplyr::mutate_at(vars(value), as.numeric) %>%
  #   dplyr::filter(time >= 0) %>%
  #   dplyr::mutate(actual_time = time,time = floor(time/window_size)*window_size) %>%
  #   #group_by(encounter_id) %>%
  #   dplyr::mutate(time = factor(time,levels = seq(min(.$time,na.rm =T), max(.$time, na.rm = T),window_size))) %>%
  #   dplyr::filter(!is.na(time)) %>%
  #   tidyr::complete(encounter_id,variable,time) %>%
  #   dplyr::group_by(encounter_id) %>%
  #   dplyr::filter( as.numeric(as.character(time)) <= max(actual_time, na.rm = T) )%>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(encounter_id,variable,time) %>%#
  #   dplyr::summarise_each( funs(mean,min,max,n = . %>% na.omit %>% length, slope = wisard::slope(actual_time,value)),value) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(encounter_id,time) %>%
  #   tidyr::gather(key = "key",
  #          value = "value",
  #          mean:slope) %>%
  #   dplyr::mutate(key = case_when( key == "n_distinct" ~ "n", T ~ key)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(value = case_when( value %in% c(NaN,-Inf,Inf) ~ NA_real_, T ~ value) ) %>%
  #   #mutate(time = )
  #   dplyr::mutate( time = as.numeric(as.character(time))) %>%
  #   dplyr::mutate(time = time - min(time[which(time >= 0)]),lag = min(time))
  # 
  # final_frame = NULL
  # if( lookback > 0 ){
  #   for ( i in seq(1,(lookback/window_size)-1,1))
  #   {
  # 
  #     final_frame = dplyr::bind_rows( final_frame,
  #                              first_frame %>%
  #                                dplyr::group_by(encounter_id,variable,key) %>%
  #                                dplyr::mutate(new_value = data.table::shift(value,i, type = "lag")) %>%
  #                                dplyr::mutate(lag = lag + i*window_size) %>%
  #                                dplyr::ungroup() %>%
  #                                dplyr::mutate(variable = paste(variable,key,lag, sep = "_")) #%>%
  #     )
  #     print(i)
  #   }
  # 
  #   final_frame = dplyr::bind_rows(first_frame %>%
  #                                    dplyr::mutate(variable = paste(variable,key, lag , sep ="_")) %>%
  #                                    dplyr::mutate(lag = time),
  #                           final_frame %>%
  #                             dplyr::select(encounter_id,variable,time,key,-value,value = new_value, lag)
  #   )
  # 
  #   if(lag){
  #     lag_frame = wisard::diff_feature(final_frame,
  #                              window_size)
  # 
  #     final_frame = dplyr::bind_rows(final_frame,
  #                             lag_frame)
  #     all_variable_to_create = append(all_variable_to_create,lag_variable_to_create)
  #   }
  # 
  # }
  # else{
  #   final_frame = first_frame %>%
  #     dplyr::mutate(variable = paste(variable,key, lag , sep ="_")) %>%
  #     dplyr::mutate(lag = time)
  #   print("here")
  # }


  
  
    if (impute){
    #print("I'm here")
    final_frame = final_frame %>%
      dplyr::group_by(encounter_id,variable) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(value = zoo::na.locf(value, na.rm = F)) %>%
      dplyr::ungroup()

  }
  print("done imputing the data")


  if (!is.null(outcome_var)){
    outcome_tab = dummy_outcome_variable(temporal_data = temporal_data,
                                         outcome_var = outcome_var,
                                         outcome_stat = outcome_stat,
                                         lookahead = lookahead,
                                         window_size = window_size )

    final_frame = dplyr::bind_rows(final_frame,
                            outcome_tab)
    all_variable_to_create = c(all_variable_to_create, outcome_tab$variable %>% data.table::unique()  )

    final_frame = final_frame %>%
      dplyr::group_by(encounter_id,time) %>%
      dplyr::filter(any(str_detect(variable,"OUTCOME_"))) %>%
      dplyr::ungroup()
  }

  #print(all_variable_to_create)

  print("done gathering all the data")
  print(class(final_frame))
  #return (final_frame)
  # if (impute){
  #   #print("I'm here")
  #   final_frame = final_frame %>%
  #     group_by(encounter_id,variable) %>%
  #     arrange(time) %>%
  #     mutate(value = na.locf(value, na.rm = F)) %>%
  #     ungroup()
  #
  # }
  if(is.null(outcome_var)){
    final_frame = final_frame %>%
      dplyr::mutate(variable = factor(variable, levels = all_variable_to_create)) %>%
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
    final_frame = final_frame %>%
      dplyr::mutate(variable = factor(variable, levels = all_variable_to_create)) %>%
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

  print("Completed spreading the data frame.")



  final_frame = final_frame %>%
    dplyr::arrange(encounter_id,time)
  # print(impute)
  # if (impute){
  #   #print("I'm here")
  #   final_frame = final_frame %>%
  #     group_by(encounter_id) %>%
  #     mutate_at(all_variable_to_create, funs(na.locf(.,na.rm = FALSE)))
  #
  # }
  #
  #



  final_frame

}
