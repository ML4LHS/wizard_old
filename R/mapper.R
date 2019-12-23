# library(tidyverse)
# 
# 
# 
# first_frame = temporal_data %>% 
#   group_by(encounter_id) %>% 
#   mutate(max_time = max(time,na.rm = T)) %>% 
#   ungroup() %>% 
#   group_by(category) %>% 
#   group_map(~check_mapper(.x = .x ,.y = .y)) %>% 
#   bind_rows() 
# 
# 
# 
# final_frame = first_frame %>% 
#   group_by(category) %>% 
#   group_map(~lagged_features(.x = .x, .y =.y,recent_only = T)) %>% 
#   bind_rows()
# 
# column_names = colnames_generator(temporal_data = temporal_data)
# 
# all_variables_to_create = c(column_names[[1]],column_names[[2]])
# 
# 
# # After this all the data are gathered together and we have to just impute the data and combine the outcome
# 
# outcome_tab = dummy_outcome_variable(temporal_data = temporal_data,
#                                      outcome_var = "SBP",
#                                      outcome_stat = list("min","mean"),
#                                      lookahead = 48,
#                                      window_size = step )
# 
# final_frame = dplyr::bind_rows(final_frame,
#                                outcome_tab)
# all_variables_to_create = c(all_variables_to_create, outcome_tab$variable %>% base::unique()  )
# 
# final_frame = final_frame %>%
#   dplyr::group_by(encounter_id,time) %>%
#   dplyr::filter(any(str_detect(variable,"OUTCOME_"))) %>%
#   dplyr::ungroup()
# 
# spread_data = final_frame %>%
#   dplyr::mutate(variable = factor(variable, levels = all_variables_to_create)) %>%
#   dplyr::filter(!is.na(variable)) %>%
#   dplyr::select(-lag) %>%
#   dplyr::group_by(encounter_id,variable,time) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(encounter_id,time) %>%
#   dplyr::select(-key) %>%
#   tidyr::spread(variable, value)  %>%
#   dplyr::ungroup()
  



lagged_features = function(.x,.y,lookback = list("meds"= 7,"labs"= 48),window_size = list("meds" = 1,"labs" = 6),step = 3,lag_display = list("meds" = T, "labs" = T),
                        recent_only = F   ,lag_comp = list("meds" = "both", "labs" = "prop")){
  
  
  start = 1
  final_frame = NULL
  temp_window_size = window_size[[.y$category]] %>% as.numeric()
  temp_lookback = lookback[[.y$category]] %>% as.numeric()
  if(step < temp_window_size){
    start = 2
  }
  
  if( temp_lookback > 0 & lag_display[[.y$category]]){
    for ( i in seq(1,(temp_lookback/temp_window_size)-start,1))
    {
      print(i)
      final_frame = dplyr::bind_rows( final_frame,
                                      .x %>%
                                        filter(lag == max(lag, na.rm = T)) %>% 
                                        dplyr::group_by(encounter_id,variable,key) %>%
                                        dplyr::mutate(new_value = data.table::shift(value,i, type = "lag")) %>%
                                        dplyr::mutate(lag = lag + i*temp_window_size) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::mutate(variable = paste(variable,key,lag, sep = "_")) #%>%
      )
      print(i)
    }
    
    final_frame = dplyr::bind_rows(.x %>%
                                     dplyr::mutate(variable = paste(variable,key, lag , sep ="_")) %>%
                                     dplyr::mutate(lag = time),
                                   final_frame %>%
                                     dplyr::select(encounter_id,variable,time,key,-value,value = new_value, lag)
    )
    
  if(!lag_comp[[.y$category]] %in% c("diff","prop","both")){
    stop("The lag compute function not supported")
  }
  
 # if(lag_display[[.y$category]] == F)
    if(recent_only == T)
    {
  final_frame  = bind_rows(.x %>%
                             filter(lag == 0) %>% 
                             dplyr::mutate(variable = paste(variable,key, lag , sep ="_")),
                           diff_feature(final_frame = final_frame, window_size = temp_window_size, lag_comp[[.y$category]])
                          )

  }

  else
    {
      final_frame = bind_rows( final_frame,
                               diff_feature(final_frame = final_frame, window_size = temp_window_size, lag_comp[[.y$category]])
                               )
    }

  temp_step = step %>% as.numeric()
  print(temp_step)
  if( temp_step >= temp_window_size){

  final_frame = final_frame %>%
    filter(time %% temp_step == 0)

  final_frame
  }
  final_frame
  # else{
  #   stop(" Provide a valid step function for the analysis")
  # }


  }
 
 
}






# The step function 


check_mapper = function(.x,.y,window_size = list("meds" = 1,"labs" = 6), step = 3,  feature_stat = list(labs = c('min', 'mean', 'max'),
                                                                                                  meds = ('min'))){
  count = 1000
  op = c(feature_stat[[.y$category]],"slope(time,value)")
  print(op)
  times = 1
  print(step)
  temp_window_size = window_size[[.y$category]] %>% as.numeric()
  print(temp_window_size)
  
  check_first_frame = NULL
  
  if(step> temp_window_size){
     
    if( step %% temp_window_size == 0){
      print("Good job specifying the right step size") 
      cat ("window_size:",temp_window_size)
    }
    else{
      stop("The step size is relatively large for the window size")
    }
  }
  else{
  if( temp_window_size %% step == 0){
  print("Validating if the window_size is a multiple of the step size")
  if (step < temp_window_size){
    print("step is lesser than I thought")
    #times = temp_window_size/step
    temp_window_size = step
  }
  else{
    stop("Provide a valid step size for the computation of window based summary statistics.")
  }
  cat ("window_size:",temp_window_size)
  }
}
    
check_first_frame = .x %>% 
    mutate_at(vars(time), as.numeric) %>% 
    mutate(time = floor(time/temp_window_size)*temp_window_size) %>% 
    group_by(encounter_id,time,variable,max_time) %>% 
    summarise_each({{op}}, (value)) %>% 
    ungroup() %>%  
    group_by(encounter_id,time) %>%
    gather(key = "key",
           value = "value",
           min:slope) %>% 
    mutate(key = case_when( key == "n_distinct" ~ "n", T ~ key)) %>%
    ungroup() %>% 
    mutate(value = case_when( value %in% c(NaN,-Inf,Inf) ~ NA_real_, T ~ value) ) %>%
    mutate(time = as.numeric(as.character(time))) %>%
    mutate(time = time - min(time[which(time >= 0)])) %>% 
    mutate( time = factor( time , levels = seq( min(time,na.rm = T),max(time,na.rm = T), temp_window_size))) %>% 
    complete(encounter_id,variable,key,time) %>% 
    group_by(encounter_id) %>%
    mutate(max_time = zoo::na.locf(max_time,na.rm = F)) %>% 
    mutate(time = as.numeric(as.character(time))) %>% 
    #mutate(max_time = max(time[which(!is.na(value))], na.rm = T)) %>% 
    filter(time <= max_time) %>% 
    ungroup() %>% 
    mutate(lag = min(time),category = .y$category) %>% 
    select(-max_time)

print("I ran")
  
  
  if ( step <= temp_window_size){
  check_first_frame = bind_rows(
    check_first_frame,
    step_lag(temporal_data = .x, step = step, window_size = as.numeric(window_size[[.y$category]]),category = .y$category)
  )
}
  else{
    # check_first_frame  = check_first_frame %>%
    #     filter( time %% step == 0) %>% 
      
    check_first_frame
  }
}



# This section computes the first lag feature if the step is smaller than the window size. if bigger is not an issue.


step_lag =  function ( temporal_data, step,window_size,category){

  print(category)
  

dummy_frame = NULL
for ( i in seq(0, max(temporal_data$time, na.rm = T),step))
{
 dummy_frame = bind_rows( dummy_frame,
  temporal_data %>% 
  mutate (time = floor(time / step)*step) %>% 
  #mutate(check = time-6) %>% 
  filter(time < i & time>= i-window_size) %>% 
  group_by(encounter_id,variable,max_time) %>%
  summarise_each(funs(min,max,mean),(value))%>% 
  ungroup() %>% 
  as_data_frame() %>% 
    mutate(time = i)#%>% 
    #mutate(time = i)
)
 print(i)
} 

#dummy_frame %>% View()
dummy_frame = dummy_frame %>%  
  group_by(encounter_id,time,max_time) %>%
  gather(key = "key",
         value = "value",
         min:mean) %>% 
  mutate(key = case_when( key == "n_distinct" ~ "n", T ~ key)) %>%
  ungroup() %>%
  mutate(value = case_when( value %in% c(NaN,-Inf,Inf) ~ NA_real_, T ~ value) ) %>%
  mutate(time = as.numeric(as.character(time))) %>%
  #mutate(time = time - min(time[which(time >= 0)])) %>% 
  mutate( time = factor( time , levels = seq( min(time,na.rm = T),max(time,na.rm = T), step))) %>% 
  complete(encounter_id,variable,key,time) %>% 
  group_by(encounter_id) %>%
  mutate(time = as.numeric(as.character(time))) %>% 
  #mutate(max_time = max(time[which(!is.na(value))], na.rm = T)) %>% 
  #filter(time <= max_time) %>% 
  ungroup() %>% 
  mutate(lag = window_size,category = category) %>% 
  select(-max_time)



time_valid = temporal_data %>% 
  filter(category == category) %>% 
  group_by(encounter_id) %>% 
  #mutate(max_time= max(time,na.rm = T)) %>% 
  select(encounter_id,max_time) %>% 
  distinct(encounter_id,max_time)



print("Did I come here")
dummy_frame = dummy_frame %>% 
  left_join(time_valid,
            by = "encounter_id"
              ) %>% 
  filter(time <= max_time) %>% 
  select(-max_time)

dummy_frame

}
