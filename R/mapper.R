
#' Check mapper function
#' @param window_size window size for each category
#' @param step step size
#' @param feature_stat stats to compute for each feature
#' @export


check_mapper = function(.x,.y,window_size = list("meds" = 1,"labs" = 6), step = 3,  feature_stat = list(labs = c('min', 'mean', 'max'),
                                                                                                  meds = ('min'))){
  count = 1000
  op = c(feature_stat[[.y$category]],"wizard::slope(time,value)")
  print(op)
  times = 1
  #print(step)
  temp_window_size = window_size[[.y$category]] %>% as.numeric()
  #print(temp_window_size)
 # cat("The minimum time:",.x %>% min(time,na.rm =T) %>% pull(1))  
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
           -encounter_id:-time) %>% 
    mutate(key = case_when( key == "n_distinct" ~ "n", T ~ key)) %>%
    ungroup() %>% 
    mutate(value = case_when( value %in% c(NaN,-Inf,Inf) ~ NA_real_, T ~ value) ) %>%
    mutate(time = as.numeric(as.character(time))) %>%
    mutate(time = time - min(time[which(time >= 0)])) %>%
    mutate( time = case_when(time %>% unique() %>% length()>1~ factor( time , levels = seq(min(time,na.rm = T),max(time,na.rm = T), temp_window_size)),T~as.factor(time))) %>% 
    complete(encounter_id,variable,key,time) %>% 
    group_by(encounter_id) %>%
    mutate(max_time = zoo::na.locf(max_time,na.rm = F)) %>% 
    mutate(time = as.numeric(as.character(time))) %>% 
    #mutate(max_time = max(time[which(!is.na(value))], na.rm = T)) %>% 
    filter(time <= max_time) %>% 
    ungroup() %>% 
    mutate(lag = min(time),category = .y$category) %>% 
    select(-max_time)


  
  
  if ( step <= temp_window_size & length(.x$time[which(.x$time >= step)]) > 0){
  check_first_frame = bind_rows(
    check_first_frame,
    wizard::step_lag(temporal_data = .x, step = step, window_size = as.numeric(window_size[[.y$category]]),category = .y$category)
  )
}
  else{

      
    check_first_frame
  }
}



# This section computes the first lag feature if the step is smaller than the window size. if bigger is not an issue.
#' Step lag function is a internal function to adjust to the step value.
#' @param  temporal_data temporal data frame
#' @param step step size
#' @param window_size window size
#' @param category category of the feature
#' @export
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
# print(i)
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




dummy_frame = dummy_frame %>% 
  left_join(time_valid,
            by = "encounter_id"
              ) %>% 
  filter(time <= max_time) %>% 
  select(-max_time)

dummy_frame

}




#' lagged feature generator
#' @param lookback
#' @param window_size
#' @param step
#' @export 

lagged_feature_generator = function(.x,.y,
                                    lookback,
                                    window_size,
                                    step){
  
  start = 1
  final_frame = NULL
  temp_window_size = window_size[[.y$category]] %>% as.numeric()
  temp_lookback = lookback[[.y$category]] %>% as.numeric()
  if(step < temp_window_size){
    start = 2
  }
  
  if( temp_lookback > 0 &  (temp_lookback/temp_window_size)-start > 0){
    for ( i in seq(1,(temp_lookback/temp_window_size)-start,1))
    {
      #print(i)
      final_frame = dplyr::bind_rows( final_frame,
                                      .x %>%
                                        filter(lag == max(lag, na.rm = T)) %>% 
                                        dplyr::group_by(encounter_id,variable,key) %>%
                                        dplyr::mutate(new_value = data.table::shift(value,i, type = "lag")) %>%
                                        dplyr::mutate(lag = lag + i*temp_window_size) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::mutate(variable = paste(variable,key,lag, sep = "_")) #%>%
      )
     # print(i)
    }
    print("Merging the final frame")
    final_frame = dplyr::bind_rows(.x %>%
                                     dplyr::mutate(variable = paste(variable,key, lag , sep ="_")) %>%
                                     dplyr::mutate(lag = time) %>% 
                                     dplyr::mutate(category = .y$category),
                                   final_frame %>%
                                     dplyr::select(encounter_id,variable,time,key,-value,value = new_value, lag) %>% 
                                     dplyr::mutate(category = .y$category)
    )
  
  }
  final_frame
}


#' iterative_lag_features
#' @param final_frame data frame of lagged features
#' @param categories categories 
#' @param window_size window size

iterative_lag_features = function(final_frame,categories,window_size,lag_compute){
  
  final_frame = final_frame %>% 
    filter(category %in% categories) %>% 
    group_by(category) %>% 
    group_map(~wizard::diff_feature(.x =.x,.y=.y,window_size = window_size[[.y$category]],lag_compute = lag_compute)) %>% 
    bind_rows()
  final_frame
}
