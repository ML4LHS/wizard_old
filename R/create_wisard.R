create_wisard =  function(
  file = "",
  fixed_data = NULL,
  window_size = list("meds" = 6,"labs" = 6),
  lookback = list("meds" = 6, labs = 48),
  lookahead = 30,
  step = 3,
  feature_stat = list(labs = c('min', 'mean', 'max'),
                      meds = ('min')),
  lag_display = list("meds" = T, "labs" = T),
  recent_only = F,
  lag_comp = list("meds" = "both", "labs" = "prop"),
  outcome_var = NULL,
  outcome_stat = list("mean","min",",max"),
  impute = T,
  write_file = NULL
){
  # The options enables parallel processing of the files.
  options(future.globals.maxSize = Inf)
  
  # Checking for a valida file location if in case of any issues it throws a bug.
  
  if (file == ""){
    stop(" Enter a valid file location")
  }
  else{
    print("Reading in the file from the file system (file location)")
    dev_data =  disk.frame::csv_to_disk.frame(infile = file,
                                              outdir = "Z://adharsh_hrqol_brfss/chunk_data",
                                              shardby = "encounter_id",
                                              backend = "data.table")
    
  }
  
  
  # Function call to invoke the categorical column name generator
  
  # final_data = disk.frame(file.path(tempdir(), "tmp_add_chunk2"))
  # 
  # final_data = add_chunk(final_data,collect(map(dev_data, ~ categorical_col_names_generator(temporal_data = .)
  #                                                                                   ),parallel = F))
  # 
  
  final_data = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(dev_data, ~ categorical_col_names_generator(temporal_data = .)
  ),parallel = FALSE),
  outdir = "tmp1",
  overwrite = T,
  shardby = "encounter_id",
  backend = "data.table")
  
  print("Finished generating categorical names")
  disk.frame::delete(dev_data)
  
  # Function to convert the period object to number to convert the time in period object to seconds or type of step function. 
  
  if(length(lapply(lookback,function(x){class(x)}) %>% unique) == 1 & length(lapply(window_size,function(x){class(x)}) %>% unique) == 1){
    
    if (tolower(lapply(lookback,function(x){class(x)}) %>% unique) == "period" & tolower(class(lookahead)[1]) == "period" & tolower(lapply(window_size,function(x){class(x)}) %>% unique) == "period"){
      
      if(tolower(class(collect(sample_frac(final_data,0.01))$time)) %in% c("character","period")){
        
        period_measure = wisard::hour_to_number(lookback = lookback,window_size = window_size,lookahead = lookahead, step = step)
        lookback = period_measure$lookback
        lookahead = period_measure$lookahead
        window_size = period_measure$window_size
        step = period_measure$step
        cat("step:",step)
        time_fix = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(final_data, ~ wisard::date_to_time(temporal_data = .,fixed_data = fixed_data,units = period_measure$units))) ,
                                             overwrite = T,
                                             shardby = "encounter_id",
                                             backend = "data.table")
        disk.frame::delete(final_data)
        final_data = time_fix
      }
      
      else{
        stop( "Lookback,lookahead and window_size should object of same data type as time variable in temporal data frame")
      }
    }
    if (tolower(lapply(lookback,function(x){class(x)}) %>% unique) == "numeric" & tolower(class(lookahead)[1]) == "numeric" & tolower(lapply(lookback,function(x){class(x)}) %>% unique) == "numeric"){
      print("Second if condition worked this time")
      if(tolower(class(dplyr::collect(disk.frame::sample_frac(final_data,0.01))$time)) %in% c("character","period")){
        stop("Lookback,lookahead and window_size should object of same data type as time variable in temporal data frame")
      }
    }
  }
  else{
    stop(" The data type of the categories in the lookback and window size objects must be the same.")
  }
  
  ## End of the call
  print("Finished converting timestamps as numeric time variables")
  
  
  # Function to identify all the unique variables in the dataframe as initial values for column name generation.
  
  
  column_names_df = data.frame(variable = character(),
                               category = character())
  
  print(final_data %>% class())
  
  column_names_df = dplyr::bind_rows( column_names_df,
                                      map(final_data, ~wisard::unique_variables(.), lazy = F) %>% head()
  )
  print(" Generating the unique names")
  column_names_df = column_names_df %>%
    dplyr::distinct(variable, category) %>%
    dplyr::mutate(variable = stringr::str_sort(variable, numeric = T))
  
  
  
  # Calling in the column name generator function to create all the column names.
  
  
  final_columns = wisard::colnames_generator(temporal_data = column_names_df
                                     ,feature_stat = feature_stat,lookback = lookback,
                                     window_size = window_size)
  
  all_variable_to_create = final_columns[[1]]
  
  print("Column names extensions created")
  
  if(lookback > 0){
    lag_variable_to_create = final_columns[[2]]
  }
  
  
  
  print("Calling in the main function")
  # if(is.null(write_file)){
  #    result_data = disk.frame::as.disk.frame(collect(disk.frame::map(final_data, ~ dummy_wisard(temporal_data = .,
  #                                                                                                      all_variable_to_create = all_variable_to_create,
  #                                                                                                      lag_variable_to_create = lag_variable_to_create,
  #                                                                                                      window_size = window_size,
  #                                                                                                      feature_stat = feature_stat,
  #                                                                                                      outcome_var = outcome_var ,
  #                                                                                                      outcome_stat = outcome_stat,
  #                                                                                                      lookback = lookback,
  #                                                                                                      lookahead = lookahead,
  #                                                                                                      step = step,
  #                                                                                                      lag_display = lag_display,
  #                                                                                                      lag_comp = lag_comp,
  #                                                                                                      recent_only = recent_only,
  #                                                                                                      impute = impute))))
  # 
  #   result_data = result_data %>%
  #     dplyr::arrange(encounter_id, time)
  # 
  #   if(!is.null(fixed_data)){
  #     if (class(fixed_data) == "character"){
  #       fixed_data = disk.frame::csv_to_disk.frame(infile = fixed_data,
  #                                                  outdir = "tmp",
  #                                                  shardby = "encounter_id",
  #                                                  backend = "data.table")
  #       result_data = inner_join.disk.frame(fixed_data,result_data,
  #                                           by = "encounter_id",merge_by_chunk_id = FALSE)
  #       return(result_data)
  #     }
  #     if (class(fixed_data) %in% c("data.frame","tibble","data.table","disk.frame")){
  #       fixed_data = as.disk.frame(fixed_data)
  #       result_data = fixed_data %>%
  #         inner_join.disk.frame(result_data,by = "encounter_id",merge_by_chunk_id = F)
  #       return(result_data)
  #     }
  #   }
  #   else{
  # 
  #     return (result_data)
  #   }
  # }
  # else
  # {
  #result_data = disk.frame::disk.frame(write_file)
  result_data = disk.frame::as.disk.frame(collect(disk.frame::map(final_data, ~ dummy_wisard(temporal_data = .,
                                                                                             all_variable_to_create = all_variable_to_create,
                                                                                             lag_variable_to_create = lag_variable_to_create,
                                                                                             window_size = window_size,
                                                                                             feature_stat = feature_stat,
                                                                                             outcome_var = outcome_var ,
                                                                                             outcome_stat = outcome_stat,
                                                                                             lookback = lookback,
                                                                                             lookahead = lookahead,
                                                                                             step = step,
                                                                                             lag_display = lag_display,
                                                                                             lag_comp = lag_comp,
                                                                                             recent_only = recent_only,
                                                                                             impute = impute))))
  
  result_data = result_data %>%
    dplyr::arrange(encounter_id, time)
  
  # fwrite(result_data, write_file)
  if(!is.null(fixed_data)){
    if (class(fixed_data)[1] == "character"){
      
      fixed_data = disk.frame::csv_to_disk.frame(infile = fixed_data,
                                                 outdir = "tmp",
                                                 shardby = "encounter_id",
                                                 backend = "data.table")
      result_data = fixed_data %>%
        inner_join.disk.frame(result_data, by = "encounter_id",merge_by_chunk_id = F)
      if(!is.null(write_file)){
       disk.frame::write_disk.frame(result_data,outdir = write_file)
        disk.frame::delete(result_data)
      }
      else{
        return(result_data)
      }
      # return(result_data)
      #data.table::fwrite(result_data, write_file)
    }
    if (class(fixed_data)[1] %in% c("data.frame","tibble","data.table","disk.frame")){
      fixed_data = disk.frame::as.disk.frame(fixed_data)
      
      result_data = fixed_data %>%
        inner_join.disk.frame(result_data,
                                          by = "encounter_id", merge_by_chunk_id = F)
      if(!is.null(write_file)){
        disk.frame::write_disk.frame(result_data,outdir = write_file)
        delete(result_data)
      }
      else{
        return(result_data)
      }
    }
  }
  else{
    print("Final condition where fixed data is not present")
    
    if(!is.null(write_file)){
      disk.frame::write_disk.frame(result_data,outdir = write_file)
      disk.frame::delete(result_data)
    }
    else{
      return(result_data)
    }
    #return (result_data)
    #data.table::fwrite(result_data, write_file)
  }
  #}
  
  
  
}