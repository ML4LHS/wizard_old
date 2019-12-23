#' Create wisard
#'
#' @param file temporal data file
#' @param fixed_data fixed data file
#' @param window_size size of window: numeric when time var is numeric or period (e.g. hour(6)) when time is date object
#' @param lookback lookback period
#' @param lookahead lookahead period
#' @param feature_stat feature stats
#' @param outcome_var outcome variable
#' @param outcome_stat outcome stats
#' @param lag whether to include lag features
#' @param impute whether to impute data
#' @param write_file path to csv output
#' @export

create_wisard =  function(
  file = "",
  fixed_data = NULL,
  window_size = 6,
  lookback = 48,
  lookahead = 30,
  feature_stat = list(labs = c('min', 'mean', 'max','n'),
                      meds = ('n')#, Drug = c('n')
  ),
  outcome_var = NULL,
  outcome_stat = list("mean","min",",max"),
  lag = FALSE,
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
  
  #   if (tolower(class(lookback)[1]) == "period" & tolower(class(lookahead)[1]) == "period" & tolower(class(window_size)[1]) == "period"){
  #     
  #     if(tolower(class(collect(sample_frac(final_data,0.01))$time)) %in% c("character","period")){
  # 
  #     period_measure = wisard::hour_to_number(lookback = lookback,window_size = window_size,lookahead = lookahead)
  #     lookback = period_measure$lookback
  #     lookahead = period_measure$lookahead
  #     window_size = period_measure$window_size
  # 
  #     time_fix = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(final_data, ~ date_to_time(temporal_data = .,fixed_data = fixed_data,units = period_measure$units))) ,
  #                                    overwrite = T,
  #                                    shardby = "encounter_id",
  #                                    backend = "data.table")
  #     disk.frame::delete(final_data)
  #     final_data = time_fix
  #   }
  #   
  #   else{
  #     stop( "Lookback,lookahead and window_size should object of same data type as time variable in temporal data frame")
  #   }
  # }
  #   if (tolower(class(lookback)[1]) == "numeric" & tolower(class(lookahead)[1]) == "numeric" & tolower(class(window_size)[1]) == "numeric"){
  #    print("Second if condition worked this time") 
  #     if(tolower(class(collect(sample_frac(final_data,0.01))$time)) %in% c("character","period")){
  #       stop("Lookback,lookahead and window_size should object of same data type as time variable in temporal data frame")
  #     }
  #   }
  #   ## End of the call
  #   print("Finished converting timestamps as numeric time variables")
  
  
  # Function to identify all the unique variables in the dataframe as initial values for column name generation.
  
  
  column_names_df = data.frame(variable = character(),
                               category = character())
  
  print(final_data %>% class())
  
  column_names_df = dplyr::bind_rows( column_names_df,
                                      map(final_data, ~unique_variables(.), lazy = F) %>% head()
  )
  print(" Generating the unique names")
  column_names_df = column_names_df %>%
    dplyr::distinct(variable, category) %>%
    dplyr::mutate(variable = stringr::str_sort(variable, numeric = T))
  
  
  
  # Calling in the column name generator function to create all the column names.
  
  
  final_columns = colnames_generator(temporal_data = column_names_df)
  # ,feature_stat = feature_stat,lookback = lookback,
  #                                             window_size = window_size)
  
  all_variable_to_create = final_columns[[1]]
  
  print("Column names extensions created")
  
  if(lookback > 0){
    lag_variable_to_create = final_columns[[2]]
  }
  
  
  
  print("Calling in the main function")
  if(is.null(write_file)){
    # df_path = file.path(tempdir(), "tmp_add_chunk")
    # result_data = disk.frame::disk.frame(df_path)
    # disk.frame::add_chunk(result_data,map(final_data, ~ dummy_wisard(temporal_data = .,
    #                                                                          all_variable_to_create = all_variable_to_create,
    #                                                                          lag_variable_to_create = lag_variable_to_create,
    #                                                                          window_size = window_size,
    #                                                                          lookback = lookback,
    #                                                                          lookahead = lookahead,
    #                                                                          lag = lag,
    #                                                                          outcome_var = outcome_var ,
    #                                                                          impute = impute), lazy = F))
    result_data = disk.frame::as.disk.frame(collect(disk.frame::map(final_data, ~ dummy_wisard(temporal_data = .,
                                                                                               all_variable_to_create = all_variable_to_create,
                                                                                               lag_variable_to_create = lag_variable_to_create,
                                                                                               window_size = window_size,
                                                                                               lookback = lookback,
                                                                                               lookahead = lookahead,
                                                                                               lag = lag,
                                                                                               outcome_var = outcome_var ,
                                                                                               impute = impute))))
    
    result_data = result_data %>%
      dplyr::arrange(encounter_id, time)
    
    if(!is.null(fixed_data)){
      if (class(fixed_data) == "character"){
        fixed_data = disk.frame::csv_to_disk.frame(infile = fixed_data,
                                                   outdir = "tmp",
                                                   shardby = "encounter_id",
                                                   backend = "data.table")
        result_data = inner_join(fixed_data,result_data,
                                 by = "encounter_id",merge_by_chunk_id = FALSE)
        return(result_data)
      }
      if (class(fixed_data) %in% c("data.frame","tibble","data.table","disk.frame")){
        fixed_data = as.disk.frame(fixed_data)
        result_data = fixed_data %>%
          disk.frame::inner_join(result_data,
                                 by = "encounter_id")
        return(result_data)
      }
    }
    else{
      
      return (result_data)
    }
  }
  else
  {
    #result_data = disk.frame::disk.frame(write_file)
    result_data = disk.frame::as.disk.frame(collect(disk.frame::map(final_data, ~ wisard::dummy_wisard(temporal_data = .,
                                                                                                       all_variable_to_create = all_variable_to_create,
                                                                                                       lag_variable_to_create = lag_variable_to_create,
                                                                                                       window_size = window_size,
                                                                                                       lookback = lookback,
                                                                                                       lookahead = lookahead,
                                                                                                       lag = lag,
                                                                                                       outcome_var = outcome_var ,
                                                                                                       impute = impute))))
    
    result_data = result_data %>%
      dplyr::arrange(encounter_id, time)
    
    # fwrite(result_data, write_file)
    if(!is.null(fixed_data)){
      if (class(fixed_data) == "character"){
        fixed_data = disk.frame::csv_to_disk.frame(infile = fixed_data,
                                                   outdir = "tmp",
                                                   shardby = "encounter_id",
                                                   backend = "data.table")
        result_data = fixed_data %>%
          dplyr::inner_join(result_data,
                            by = "encounter_id")
        # return(result_data)
        #data.table::fwrite(result_data, write_file)
      }
      if (class(fixed_data) %in% c("data.frame","tibble","data.table","disk.frame")){
        result_data = fixed_data %>%
          dplyr::inner_join(result_data,
                            by = "encounter_id")
        # return(result_data)
        # data.table::fwrite(result_data, write_file)
      }
    }
    else{
      print("Final condition where write file is not present")
      #return (result_data)
      #data.table::fwrite(result_data, write_file)
    }
  }
  
  
  
}

