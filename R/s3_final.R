

#' new wizard
#' 
#' The function initialises the wizard object.
#' 
#'@export

new_wizard = function(temporal_data = NA,
                      fixed_data = NA){
  ## Checking if the temporal data is a data.frame,data.table or a disk frame
  stopifnot(disk.frame::is_disk.frame(temporal_data) || data.table::is.data.table(temporal_data) || is.data.frame(temporal_data))
  
  ## If temporal data is of accepted type build a S3 object called wizard.
  
  structure(list(
    temporal_data = temporal_data,
    fixed_data = fixed_data,
    wizard_frame = NA,
    lookback = NA,
    window_size = NA,
    lookahead = NA,
    step = NA,
    column_names_df = NA,
    lag_display = list(),
    lag_compute = list(),
    feature_stat = NA,
    lagged_predictors = FALSE,
    prop_predictors = NA,
    diff_predictors = NA,
    outcome_var = NA,
    outcome_stat = NA,
    outcome_table = NA
    ),class = "wizard"
  )
}


#' Build wizard object
#' 
#' Creates and initialized a wizard object with temporal and fixed data frame
#' 
#' @param temporal_data temporal dataframe
#' @param fixed_data fixed data
#' @return wizard object
#' @export


build_wizard_object = function( temporal_data ,
                                fixed_data,
                                nchunk){
  
  
  if (is.na(temporal_data) || temporal_data == ''){
    stop(" Enter a valid file location")
  }
  
  else if (class(temporal_data) == "character"){
    
    cat("Reading in the file from the file system:",temporal_data)
    
    if(is.na(nchunk)){
      
      dev_data =  disk.frame::csv_to_disk.frame(infile = temporal_data,
                                                outdir = file.path(getwd(),"dev_data"),
                                                shardby = "encounter_id",
                                                backend = "data.table")
    }
    else{
      dev_data =  disk.frame::csv_to_disk.frame(infile = temporal_data,
                                                outdir = file.path(getwd(),"dev_data"),
                                                shardby = "encounter_id",
                                                nchunks = nchunk,
                                                backend = "data.table")
    }
    cat("I'm done with loading the data")
    
    obj = new_wizard(temporal_data = dev_data,
                     fixed_data = fixed_data)
  }
  
  else{
    
    obj = new_wizard(temporal_data = temporal_data ,
                     fixed_data = fixed_data)
  }
  
  cat("Loaded the wizard object")
  
  obj
  
}


## Defining the add_lagged_predictors.

## Initially describing the namespace that will only accept the wizard object as a parameter.

add_lagged_predictors = function(x){
  
  UseMethod("add_lagged_predictors",x)
}

add_prop_predictors = function(x){
  UseMethod("add_prop_predictors",x)
}

add_diff_predictors = function(x){
  UseMethod("add_diff_predictors",x)
}

add_outcome = function(x){
  UseMethod("add_outcome",x)
}

#' add_lagged_predictors
#' 
#' The function generates the lagged features for every window
#' 
#' @param obj wizard object
#' @param lookback lookback period for each category
#' @param window_size window_size for each category
#' @param step Number of windows to move forward
#' @param lookahead The outcome period
#' @param feature_stat The summary statistics to compute for each category
#' @param impute T/F to carry forward impute
#' @return wizard object
#' @export

add_lagged_predictors = function ( obj ,
                                   lookback,
                                   window_size,
                                   step,
                                   lookahead,
                                   feature_stat,
                                   impute){
  obj$feature_stat = feature_stat
  if(!dir.exists(file.path(getwd(),"temporal_1"))){
    
    obj$categorical_columns = disk.frame::disk.frame(file.path(getwd(),"temporal_1"))
    
  }
  else{
    unlink(file.path(getwd(),"temporal_1"),force = T,recursive = T)
    obj$categorical_columns = disk.frame::disk.frame(file.path(getwd(),"temporal_1"))
  }
  
  cat_call = disk.frame::cmap(obj$temporal_data, ~ wizard::categorical_col_names_generator(temporal_data = .,obj = obj),lazy = F)
  
  obj$temporal_data = obj$categorical_columns
  obj$categorical_columns = NULL
  print("Finished generating categorical names")
  rm(cat_call)
  
  if(length(lapply(lookback,function(x){class(x)}) %>% unique) == 1 & length(lapply(window_size,function(x){class(x)}) %>% unique) == 1){
    
    if (tolower(lapply(lookback,function(x){class(x)}) %>% unique) == "period" & tolower(class(lookahead)[1]) == "period" & tolower(lapply(window_size,function(x){class(x)}) %>% unique) == "period"){
      
      if(tolower(class(collect(sample_frac(obj$temporal_data,0.01))$time)) %in% c("character","period")){
        
        period_measure = wizard::hour_to_number(lookback = lookback,window_size = window_size,lookahead = lookahead, step = step)
        obj$lookback = period_measure$lookback
        obj$lookahead = period_measure$lookahead
        obj$window_size = period_measure$window_size
        obj$step = period_measure$step
        cat("step:",obj$step)
        
        if(!dir.exists(file.path(getwd(),"date_time"))){
          obj$date_time = disk.frame::disk.frame(file.path(getwd(),"date_time"))
        }
        else{
          unlink(file.path(getwd(),"date_time"),force = T,recursive = T)
          obj$date_time = disk.frame::disk.frame(file.path(getwd(),"date_time"))
        }
        
        date_call = disk.frame::cmap(obj$temporal_data,~wizard::date_to_time(temporal_data = .,fixed_data = obj$fixed_data,units = period_measure$units,obj = obj),lazy = F)
        obj$temporal_data = obj$date_time
        rm(date_call)
        obj$date_time = NULL
       
      }
      
      else{
        stop( "Lookback,lookahead and window_size should object of same data type as time variable in temporal data frame")
      }
    }
    if (tolower(lapply(lookback,function(x){class(x)}) %>% unique) == "numeric" & tolower(class(lookahead)[1]) == "numeric" & tolower(lapply(lookback,function(x){class(x)}) %>% unique) == "numeric"){
      print("Second if condition worked this time")
      if(tolower(class(dplyr::collect(sample_frac(obj$temporal_data,0.01))$time)) %in% c("character","period")){
        stop("Lookback,lookahead and window_size should object of same data type as time variable in temporal data frame")
      }
      else{
        obj$lookback = lookback
        obj$window_size = window_size
        obj$lookahead = lookahead
        obj$step = step
      }
    }
  }
  else{
    stop(" The data type of the categories in the lookback and window size objects must be the same.")
  }
  
  ## End of the call
  print("Finished converting timestamps as numeric time variables")
  
  #obj$wizard_frame = obj$temporal_data
  
  
  # Function to identify all the unique variables in the dataframe as initial values for column name generation.
  
  
  column_names_df = data.frame(variable = character(),
                               category = character())
  
  
  
  column_names_df = dplyr::bind_rows( column_names_df,
                                      disk.frame::cmap(obj$temporal_data, ~wizard::unique_variables(.), lazy = F) %>% head()
  )
  print(" Generating the unique names")
  column_names_df = column_names_df %>%
    dplyr::distinct(variable, category) %>%
    dplyr::mutate(variable = stringr::str_sort(variable, numeric = T))
  
  obj$column_names_df = column_names_df
  
  
 
  if(!dir.exists(file.path(getwd(),"wizard_frame"))){
    obj$wizard_frame = disk.frame::disk.frame(file.path(getwd(),"wizard_frame"))
  }
  else{
    unlink(file.path(getwd(),"wizard_frame"),force = T,recursive = T)
    obj$wizard_frame = disk.frame::disk.frame(file.path(getwd(),"wizard_frame"))
  }
  # obj$wizard_frame =  disk.frame::as.disk.frame(disk.frame::collect(disk.frame::cmap(obj$temporal_data, ~ lagged_feature(temporal_data = .,
  #                                                                                                                  window_size = obj$window_size,
  #                                                                                                                     lookback = obj$lookback,
  #                                                                                                                     feature_stat = feature_stat,
  #                                                                                                                     step = obj$step,
  #                                                                                                                     impute = impute))) ,
  #                                                                  overwrite = T,
  #                                                                  shardby = "encounter_id",
  #                                                                  backend = "data.table")
  # 
  
  dummy = (disk.frame::cmap(obj$temporal_data, ~ wizard::lagged_feature(temporal_data = .,
                                                               obj = obj,
                                                               window_size = obj$window_size,
                                                               lookback = obj$lookback,
                                                               feature_stat = feature_stat,
                                                               step = obj$step,
                                                               impute = impute),lazy = F))
  
  rm(dummy)
  print("Finished Generating lagged features")
  
  obj$lagged_predictors = TRUE
  obj
  
  
}



#'add_prop_predictors
#'
#' Function to generate proportional variation between lagged features
#' 
#' @param obj wizard object
#' @param categories List of categories to generate teh proportional variation feature
#' @return wizard object
#' @export 


add_prop_predictors = function(obj,categories = list()){
  
  if (obj$lagged_predictors == FALSE){
    cat("The lagged features are not found.")
  }
  
  if(!dir.exists(file.path(getwd(),"prop_predictors"))){
    obj$prop_predictors = disk.frame::disk.frame(file.path(getwd(),"prop_predictors"))
  }
  else{
    unlink(file.path(getwd(),"prop_predictors"),force = T, recursive = T)
    obj$prop_predictors = disk.frame::disk.frame(file.path(getwd(),"prop_predictors"))
  }
  
  dummy= (disk.frame::cmap(obj$wizard_frame, ~ wizard::iterative_lag_features(final_frame = .,
                                                                     obj = obj,
                                                                     categories = categories,
                                                                     window_size = obj$window_size, 
                                                                     step = obj$step,
                                                                     lag_compute = "prop"
  ),lazy = F)) 
  
  rm(dummy)
  
  for (i in categories){
    if (i %in% names(obj$lag_compute)){
      obj$lag_compute[[i]] = append(obj$lag_compute[[i]],"prop")
    }
    else{
      obj$lag_compute[[i]] = list("prop")
    }
    obj$lag_display[[i]] = T
  }
  obj
}


#'add_diff_predictors
#'
#' Function to generate iterated difference between lagged features
#' 
#' @param obj wizard object
#' @param categories List of categories to generate the iterated difference feature
#' @return wizard object
#' @export 


add_diff_predictors = function(obj,categories = list()){
  
  if (obj$lagged_predictors == FALSE){
    cat("The lagged features are not found.")
  }
  
  
  if(!dir.exists(file.path(getwd(),"diff_predictors"))){
    obj$diff_predictors = disk.frame::disk.frame(file.path(getwd(),"diff_predictors"))
  }
  else{
    unlink(file.path(getwd(),"diff_predictors"),force = T,recursive = T)
    obj$diff_predictors = disk.frame::disk.frame(file.path(getwd(),"diff_predictors"))
  }
  
  dummy = disk.frame::cmap(obj$wizard_frame, ~ wizard::iterative_lag_features(final_frame = ., 
                                                                      obj = obj,
                                                                      categories = categories,
                                                                      window_size = obj$window_size, 
                                                                      step = obj$step,
                                                                      lag_compute = "diff"
  ),lazy = F) 
  
  rm(dummy)
  
  for (i in categories){
    if (i %in% names(obj$lag_compute)){
      obj$lag_compute[[i]] = append(obj$lag_compute[[i]],"diff")
    }
    else{
      obj$lag_compute[[i]] = list("diff")
    }
    
    obj$lag_display[[i]] = TRUE
    #cat("lag_display",obj$lag_display[[i]])
  }
  
  obj
}


#' add_outcome
#' 
#' The function to generate the outcome stat for the data
#' 
#' @param obj wizard object
#' @param outcome_var outcome variable
#' @param outcome_stat outcome statistics
#' @return wizard object
#' @export


add_outcome = function(obj, outcome_var,outcome_stat = list()){
  
  obj$outcome_var = outcome_var
  obj$outcome_stat = outcome_stat
  
  if(!dir.exists(file.path(getwd(),"outcome_table"))){
    obj$outcome_table = disk.frame::disk.frame(file.path(getwd(),"outcome_table"))
  }
  else{
    unlink(file.path(getwd(),"outcome_table"),force = T, recursive = T)
    obj$outcome_table = disk.frame::disk.frame(file.path(getwd(),"outcome_table"))
  }
  
  # obj$outcome_table = disk.frame::as.disk.frame(dplyr::collect(disk.frame::cmap(obj$temporal_data, ~ dummy_outcome_variable(temporal_data = ., 
  #                                                                                                                         outcome_var = outcome_var,
  #                                                                                                                         window_size = obj$step, 
  #                                                                                                                         outcome_stat = outcome_stat,
  #                                                                                                                         lookahead = obj$lookahead
  # ))) ,
  # overwrite = TRUE,
  # shardby = "encounter_id",
  # backend = "data.table")
  # 
  # 
  
  outcome_call = disk.frame::cmap(obj$temporal_data, ~ wizard::dummy_outcome_variable(temporal_data = ., 
                                                                             outcome_var = outcome_var,
                                                                             window_size = obj$step, 
                                                                             outcome_stat = outcome_stat,
                                                                             lookahead = obj$lookahead,
                                                                             obj = obj
  ),lazy = F)
  rm(outcome_call)
 
  
  obj
}


#' write_to
#' 
#' The function to write or spread the temporal data frame.
#' 
#' @param obj wizard object
#' @param write_file File location to write file
#' @param most_recent To include only the most recent lagged features
#' @return wizard object
#' @import disk.frame
#' @export 


write_to = function(obj,write_file = NULL,most_recent){
  
  if (obj$lagged_predictors == FALSE){
    cat("The lagged features are not found.")
  }
  
  #obj$lag_compute =obj$lag_compute[!is.na(wizard_object$lag_compute)]
  
  
  # Calling in the column name generator function to create all the column names.
  
  
  final_columns = colnames_generator(temporal_data = obj$column_names_df
                                     ,feature_stat = obj$feature_stat,
                                     lookback = obj$lookback,
                                     window_size = obj$window_size,
                                     recent_only = most_recent,
                                     lag_compute = obj$lag_compute,
                                     lag_display = obj$lag_display)
  
  all_variables_to_create = final_columns[[1]]
  
  print("Column names are generated")
  
  
  lag_variables_to_create = final_columns[[2]]
  
  all_variables_to_create = c(all_variables_to_create,lag_variables_to_create)
  if(!is.na(obj$outcome_var)){
  all_variables_to_create = c(all_variables_to_create,obj$outcome_table %>% select(variable) %>%  collect() %>% unique() %>% pull())
  }
  #print(all_variables_to_create)
  
  ## Binding all the disk frame together for the initial analysis
  disk_frame_list = list(obj$wizard_frame,
                         obj$prop_predictors,
                         obj$diff_predictors,
                         obj$outcome_table)
  
  
  ## only retaining the features that were generated.
  
  obj$wizard_frame = disk.frame::rbindlist.disk.frame(df_list = disk_frame_list[!is.na(disk_frame_list)])
  
  
  
  ## calling a function which will spread and merge the data.
  
  if(!dir.exists(file.path(getwd(),"result_frame"))){
    obj$result_frame = disk.frame::disk.frame(file.path(getwd(),"result_frame"))
  }
  else{
    unlink(file.path(getwd(),"result_frame"),force = T,recursive = T)
    obj$result_frame = disk.frame::disk.frame(file.path(getwd(),"result_frame"))
  }
  
  result_call = disk.frame::cmap(obj$wizard_frame, ~ wizard::final_spread_data(temporal_data = .,
                                                                      outcome_var = obj$outcome_var,
                                                                      all_variables_to_create = all_variables_to_create,
                                                                      obj = obj
  ),lazy = F)
  obj$wizard_frame = obj$result_frame
  obj$result_frame = NULL
  rm(result_call)

  if(!is.null(obj$fixed_data)){
    if (class(obj$fixed_data)[1] == "character"){
      
      obj$fixed_data = disk.frame::csv_to_disk.frame(infile = obj$fixed_data,
                                                     outdir = "tmp",
                                                     shardby = "encounter_id",
                                                     backend = "data.table",
                                                     nchunks = nchunk(obj$wizard_frame))
      obj$wizard_frame = obj$fixed_data %>%
        inner_join(obj$wizard_frame,by = "encounter_id",merge_by_chunk_id = F)
      if(!is.null(write_file)){
        disk.frame::write_disk.frame(obj$wizard_frame,outdir = write_file)
        disk.frame::delete(obj$wizard_frame)
      }
      
    }
    if (class(obj$fixed_data)[1] %in% c("data.frame","tibble","data.table","disk.frame","tbl_df")){
      
      if(class(obj$fixed_data)[1] != "disk.frame"){
      obj$fixed_data = disk.frame::as.disk.frame(obj$fixed_data, nchunks = nchunk(obj$wizard_frame),shardby = "encounter_id")
      }
      
      obj$wizard_frame = obj$fixed_data %>%
        inner_join(obj$wizard_frame,
                   by = "encounter_id", merge_by_chunk_id = F)
      if(!is.null(write_file)){
        disk.frame::write_disk.frame(obj$wizard_frame,outdir = write_file)
        delete(obj$wizard_frame)
      }
      
    }
  }
  else{
    print("Final condition where fixed data is not present")
    
    if(!is.null(write_file)){
      disk.frame::write_disk.frame(obj$wizard_frame,outdir = write_file)
      disk.frame::delete(obj$wizard_frame)
    }
    else{
      return(obj$wizard_frame)
    }
    #return (obj$wizard_frame)
    #data.table::fwrite(obj$wizard_frame, write_file)
  }
  
  obj
}





