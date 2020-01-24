## Creating a S3 object variable with only 3 variables.


new_wizard = function(temporal_data = NA,
                      fixed_data = NA){
  ## Checking if the temporal data is a data.frame,data.table or a disk frame
  stopifnot(is_disk.frame(temporal_data) || is.data.table(temporal_data) || is.data.frame(temporal_data))
  
  ## If temporal data is of accepted type build a S3 object called wizard.
  
  structure(list(
    temporal_data = temporal_data,
    fixed_data = fixed_data,
    wizard_frame = NA,
    lookback = NA,
    window_size = NA,
    lookahead = NA,
    step = NA,
    column_df = NA,
    lag_display = list(),
    lag_compute = list(),
    feature_stat = NA,
    all_variables_to_create = NA,
    lag_variables_to_create = NA,
    lagged_predictors = FALSE,
    prop_predictors = NA,
    diff_predictors = NA,
    outcome_var = NA,
    outcome_stat = NA
    ),class = "wizard"
  )
}

## The first function to create the wizard object for the analysis.

build_wizard_object = function( temporal_data ,
                                fixed_data){
  
  
  if (temporal_data == ""){
    stop(" Enter a valid file location")
  }
  else if (class(temporal_data)[1] == "character"){
    cat("Reading in the file from the file system:",temporal_data)
    dev_data =  disk.frame::csv_to_disk.frame(infile = temporal_data,
                                              outdir = "Z://adharsh_hrqol_brfss/chunk_data",
                                              shardby = "encounter_id",
                                              backend = "data.table")
    
  }
  
  obj = new_wizard(temporal_data = temporal_data,
                   fixed_data = fixed_data)
  
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

## The add_lagged_predictors function definition takes the wizard object as a parameter and the lookback and lookahead.

add_lagged_predictors = function ( obj ,
                                   lookback,
                                   window_size,
                                   step,
                                   lookahead,
                                   recent_only,
                                   feature_stat,
                                   impute){
  obj$feature_stat = feature_stat
  obj$temporal_data = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(obj$temporal_data, ~ categorical_col_names_generator(temporal_data = .)
  ),parallel = FALSE),
  outdir = "tmp1",
  overwrite = T,
  shardby = "encounter_id",
  backend = "data.table")
  
  print("Finished generating categorical names")
  
  if(length(lapply(lookback,function(x){class(x)}) %>% unique) == 1 & length(lapply(window_size,function(x){class(x)}) %>% unique) == 1){
    
    if (tolower(lapply(lookback,function(x){class(x)}) %>% unique) == "period" & tolower(class(lookahead)[1]) == "period" & tolower(lapply(window_size,function(x){class(x)}) %>% unique) == "period"){
      
      if(tolower(class(collect(sample_frac(obj$temporal_data,0.01))$time)) %in% c("character","period")){
        
        period_measure = hour_to_number(lookback = lookback,window_size = window_size,lookahead = lookahead, step = step)
        obj$lookback = period_measure$lookback
        obj$lookahead = period_measure$lookahead
        obj$window_size = period_measure$window_size
        obj$step = period_measure$step
        cat("step:",obj$step)
        
        obj$temporal_data = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(obj$temporal_data, ~ date_to_time(temporal_data = .,fixed_data = fixed_data,units = period_measure$units))) ,
                                                      overwrite = T,
                                                      shardby = "encounter_id",
                                                      backend = "data.table")
        # time_fix = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(obj$temporal_data, ~ date_to_time(temporal_data = .,fixed_data = fixed_data,units = period_measure$units))) ,
        #                                      overwrite = T,
        #                                      shardby = "encounter_id",
        #                                      backend = "data.table")
        # disk.frame::delete(final_data)
        # final_data = time_fix
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
                                      map(obj$temporal_data, ~unique_variables(.), lazy = F) %>% head()
  )
  print(" Generating the unique names")
  column_names_df = column_names_df %>%
    dplyr::distinct(variable, category) %>%
    dplyr::mutate(variable = stringr::str_sort(variable, numeric = T))
  
  obj$column_names_df = column_names_df
  
  
 
  obj$wizard_frame =  disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(obj$temporal_data, ~ lagged_feature(temporal_data = .,
                                                                                                                   window_size = obj$window_size,
                                                                                                                      lookback = obj$lookback,
                                                                                                                      feature_stat = feature_stat,
                                                                                                                      step = obj$step,
                                                                                                                      impute = impute))) ,
                                                                   overwrite = T,
                                                                   shardby = "encounter_id",
                                                                   backend = "data.table")
  

  
  print("Finished Generating lagged features")
  
  obj
  
  
}



## Add proportional predictors takes in categories and 


add_prop_predictors = function(obj,categories = list()){
  
  obj$prop_predictors = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(obj$wizard_frame, ~ iterative_lag_features(final_frame = ., 
                                                                                                                            categories = categories,
                                                                                                                          window_size = obj$window_size, 
                                                                                                                          lag_compute = "prop"
                                                                                                                    ))) ,
                                                  overwrite = T,
                                                  shardby = "encounter_id",
                                                  backend = "data.table")
  
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


add_diff_predictors = function(obj,categories = list()){
  
  obj$diff_predictors = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(obj$wizard_frame, ~ iterative_lag_features(final_frame = ., 
                                                                                                                            categories = categories,
                                                                                                                            window_size = obj$window_size, 
                                                                                                                            lag_compute = "diff"
  ))) ,
  overwrite = T,
  shardby = "encounter_id",
  backend = "data.table")
  
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

add_outcome = function(obj, outcome_var,outcome_stat = list()){
  
  obj$outcome_var = outcome_var
  obj$outcome_stat = outcome_stat
  
  obj$outcome_table = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(obj$temporal_data, ~ dummy_outcome_variable(temporal_data = ., 
                                                                                                                          outcome_var = outcome_var,
                                                                                                                          window_size = obj$step, 
                                                                                                                          outcome_stat = outcome_stat,
                                                                                                                          lookahead = obj$lookahead
  ))) ,
  overwrite = TRUE,
  shardby = "encounter_id",
  backend = "data.table")
  
 
  
  obj
}


write_to = function(obj,write_file = NULL,most_recent){
  
  
  
  obj$lag_compute =obj$lag_compute[!is.na(wizard_object$lag_compute)]
  
  
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
  
  #print(class(obj$wizard_frame))
  
  obj$wizard_frame = disk.frame::as.disk.frame(dplyr::collect(disk.frame::map(obj$wizard_frame, ~ final_spread_data(temporal_data = .,
                                                                                                                           outcome_var = obj$outcome_var,
                                                                                                                           all_variables_to_create = all_variables_to_create

   ))),
  overwrite = TRUE,
  nchunks = 2,
  shardby = "encounter_id",
  backend = "data.table"
  )

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



## To validate the function Loading the temporal dataframe and creating a wizard object.


## To validate the function Loading the temporal dataframe and creating a wizard object.

 # temporal_data = fread("Z://va_aki_project/datasets/temporal_data.csv") %>%
 #   as_tibble()
 # 
 # fixed_data = fread("Z://va_aki_project/datasets/fixed_data.csv") %>%
 #   as_tibble()
 # 
 # wizard_object = build_wizard_object(temporal_data = "Z://va_aki_project/datasets/temporal_data.csv",
 #                            fixed_data = fixed_data) %>%
 #   add_lagged_predictors(obj = .,
 #                       window_size = list("meds" = hours(6),"labs" = hours(6)),
 #                       lookback = list("meds" = hours(6), labs = hours(48)),
 #                       lookahead = hours(30),
 #                       step = hours(3),
 #                       feature_stat = list(labs = c('min', 'mean', 'max'),
 #                       meds = ('min')),
 #                       impute = F) %>%
 #                   add_prop_predictors(categories = list("labs")) %>%
 #                   add_diff_predictors(categories = list("labs")) %>%
 #   add_outcome(outcome_var = "SBP",outcome_stat = list("mean")) %>%
 #   write_to(obj = .,write_file = NULL,most_recent = F)
 # 
 # 
 # 
 # wizard_object$wizard_frame %>%
 #   collect() %>%
 #   arrange(encounter_id,time.y) %>%
 #   View()
 # 
 # 
 # 
 # 

 
# ## Checking the nchunks
# 
# check_data = as.disk.frame(fixed_data, nchunks = nchunk(wizard_object$wizard_frame))
# inner_join(check_data,wizard_object$wizard_frame,by = "encounter_id",merge_by_chunk_id = F)

