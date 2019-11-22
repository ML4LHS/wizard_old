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


  if (file == ""){
    stop(" Enter a valid file location")
  }
  else{

    dev_data =  disk.frame::csv_to_disk.frame(infile = file,
                                  #outdir = "tmp",
                                  shardby = "encounter_id",
                                  backend = "data.table")
  }

  # Function call to invoke the categorical column name generator
  final_data = disk.frame::cas.disk.frame(map(dev_data, ~ wisard::categorical_col_names_generator(temporal_data = .),
                                              lazy = F) %>%
                                            dplyr::bind_rows(),
                                          outdir = "tmp",
                                          overwrite = T,
                                          shardby = "encounter_id",
                             backend = "data.table")
  print("Finished generating categorical names")
  disk.frame::delete(dev_data)


  # Function to convert the period object to number

  if (tolower(class(lookback)[1]) == "period" & tolower(class(lookahead)[1]) == "period" & tolower(class(window_size)[1]) == "period"){

    period_measure = wisard::hour_to_number(lookback = lookback,window_size = window_size,lookahead = lookahead)
    lookback = period_measure$lookback
    lookahead = period_measure$lookahead
    window_size = period_measure$window_size

    va = disk.frame::as.disk.frame(map(final_data, ~ date_to_time(temporal_data = .,fixed_data = fixed_data,units = period_measure$units),lazy = F) %>% dplyr::bind_rows(),overwrite = T, shardby = "encounter_id",backend = "data.table")
    disk.frame::delete(final_data)
    final_data = va
  }

  ## End of the call
  print("Finished converting timestamps as numeric time variables")


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


  final_columns = wisard::colnames_generator(temporal_data = column_names_df,feature_stat = feature_stat,lookback = lookback,
                                     window_size = window_size)

  all_variable_to_create = final_columns[[1]]

  print("Column names extensions created")

  if(lookback > 0){
    wisard::lag_variable_to_create = final_columns[[2]]
  }

  print("Calling in the main function")
  if(is.null(write_file)){

    result_data = map(final_data, ~ wisard::dummy_wisard(temporal_data = .,
                                                 all_variable_to_create = all_variable_to_create,
                                                 lag_variable_to_create = lag_variable_to_create,
                                                 window_size = window_size,
                                                 lookback = lookback,
                                                 lookahead = lookahead,
                                                 lag = lag,
                                                 outcome_var = outcome_var ,
                                                 impute = impute), lazy = F) %>%
      dplyr::bind_rows()

    result_data = result_data %>%
      dplyr::arrange(encounter_id, time)

    if(!is.null(fixed_data)){
      if (class(fixed_data) == "character"){
        fixed_data = disk.frame::csv_to_disk.frame(infile = fixed_data,
                                       outdir = "tmp",
                                       shardby = "encounter_id",
                                       backend = "data.table")
        result_data = fixed_data %>%
          dplyr::inner_join(result_data,
                     by = "encounter_id")
        return(result_data)
      }
      if (class(fixed_data) %in% c("data.frame","tibble","data.table","disk.frame")){
        result_data = fixed_data %>%
          dplyr::inner_join(result_data,
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
    result_data = map(final_data, ~ wisard::dummy_wisard(temporal_data = .,
                                                 all_variable_to_create = all_variable_to_create,
                                                 lag_variable_to_create = lag_variable_to_create,
                                                 window_size = window_size,
                                                 lookback = lookback,
                                                 lookahead = lookahead,
                                                 lag = lag,
                                                 outcome_var = outcome_var ,
                                                 impute = impute), lazy = F) %>%
      dplyr::bind_rows()

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
        data.table::fwrite(result_data, write_file)
      }
      if (class(fixed_data) %in% c("data.frame","tibble","data.table","disk.frame")){
        result_data = fixed_data %>%
          dplyr::inner_join(result_data,
                     by = "encounter_id")
        # return(result_data)
        data.table::fwrite(result_data, write_file)
      }
    }
    else{

      #return (result_data)
      data.table::fwrite(result_data, write_file)
    }
  }



}
