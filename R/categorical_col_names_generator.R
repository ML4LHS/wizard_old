
#' Output file column names generator
#'
#' The colnames_generator function is a  function to identify categorical features and combine them with their values and provide a true value for their presence in datafile.
#'
#' @param temporal_data Data frame
#' @param obj Wizard_object
#' @return A matrix of the infile
#' @export

categorical_col_names_generator = function(temporal_data, obj){
  unique_column = list()
  get_class_data = temporal_data %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(value_df = paste(value, sep='\n',collapse='\n')) %>% 
    dplyr::mutate(value_df = paste(variable, value_df, sep = '\n')) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(var_type = data.table::fread(value_df, data.table = FALSE) %>%
                    dplyr::pull(1) %>%
                    class) %>%
    dplyr::select(-value_df)
  
  # one-hot code categoricals
  
  char_vars = get_class_data %>%
    dplyr::filter(var_type == 'character') %>%
    pull(variable)
  
  temp_data = temporal_data %>%
    dplyr::mutate(new_variable = if_else(variable %in% char_vars, paste(variable, value, sep= '_'), variable)) %>%   mutate(value = if_else(variable %in% char_vars, 1, as.numeric(value))) %>%
    dplyr::mutate(variable = new_variable) %>%
    select(-new_variable) #%>%
  # distinct(variable,category)
  
  
  temp_data = temp_data %>% as_data_frame()
  obj$categorical_columns = disk.frame::add_chunk(obj$categorical_columns,temp_data)
  rm(temp_data)
}





