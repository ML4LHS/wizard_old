
#' Output file column names generator
#'
#' The colnames_generator function is a  function to identify categorical features and combine them with their values and provide a true value for their presence in datafile.
#'
#' @param temporal_data Data frame
#' @return A matrix of the infile
#' @export

categorical_col_names_generator = function(temporal_data){
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
}
