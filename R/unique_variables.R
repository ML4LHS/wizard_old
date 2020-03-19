#' Unique variables
#'
#' Function to generate the column names to write to the file system.
#'
#' @param data data
#' @return list of variables
#' @export

unique_variables =  function (data){
  print("Generating the unique variables for the analysis.")

  data %>%
    dplyr::group_by(variable) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(variable,category)%>% 
    mutate_at(vars(variable,category),as.character)

}
