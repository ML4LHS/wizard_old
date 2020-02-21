#' Difference features
#'
#' The function to compute the difference features.
#'
#' @param final_frame final df
#' @param window_size window size
#' @return lag frame
#' @export

diff_feature = function(.x,.y,window_size, step,lag_compute){
  
  
  
  
  lag_frame = .x %>% 
    group_by(encounter_id,variable) %>% 
    arrange(encounter_id,variable,time) %>% 
    dplyr::mutate(lag_value = data.table::shift(value,n = window_size/step,type = "lag") ) %>% 
    dplyr::mutate_at(vars(lag_value), as.numeric) %>% 
    ungroup() %>% 
    dplyr::mutate(lag_compute = case_when(lag_compute == "both" ~ "prop/diff", T~ lag_compute)) %>%
    separate_rows(lag_compute,convert = T, sep = "/") %>%
    dplyr::mutate(new_value = ifelse(lag_compute == "prop",value / lag_value, value - lag_value)) %>% 
    dplyr::mutate(lag_time = str_extract(variable, stringr::regex("[\\d+]*$"))) %>%
    dplyr::mutate_at(vars(lag_time), as.numeric) %>%
    #dplyr::mutate(lag_time = lag_time - min(lag_time, na.rm = T)) %>%
    dplyr::filter(lag >= 0) %>%
    dplyr::mutate(lag_time = floor(lag_time/window_size)) %>%
    dplyr::mutate(variable = stringr::str_replace_all(variable, "\\d+",paste(lag_compute,lag_time,sep ="_"))) %>%
    dplyr::select(-lag_value,-lag_time,-value,-lag_compute) %>% 
    dplyr::rename(value = new_value) %>% 
    dplyr::mutate(category = .y$category)
  
  
  lag_frame
  
  
}



