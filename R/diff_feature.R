#' Difference features
#'
#' The function to compute the difference features.
#'
#' @param final_frame final df
#' @param window_size window size
#' @return lag frame
#' @export

diff_feature = function(.x,.y,window_size, lag_compute){



  
  lag_frame = .x %>%
    dplyr::group_by(encounter_id,variable,key) %>%
    # arrange(encounter_id,variable) %>%
    dplyr::mutate(lead_value = data.table::shift(value,n =1,type = "lead") ) %>%
    dplyr::mutate(lag_compute = case_when(lag_compute == "both" ~ "prop/diff", T~ lag_compute)) %>% 
    separate_rows(lag_compute,convert = T, sep = "/") %>% 
    dplyr::mutate(new_value = ifelse(lag_compute == "prop",lead_value / value, lead_value - value)) %>% 
    #ungroup() %>%
    dplyr::mutate(lag_time = str_extract_all(variable, "\\d+")) %>%
    tidyr::unnest() %>%
    dplyr::mutate_at(vars(lag_time), as.numeric) %>%
    #dplyr::mutate(lag_time = lag_time - min(lag_time, na.rm = T)) %>%
    dplyr::filter(lag >= 0) %>%
    dplyr::mutate(lag_time = floor(lag_time/window_size)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(variable = stringr::str_replace_all(variable, "\\d+",paste(lag_compute,lag_time,sep ="_"))) %>%
   # dplyr::group_by(encounter_id,variable) %>% 
    #dplyr::mutate(new_value = data.table::shift(new_value, n = 1,type = "lag")) %>% 
   # dplyr::mutate(new_value = coalesce(value,new_value)) %>% 
    #dplyr::ungroup() %>% 
    dplyr::select(-lead_value,-lag_time,-value,-lag_compute) %>% 
    dplyr::rename(value = new_value) %>% 
    dplyr::mutate(category = .y$category)

lag_frame %>% View()
  
 lag_frame

 
}



