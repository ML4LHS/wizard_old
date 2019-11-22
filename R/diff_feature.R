#' Difference features
#'
#' The function to compute the difference features.
#'
#' @param final_frame final df
#' @param window_size window size
#' @return lag frame
#' @export

diff_feature = function(final_frame,window_size){

  lag_frame = final_frame %>%
    dplyr::group_by(encounter_id,variable) %>%
    # arrange(encounter_id,variable) %>%
    dplyr::mutate(lead_value = shift(value,n =1,type = "lead") ) %>%
    dplyr::mutate(value = lead_value - value) %>%
    #ungroup() %>%
    dplyr::mutate(lag_time = str_extract_all(variable, "\\d+")) %>%
    tidyr::unnest() %>%
    dplyr::mutate_at(vars(lag_time), as.numeric) %>%
    dplyr::mutate(lag_time = lag_time - min(lag_time, na.rm = T)) %>%
    dplyr::filter(lag_time > 0) %>%
    dplyr::mutate(lag_time = floor(lag_time/window_size)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(variable = stringr::str_replace_all(variable, "\\d+",paste("lag",lag_time,sep ="_"))) %>%
    dplyr::select(-lead_value,-lag_time)


  return(lag_frame)
}
