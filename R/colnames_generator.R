#' Creates features
#'
#' The function is intended to create a expanded list of all the features to be created after transforming the data in way.
#'
#' @param temporal_data Data frame
#' @param feature_stat List of statistical methods to use
#' @param lagged_feature_stat feature stats
#' @param default_stats default stats
#' @param lookback lookback period
#' @param window_size window size
#' @return Variable list
#' @export

colnames_generator = function( temporal_data,
                               feature_stat = list(labs = c('min', 'mean', 'max','n'),
                                                   meds = ('n')#, Drug = c('n')
                               ),
                               lagged_feature_stat = list(labs = c('min', 'mean')),
                               default_stats = c("min","max","mean","n"),
                               lookback = 48,
                               window_size = 6){

  max_lag = lookback %/% window_size
  all_variables_to_create = c()
  lag_variables_to_create = c()

  for (j in temporal_data %>%
       distinct(category) %>%
       pull(1)){

    if (length(feature_stat[[as.name(j)]])>0){
      # print(feature_stat[[as.name(i)]])
      all_variables_to_create = append(all_variables_to_create, expand.grid(temporal_data %>%
                                                                              dplyr::filter(category== j ) %>%
                                                                              dplyr::distinct(variable) %>%
                                                                              dplyr::pull(variable),
                                                                            feature_stat[[as.name(j)]],
                                                                            seq(0, ifelse(lookback >0,lookback-window_size,0), by = window_size)) %>%
                                         dplyr::arrange(Var1, Var2, Var3) %>%
                                         tidyr::unite(variable_names, Var1:Var3) %>%
                                         dplyr::pull(variable_names))
    }
    else{
      all_variables_to_create = append(all_variables_to_create, expand.grid(temporal_data %>%
                                                                                     dplyr::filter(category== j ) %>%
                                                                                     dplyr::distinct(variable) %>%
                                                                                     dplyr::pull(variable),
                                                                            default_stats,
                                                                            seq(0, ifelse(lookback >0,lookback-window_size,0), by = window_size)) %>%
                                                dplyr::arrange(Var1, Var2, Var3) %>%
                                                tidyr::unite(variable_names, Var1:Var3) %>%
                                                dplyr::pull(variable_names))
    }

    if (length(lagged_feature_stat[[as.name(j)]])>0 & lookback > 0){
      # print(feature_stat[[as.name(i)]])
      lag_variables_to_create = append(lag_variables_to_create, expand.grid(temporal_data %>%
                                                                              dplyr::filter(category== j ) %>%
                                                                              dplyr::distinct(variable) %>%
                                                                              dplyr::pull(variable),
                                                                            lagged_feature_stat[[as.name(j)]],
                                                                            "lag",
                                                                            seq(1, ifelse(max_lag>1,max_lag-1,1), by = 1)) %>%
                                         dplyr::arrange(Var1, Var2, Var3 , Var4) %>%
                                         tidyr::unite(variable_names, Var1:Var4) %>%
                                         dplyr::pull(variable_names))
    }

    else{
      next
    }

  }


  # cat("The total number of features ",
  #     length(colnames(fixed_data))+
  #       length(c(all_variables_to_create,"length_of_stay"))+length(lag_variables_to_create)
  # )

  all_variables_to_create =  append(all_variables_to_create,expand.grid(temporal_data$variable %>% unique(),
                                                                        "slope",
                                                                        "0") %>%
                                      dplyr::arrange(Var1, Var2, Var3) %>%
                                      tidyr::unite(variable_names, Var1:Var3) %>%
                                      dplyr::pull(variable_names))
  if (lookback > 0){
    return(list(all_variables_to_create,lag_variables_to_create))
  }
  else{
    return(list(all_variables_to_create))
  }

}
