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
                               feature_stat = list(labs = c('min', 'mean', 'max'),
                                                   meds = ('min')
                               ),
                               recent_only = T,
                               lag_display = list("meds" = T,"labs" = T),
                               lag_compute = list("meds" = c("both"),
                                                  "labs" = c("prop")),
                               #lagged_feature_stat = list(labs = c('min', 'mean')),
                               
                               lookback = list("meds" = 7,"labs" = 48),
                               window_size = list("meds" = 1,"labs" = 6)){
  
  default_stats = c("min","max","mean","n")
  all_variables_to_create = c()
  lag_variables_to_create = c()
  
  for (j in temporal_data %>%
       distinct(category) %>%
       pull(1)){
    max_lag = lookback[[j]] %/% window_size[[j]]
    print(j)
    if(recent_only == F){
      cat("For:",j)
      cat("lookback",lookback[[j]])
      
      ## The condition checks if the user has given any feature stat for the categories else default statistics are used.
      
      if (length(feature_stat[[as.name(j)]])>0){
        # print(feature_stat[[as.name(i)]])
        all_variables_to_create = append(all_variables_to_create, expand.grid(temporal_data %>%
                                                                                dplyr::filter(category== j ) %>%
                                                                                dplyr::distinct(variable) %>%
                                                                                dplyr::pull(variable),
                                                                              feature_stat[[as.name(j)]],
                                                                              seq(0, ifelse(lookback[[j]] >0,lookback[[j]]-window_size[[j]],0), by = window_size[[j]])) %>%
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
                                                                              seq(0, ifelse(lookback[[j]] >0,lookback[[j]]-window_size[[j]],0), by = window_size[[j]])) %>%
                                           dplyr::arrange(Var1, Var2, Var3) %>%
                                           tidyr::unite(variable_names, Var1:Var3) %>%
                                           dplyr::pull(variable_names))
      }
    }
    else{
      all_variables_to_create = append(all_variables_to_create, expand.grid(temporal_data %>%
                                                                              dplyr::filter(category== j ) %>%
                                                                              dplyr::distinct(variable) %>%
                                                                              dplyr::pull(variable),
                                                                            feature_stat[[as.name(j)]],
                                                                            "0") %>%
                                         dplyr::arrange(Var1, Var2, Var3) %>%
                                         tidyr::unite(variable_names, Var1:Var3) %>%
                                         dplyr::pull(variable_names))
    }
    
    ## Generating the lag columns based on lag_display condition.
    
    if (length(feature_stat[[as.name(j)]])>0 & lookback[[j]] > 0 & !is.null(lag_compute[[j]])){
      print("Working on generating the lag  features for the analysis")
      if(lag_display[[j]] ){
        print("The category had lag display enabled")
      # print(feature_stat[[as.name(i)]])
      lag_variables_to_create = append(lag_variables_to_create, expand.grid(temporal_data %>%
                                                                              dplyr::filter(category == j ) %>%
                                                                              dplyr::distinct(variable) %>%
                                                                              dplyr::pull(variable),
                                                                            feature_stat[[as.name(j)]],
                                                                            ifelse(lag_compute[[j]] == "both",tibble("comp" = c("prop","diff")) %>% distinct(comp) ,lag_compute[[j]]) ,
                                                                            seq(1, ifelse(max_lag>1,max_lag-1,1), by = 1)) %>%
                                         tidyr::unnest() %>% 
                                         dplyr::arrange(Var1, Var2, Var3 , Var4) %>%
                                         tidyr::unite(variable_names, c(Var1,Var2,Var3,Var4)) %>%
                                         dplyr::pull(variable_names))
    }
    
    else{
      next
    }
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
  
  
  for (j in temporal_data %>%
       distinct(category) %>%
       pull(1)){
    
    if(!is.null(lag_compute[[j]])){
      
      lag_variables_to_create =  append(lag_variables_to_create,expand.grid(temporal_data$variable %>% unique(),
                                                                            "slope",
                                                                            ifelse(lag_compute[[as.name(j)]] == "both",tibble("comp" = c("prop","diff")) %>% distinct(comp) ,lag_compute[[as.name(j)]]),
                                                                            "0") %>%
                                          tidyr::unnest() %>% 
                                          dplyr::arrange(Var1, Var2, Var3, Var4) %>%
                                          tidyr::unite(variable_names, c(Var1,Var2,Var3,Var4)) %>%
                                          dplyr::pull(variable_names))
      
    }
  }
 
  # if (lookback[[j]] > 0){
  #   return(list(all_variables_to_create,lag_variables_to_create))
  # }
  # 
  # else{
  #   return(list(all_variables_to_create))
  # }
  return(list(all_variables_to_create %>% unique(),lag_variables_to_create %>% unique()))
  
}




