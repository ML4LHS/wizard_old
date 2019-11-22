#' Slope
#'
#' Designing a function to compute the slope of the most recent window.
#'
#' @param time time
#' @param value value
#' @return slope value
#' @export

slope = function(time,value){

  if(all(is.na(time)) | all(is.na(value))){
    return( NA_real_)
  }
  else{
    return(stats::lm(time ~ value)$coefficients[2])
  }
}
