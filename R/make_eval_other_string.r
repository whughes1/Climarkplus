#' Create a string that can be evalutaed to give
#' the offset from the "others" part of the fit
#' 
#' 
#'
#' @param cs the coeficients of "others" parts of the fit
#' @details  This function uses
#'           R string manipulation to produce a string that
#'           can be evaluated as R code
#' @export
make_eval_other_string=function(cs){
  
  
  csn=names(cs)
  eval_string=NULL
  
  #check is now redundant as only stuff that is used should
  #be passed here
  
  for(other_name in csn){
    other_coef = cs[other_name]
    eval_string = paste(eval_string,"+",cdot(other_coef,other_name))
    
  }
  
  eval_string
}