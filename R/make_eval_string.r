#' Create a string that can be evaluated to give the
#' output of the model
#' 
#' 
#'
#' @param cs the coefficients of the fit
#' @param lag the lag for which we want a value
#' @param is_rain   The function works slightly differently
#'        for fit_amounts
#' @details  This function and underlying functions use
#'           R string manipulation to produce strings that
#'           can be evaluated as R code              
#' @export
make_eval_string=function(cs,lag,is_rain=FALSE){
  
  if(is_rain && is.null(lag)){
    eval_string=cdot(cs,names(cs))
  } else{
    
    csn=names(cs)
    eval_string=NULL
    
    
    while (!grepl("ULAGS",csn[1])){
      other_coef= cs[1]
      other_name= csn[1]
      cs=tail(cs,-1)
      csn=tail(csn,-1)
      eval_string = paste(eval_string,"+",cdot(other_coef,other_name))
      
    }
    eval_string=paste(eval_string,"+",make_fourier_string(cs,lag))
  }
  eval_string
}