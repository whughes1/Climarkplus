#' fit a simple model to a variable (probably temp)
#' 
#' @param wms  the raw data, a data set
#' @param fit_var the variable to be fit
#' @param others  vector of Names of other rows of the data set that are
#'        to be used as predictors (defaults to Julian day)
#' @param other_model_string  Can be anything.  Added to the
#'        fitting formula verbatim.  Will probably mean the
#'        fit cannot be used for the simulator                
#' @param N  The order of the fourier fit  (default 4)
#' @details 
#' Fourier coefficients are calculated in terms of the DOY
#' up to order \code{order}
#' #' The parameter \code{others}
#' should be a vector of character strings.   It is used to add predictors
#' to the fit formula  (the default is the Julian Day).   The strings should be the names of columns in the
#' data set.   The parmeter \code{other_model_string} is used to experiment with
#' different types of models and is usually NULL
#' @export

fit_temp_simple=function(wms,fit_var,others="Julian_Day",other_model_string=NULL,order=4){
 
  
  
  start=0
  fit_string=paste(f_string(order),collapse=" + ")
  fit_string=paste(fit_var,"~",fit_string, "-1 ")
  if(!(others== "")) fit_string=paste(fit_string," +",others)
  if(!is.null(other_model_string)) fit_string=paste(fit_string," +",other_model_string)
  
  
  
  
  if(!("Julian_Day" %in% names(wms))){
    
       wms[,"Julian_Day"]= julian(wms$Date)
  }
  
 
  fit=lm(fit_string,data=wms)
  
  
  
  
  
  cs =coef(fit)
  csn=names(cs)
  cs_other=cs[csn %in% others]
  cs_four=cs[!(csn %in% others)]
  
  eval_string=cdot(cs_four,names(cs_four))
  DOY=1:366
  var_doy=eval(parse(text=eval_string))
  
  
  jul_big=-25567:47482
  
  
   eval_string=cdot(cs_other["Julian_Day"],"jul_big")
  
  var_julian = eval(parse(text=eval_string))
  
  info_list=list(NULL,fit_string,wms,others,NULL,"end")
  
  t_mod_object = list(info_list,) 
  
  t_fit_object
  
  
 
}