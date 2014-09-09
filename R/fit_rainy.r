#' fit an extended Markov model to the probability of rain
#' Use a parameter \code{.pl} file to guide the fitting
#' process
#' 
#' @param wms  the raw data, a data set
#' @param filename  The parameter file, if NULL the markov
#'        part of the fitting string will not be produced
#' @param others  vector of Names of other rows of the data set that are
#'        to be used as predictors (no interactions)
#' @param other_model_string  Can be anything.  Added to the
#'        fitting formula verbatim.  Will probably mean the
#'        fit cannot be used for the simulator                
#' 
#' @details The basic idea is to fit the Fourier coefficients of the
#' Markov probabilities to the raw data using logistic regression.
#' Which lags are used,
#' the fitting order, and whether an offset is applied to get the final curve
#' is determined by the values in the parameter file.  This produces a markov
#' model of chance of rain (year independent).  The parameter \code{others}
#' should be a vector or character strings.   It is used to add predictors
#' to the fit formula.   The strings should be the names of columns in the
#' data set.   The parmeter \code{other_model_string} is used to experiment with
#' different types of models and is usually NULL
#' @export

fit_rainy=function(wms,filename=NULL,others=NULL,other_model_string=NULL){
  
  temp = make_ulags(wms,filename)
  wms["ULAGS"]= as.factor(temp)
  
  #add offset factors
  
  params=read_pl(filename)
  order = as.numeric(params["order"])
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  for (lev in levels){
    arg= paste(lev,"_offset",sep="")
    if(params[arg]=="YES"){
      k=nchar(lev)
      colname=paste("lag_",k,sep="")
      new_colname=paste("Off",lev,sep="")
      wms[new_colname]=wms[,colname]==lev
      wms[,new_colname]=as.numeric(wms[,new_colname])
    }
  }
  
  
  temp=wms[,"wet_or_dry"]
  temp[temp=="w"]=1
  temp[temp=="d"]=0
  wms["w_or_d"] = as.numeric(temp)
  
  ulags=levels(wms[,"ULAGS"])
  fit_string=make_fit_string_new(filename,others=others,other_model_string=other_model_string)
  
  fit=glm(fit_string,family="binomial",wms)
  
  info_list=list(params,fit_string,wms,"end")
  fit_object = list(info_list,fit)
  
  fit_object
}