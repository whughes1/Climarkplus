
#' A simple utility to make a vector of the
#' lag actually used for a given line 
#'
#' @param wms  a standard data set
#' @param filename  a parameter file
#' @param is_rain names slightly different if called
#' for rain lags
#' @return A vector of the lags actually used
#' @note  No error checking is done
#'

make_ulags=function(wms,filename=NULL,is_rain=FALSE){
  
  params=read_pl(filename)
  if(is_rain){
    order = as.numeric(params["rain_order"])
  } else{
    order = as.numeric(params["order"])
  }
  
  numlags=2^order
  
  # set up names of 2^order levels
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  if(order !=0){
    
    ulags = vector(mode="character",length=nrow(wms))
    
    high_lag = paste("lag_",order,sep="")
    
    
    for (i in 1: nrow(wms)){
      if(is_rain){
        ulags[i]=params[paste("r",wms[i,high_lag],sep="")]
      } else{
        ulags[i]=params[wms[i,high_lag]]
      }
    }
  } else{
    ulags = NULL
  }
  ulags  
}