#' Add makov columns up to order to a standard data set 
#' 
#' 
#' @param data a standard data set
#' @param order calculate lags to this order
#' @param thresh  threshold of rain for a wet day
#' @return A standard data set with columns "wet_or_dry" and Lag_1 Lag_2 ...
#' Lag_order
#' @export

add_markov_values=function(data,varname="Tmax",order=2) {
  
  
  # for each lag, add a column
  
  
  
  for(n in 1: (order)){
    lagname=paste(varname,"_sub_",n,sep="")
    tvec=data[,varname]
    data[,lagname] <- c(rep(NA,n),tvec[1:(length(tvec)-n)])
  }

  data
}