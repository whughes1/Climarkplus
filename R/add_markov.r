#' Add makov columns up to order to a standard data set 
#' 
#' 
#' @inheritParams construct_model
#' @return A standard data set with columns "wet_or_dry" and Lag_1 Lag_2 ...
#' Lag_order
#' @export

add_markov=function(data,order=2,thresh=0.12) {
  
  #set up w or d vector
  
  wd=as.character(data$Rain)>thresh
  wd[wd==TRUE]="w"
  wd[wd==FALSE]="d"
  data$wet_or_dry = wd
  
  #wet dry will contain the lags in symbolic form
  #the first column will be the actual values
  
  wet_dry <- as.data.frame(matrix(nrow=length(wd),ncol=order+1))
  wet_dry[,1]=wd
  
  #each column or wet_dry is the previous
  #column shifted by 1
  #we add the collation of all columns
  #except the first to  data
  
  for(n in 2: (order+1)){
    wet_dry[,n] <- c(NA,wet_dry[1:(length(wd)-1),(n-1)])
    lags=do.call("paste",c(as.list(wet_dry[,1:n]),sep=""))
    lags[grep("NA",lags)] <- NA
    lagname=paste("lag",as.character(n-1),sep="_")
    data[lagname]=sapply(lags,function(x) substring(x,2))
  }
  
  

  data
}