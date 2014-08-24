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
  
  #clean up around Feb 29  (in non leap years the DOY preceding
  #61 is 59)
  
  for(i in 1:length(data$DOY)){
    if( identical(data$DOY[i],59) && ! v_leap_year(data$Date[i]) ){
      twd=c(wd[(i+1-order):(i)],wd[(i+2):(i+1+order)])
      twet_dry <- as.data.frame(matrix(nrow=length(twd),ncol=order+1))
      twet_dry[,1]=twd
      for(j in 2:(order+1)){
        twet_dry[,j] <- c(NA,twet_dry[1:(length(twd)-1),(j-1)])
        tlags=do.call("paste",c(as.list(twet_dry[,1:j]),sep=""))
        tlags[grep("NA",tlags)] <- NA
        lagname=paste("lag",as.character(j-1),sep="_")
        tlags=sapply(tlags,function(x) substring(x,2))
        data[(i+2):(i+1+order),lagname]=tlags[(order+1):(2*order)]
      }
    }
  }
  
  data
}