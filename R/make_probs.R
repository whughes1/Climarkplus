#' make a file giving the probabilities of a rainy day
#' and the mean and standard deviation of the rainfall
#' given that the day is rainy,  
#' 
#' 
#' 
#' @return A file with 366 rows and 2^(order+1)+3 columns giving
#' the probability of rain for a given lag, the number of days
#' on which the probability is based,
#' for mean rainfall, standard deviation of rainfall and 
#' number of rainy days
#' @note obsolete, use make_all_probs
#' @export

make_probs= function(data,order=2,thresh=.12) {
  
  # set up names of 2^order levels
  levels=c("w","d")
  for(i in 1:(order-1)){
    levels=add_level(levels)
  }
  
  #probs are the markov probabilities
  #of wet or dry, weights is the amount of data
  #and rain stats as below
  probs=as.data.frame(matrix(nrow=366,ncol=length(levels)))
  weights=as.data.frame(matrix(nrow=366,ncol=length(levels)))
  rain_stats=as.data.frame(matrix(nrow=366,ncol=3))
  
  names(probs)=sapply(levels,function(x) paste("P(w|",x,")",sep=""))
  names(weights)=sapply(levels,function(x) paste("#",x))
  
  names(rain_stats)=c("num_rain_days","mean_rain_days","sd_rain_days")
  
  
  
  #num_w is the number of wet days following lagname,
  #nun_w_or_d is the number of times something follows lagname
  for(i in 1:366){
    for(j in 1:(length(levels))){
      lagname=paste("lag",as.character(order),sep="_") 
      num_w=length(which(data[lagname]==levels[j] & data$DOY==i & data$wet_or_dry=="w"))
      num_w_or_d=length(which(data[lagname]==levels[j] & data$DOY==i))
      probs[i,j]=num_w/num_w_or_d
  
      if(is.nan(probs[i,j])) probs[i,j] = NA
      
      weights[i,j]=num_w_or_d
    }
  }
  
  #days_with_rain[[i]] is a vector of the Rain on rainy days with 
  #DOY i
  days_with_rain=sapply(split(data$Rain,data$DOY),function(x) {x[is.na(x)]=0;x[x>thresh]})
  
  rain_stats["num_rain_days"]=sapply(days_with_rain,length)
  rain_stats["mean_rain_days"]=sapply(days_with_rain,mean,na.rm=TRUE)
  
  rain_stats["sd_rain_days"]=sapply(days_with_rain,sd,na.rm=TRUE)
  
  cbind(probs,weights,rain_stats)
  
  
  
}

#take the out for clarity

add_level=function(list){
  ll=NULL
  for (k in list){
    temp=paste("w",k,sep="")
    ll=c(ll,temp)
    temp=paste("d",k,sep="")
    ll=c(ll,temp)
  }
  ll
}