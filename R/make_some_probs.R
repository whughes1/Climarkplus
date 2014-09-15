#' make a file giving the probabilities of a rainy day
#' with lags up to oder
#' 
#' 
# @param data  A standard data set  
#' @param order  The order of the lags to calculate
#' @return A file with 366 rows and 2^(order+1) columns
#' giving the probability of rain given a certain lag,
#' and the number of times this lag shows up
#' @export

make_some_probs= function(data,order=2) {
  
  # set up names of 2^order levels
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  #probs are the markov probabilities
  #of wet or dry, weights is the amount of data
  #and rain stats as below
  probs=as.data.frame(matrix(nrow=366,ncol=length(levels)))
  weights=as.data.frame(matrix(nrow=366,ncol=length(levels)))
  
  names(probs)=sapply(levels,function(x) paste("P(w|",x,")",sep=""))
  names(weights)=sapply(levels,function(x) paste("#",x,sep=""))
  
  
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
  
 
  cbind(probs,weights)
  
  
  
}
