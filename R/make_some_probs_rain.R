#' make a file giving the probabilities of a rainy day
#' and the mean and standard deviation of the rainfall
#' given that the day is rainy for each lag up to order
#' 
#' 
#' @inheritParams construct_model
#' @return A file with 366 rows and 2 +  3*(2^order)  columns giving
#' the mean and deviation of rainfall on rainy days and for each  
#' lag, the number of rainy days
#' @export

make_some_probs_rain= function(data,order=2) {
  
  # set up names of 2^order levels
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  #probs are the markov probabilities
  #of ammount and standard deviationof rain
  # on rainy days, weights is the amount of data
  
  means=as.data.frame(matrix(nrow=366,ncol=length(levels)))
  stdevs=as.data.frame(matrix(nrow=366,ncol=length(levels)))
  weights=as.data.frame(matrix(nrow=366,ncol=length(levels)))

  names(means)=sapply(levels,function(x) paste("<(r|",x,")>",sep=""))
  names(stdevs)=sapply(levels,function(x) paste("sd(r|",x,")",sep=""))
  names(weights)=sapply(levels,function(x) paste("#r",x,sep=""))

  #days with rain is a data set consisting of rainy days
  days_with_rain=data[!is.na(data$wet_or_dry) & data$wet_or_dry=="w",]
  
  #tempvec is the vector of wet days following lagname
  for(i in 1:366){
    for(j in 1:(length(levels))){
      lagname=paste("lag",as.character(order),sep="_") 
      tempvec=days_with_rain$Rain[days_with_rain[lagname]==levels[j]
                                  & days_with_rain$DOY==i]
      means[i,j]=mean(tempvec,na.rm=T)
      stdevs[i,j]=sd(tempvec,na.rm=T)
      
      weights[i,j]=sum(!is.na(tempvec))
    }
  }
  
 
  cbind(means,stdevs,weights)
  
  
  
}

