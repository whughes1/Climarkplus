

#' A utility ot fill a markov order model
#' 
#' @note obsolete
#'
get_mixed_models= function(max_w,max_d){
  m_mod=NULL
  
  order=max(max_w,max_d)
  m_mod$order=order
  
  # set up names of 2^order levels
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  for(i in 1:2^order)
    if(substring(levels[i],1,1)=="w"){
      m_mod[levels[i]] = substring(levels[i],1,max_w)
    } else{
      m_mod[levels[i]] = substring(levels[i],1,max_d)
    }
  
  
  m_mod   
     
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