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