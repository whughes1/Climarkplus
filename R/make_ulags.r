make_ulags=function(wms,filename=NULL){
  
  params=read_pl(filename)
  order = as.numeric(params["order"])
  numlags=2^order
  
  # set up names of 2^order levels
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  ulags = vector(mode="character",length=nrow(wms))
  
  high_lag = paste("lag_",order,sep="")
  
  
  for (i in 1: nrow(wms)){
    ulags[i]=params[wms[i,high_lag]]
  }
  

  ulags

  
                  
  
}