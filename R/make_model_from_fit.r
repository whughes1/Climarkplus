make_model_from_fit=function(fit,filename=NULL,wms){
  params=read_pl(filename)
  coeffs=coef(fit)
  
  order = as.numeric(params["order"])
  
  #set up model
  info=vector(mode="character",length=366)
  mod=as.data.frame(info)
  
  # set up names of 2^order levels
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  for (lag in levels){
    colname=paste("(P|",lag,")",sep="")
    eval_string=make_eval_string(coeffs,params[lag])
    
    new_col=eval(parse(text=eval_string),envir=wms)
    
    new_col=inv.logit(new_col)
    
    mod[colname]=sapply(split(new_col,wms$DOY),mean,na.rm=TRUE)
  }
  
  mod
    
    
  
}
