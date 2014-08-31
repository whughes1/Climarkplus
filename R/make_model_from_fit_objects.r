make_model_from_fit_objects=function(fit_object_rainy,fit_object_amount){
  fit1=fit_object_rainy[[2]]
  fit2=fit_object_amount[[2]]
  info1=fit_object_rainy[[1]]
  info2=fit_object_amount[[1]]
  params1=info1[[1]]
  params2=info2[[1]]
  
  #make the model
  info=rep(NA,366)
  mod=data.frame(info)
  
  
  #add the chance of rain cols
  
  order=params1["order"]
  
  levels=levs(as.numeric(order))
  
  for(lag in levels) {
    
    colname=paste("P(w|",lag,")",sep="")
    cs =coef(fit1)
    eval_string = make_eval_string(cs,params1[lag])
    
    DOY=1:366
    col_cont=inv.logit(eval(parse(text=eval_string)))
    
    mod[colname]=col_cont
  }
  
  # add the amount of rain columns
  
  order=params2["rain_order"]
  
  if(as.numeric(order) > 0){
    
    levels=levs(as.numeric(order))
    
    for(lag in levels) {
      
      colname=paste("<r|",lag,">",sep="")
      cs =coef(fit2)
      eval_string = make_eval_string(cs,params2[lag])
      
      DOY=1:366
      col_cont=(eval(parse(text=eval_string)))
      
      mod[colname]=1/col_cont
    }
  }else{
    colname="<rain>"
    cs = coef(fit2)
    eval_string = make_eval_string(cs,NULL,TRUE)
    DOY=1:366
    col_cont=(eval(parse(text=eval_string)))
    
    mod[colname]=1/col_cont
    
    
  }
  
  
  mod
}
