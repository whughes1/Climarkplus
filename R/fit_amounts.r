fit_amounts=function(wms,filename=NULL,others=NULL,other_model_string=NULL){
  
  temp = make_ulags(wms,filename)
  wms["ULAGS"]= as.factor(temp)
  
  #add offset factors
  
  params=read_pl(filename)
  order = as.numeric(params["order"])
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  for (lev in levels){
    arg= paste(lev,"_offset",sep="")
    if(params[arg]=="YES"){
      k=nchar(lev)
      colname=paste("lag_",k,sep="")
      new_colname=paste("Off",lev,sep="")
      wms[new_colname]=wms[,colname]==lev
      wms[,new_colname]=as.numeric(wms[,new_colname])
    }
  }
  
  
  temp=wms[,"wet_or_dry"]
  temp[temp=="w"]=1
  temp[temp=="d"]=0
  wms["w_or_d"] = as.numeric(temp)
  
  ulags=levels(wms[,"ULAGS"])
  fit_string=make_fit_string(filename,others=others,other_model_string=other_model_string)
  
  fit=glm(fit_string,family="binomial",wms)
  
  fit
}