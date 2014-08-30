fit_amounts=function(wms,filename=NULL,others=NULL,other_model_string=NULL){
  
  temp = make_ulags(wms,filename,is_rain=TRUE)
  if(!is.null(temp)){
    wms["ULAGS"]= as.factor(temp)
  }
  
  #add offset factors
  
  params=read_pl(filename)
  order = as.numeric(params["rain_order"])
  if(order>1){
    levels=c("w","d")
    if(order>1){
      for(i in 1:(order-1)){
        levels=add_level(levels)
      }
    }
  }else{
    levels=NULL
  }
  
  
  for (lev in levels){
    arg= paste("r",lev,"_offset",sep="")
    if(params[arg]=="YES"){
      k=nchar(lev)
      colname=paste("lag_",k,sep="")
      new_colname=paste("Off",lev,sep="")
      wms[new_colname]=wms[,colname]==lev
      wms[,new_colname]=as.numeric(wms[,new_colname])
    }
  }
      
  fit_string=make_fit_string(filename,others=others,
                             other_model_string=other_model_string,
                             is_rain=TRUE)
  subdata<-subset(wms,wet_or_dry=="w")
  fit=glm(fit_string,family="Gamma",subdata)
  
  fit
}