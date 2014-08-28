fit_amounts=function(wms,filename=NULL){
  
  temp = make_ulags(wms,filename)
  wms["ULAGS"]= as.factor(temp)
  
  temp=wms[,"wet_or_dry"]
  temp[temp=="w"]=1
  temp[temp=="d"]=0
  wms["w_or_d"] = as.numeric(temp)
  
  ulags=levels(wms[,"ULAGS"])
  fit_string=make_fit_string(filename)
  
  fit=glm(fit_string,family="binomial",wms)
  
  fit
}