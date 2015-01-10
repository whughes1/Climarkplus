#' fit an autogregssive type model to a variable (probably temp)
#' 
#' @param wms  the raw data, a data set
#' @param var the variable to be fit
#' @param Four  the order of the Fourier fit
#' @param order  order of the autoregressive model               
#'
#' @details 
#' Fourier coefficients are calculated in terms of the DOY
#' up to order \code{Four}
#' 
#' @export

model_var_with_time=function(wms,var,Four_order=4,order=2){
 
  fit_string=paste(f_string(Four_order),collapse=" + ")
  fit_string=paste(var,"~",fit_string, "-1 ")
 
  fit_string=paste(fit_string,"+","Julian_Day")
  
  if(!("Julian_Day" %in% names(wms))){
    
       wms[,"Julian_Day"]= julian(as.Date(wms$Date))
  }
  
 
  fit=lm(fit_string,data=wms,na.action=na.exclude)
  
  
  
  auto_fit=NA
  if(order>0){
    wms[,"r_temp"]=resid(fit)
    fit_string=paste("r_temp","~")
    for(k in 1:order){
      markname=paste(var,"_sub_",k,sep="")
      fit_string=paste(fit_string,"+",markname)
    }
    autofit=lm(fit_string,data=wms,na.action=na.exclude)
  }
  
  list1=var
  list2=paste("autoregressive_order=",order,sep="")
  list3=paste("Fourier_order=",Four_order,sep="")
  
  cs =coef(fit)
  csn=names(cs)
  cs_four=cs[!(csn == "Julian_Day")]
  
  eval_string=cdot(cs_four,names(cs_four))
  DOY=1:366
  var_doy=eval(parse(text=eval_string))
  
  
  jul_big=-25567:47482  #1900-2100   
  eval_string=cdot(cs["Julian_Day"],"jul_big")
  var_julian = eval(parse(text=eval_string))
  
  
  list(var,auto_fit)
#   cs =coef(fit)
#   csn=names(cs)
#   cs_other=cs[csn %in% others]
#   cs_four=cs[!(csn %in% others)]
#   
#   eval_string=cdot(cs_four,names(cs_four))
#   DOY=1:366
#   var_doy=eval(parse(text=eval_string))
#   
#   
#   jul_big=-25567:47482
#   
#   
#    eval_string=cdot(cs_other["Julian_Day"],"jul_big")
#   
#   var_julian = eval(parse(text=eval_string))
#   
#   info_list=list(NULL,fit_string,wms,others,NULL,"end")
#   
  
  
 
}