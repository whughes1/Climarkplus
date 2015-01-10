plot_temp=function(wms,fit,tempvar="Tmin"){
  eval_string=cdot(coef(fit),names(coef(fit)))
  DOY=1:366
  Julian_Day=0
  t_fit=eval(parse(text=eval_string))
  plot(t_fit,ylim=c(10,30),type="l")
  wms_split_doy=split(wms[,tempvar],wms$DOY)
  avg_T=sapply(wms_split_doy,mean,na.rm=T)
  points(avg_T,col="green")
  

}