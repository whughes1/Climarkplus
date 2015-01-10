# model uitlities

avg_daily_rainfall=function(dta,plot=FALSE,mask=NULL) {
  data_day=split(dta$Rain,dta$DOY)
  avg_d_r=sapply(data_day,mean,na.rm=TRUE)
  if(plot ==TRUE){
    plot(avg_d_r,type="l")
    if(!is.null(mask)) mask_plot(mask)
  }
  avg_d_r
}

sd_daily_rainfall=function(dta,plot=FALSE,mask=NULL) {
  data_day=split(dta$Rain,dta$DOY)
  sd_d_r=sapply(data_day,sd,na.rm=TRUE)
  if(plot ==TRUE){
    plot(sd_d_r,type="l")
    if(!is.null(mask)) mask_plot(mask)
  }
  sd_d_r
}

avg_yearly_rainfall=function(dta,plot=FALSE,mask=NULL) {
  dta_wy=add_dmy(dta)
  data_year=split(dta_wy$Rain,dta_wy$mod_year)
  avg_y_r=sapply(data_year,mean_mask,mask=mask,na.rm=TRUE)
  if(plot ==TRUE){
    plot(avg_y_r,type="l")
  }
  avg_y_r
}

sd_yearly_rainfall=function(dta,plot=FALSE,mask=NULL) {
  dta_wy=add_dmy(dta)
  data_year=split(dta_wy$Rain,dta_wy$mod_year)
  sd_y_r=sapply(data_year,sd_mask,mask=mask,na.rm=TRUE)
  if(plot ==TRUE){
    plot(sd_y_r,type="l")
  }
  sd_y_r
}

avg_day_rain_in_year=function(dta,plot=FALSE,mask=NULL) {
  if(is.null(mask)) mask=rep(1,366)
  dta_wy=add_dmy(dta)
  data_year=split(dta_wy,dta_wy$mod_year)
  avg_dr_y=sapply(data_year,
                  function(i){sum((mask[i$DOY] != 0) &
                                    (i$wet_or_dry == "w"),na.rm=T)})
  if(plot ==TRUE){
    plot(avg_dr_y,type="l")
  }
  avg_dr_y
}
