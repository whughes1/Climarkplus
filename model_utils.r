# model uitlities

avg_daily_rainfall=function(dta,plot=FALSE) {
  data_day=split(dta$Rain,dta$DOY)
  avg_d_r=sapply(data_day,mean,na.rm=TRUE)
  if(plot ==TRUE){
    plot(avg_d_r,type="l")
  }
  avg_d_r
}

sd_daily_rainfall=function(dta,plot=FALSE) {
  data_day=split(dta$Rain,dta$DOY)
  sd_d_r=sapply(data_day,sd,na.rm=TRUE)
  if(plot ==TRUE){
    plot(sd_d_r,type="l")
  }
  sd_d_r
}

avg_yearly_rainfall=function(dta,plot=FALSE) {
  dta_wy=add_dmy(dta)
  data_year=split(dta_wy$Rain,dta_wy$mod_year)
  avg_y_r=sapply(data_year,mean,na.rm=TRUE)
  if(plot ==TRUE){
    plot(avg_y_r,type="l")
  }
  avg_y_r
}

sd_yearly_rainfall=function(dta,plot=FALSE) {
  dta_wy=add_dmy(dta)
  data_year=split(dta_wy$Rain,dta_wy$mod_year)
  sd_y_r=sapply(data_year,sd,na.rm=TRUE)
  if(plot ==TRUE){
    plot(sd_y_r,type="l")
  }
  sd_y_r
}
