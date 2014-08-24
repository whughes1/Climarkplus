#' Convert a data set to standard form
#' 
#' 
#' @param   Data  a dataset
#' @return A standard data set (every year has DOY 61 March 1)
#' @note  This function does little but add  DOY 60 to non_leap years
#' @export

convert_data=function(Data){
  
  #test will have only needed columns
  #(not DOY we will make a new one)
  
  test=cbind(Data$Station, as.character(Data$Date), Data$Rain)
  
  #detemine Feb 28 for all non leap years
  
  insert_dates= test[,2][yday(test[,2])==59 & !v_leap_year(test[,2])]
  
  
  #insert NA's for nonexistent Feb 29's
  tt=NULL
  nexti=1
  for(idate in insert_dates){
    lasti=which(test[,2]==idate)
    tt=rbind(tt,test[nexti:lasti,],c(test[1,1],NA,NA))
    nexti=lasti+1
  }
  lasti=nrow(test)
  tt=rbind(tt,test[nexti:lasti,])
  
  
  
  datat=as.data.frame(tt)    
  colnames(datat)=c("Station","Date","Rain") 
  datat$Rain=as.numeric(as.character(datat$Rain))
  
  # add daz of year, for every year Day 60
  # is Feb 29, add 1 to non leap years
  
  
  trow=yday(datat$Date)
  dd=trow>59 & !leap_year(year(datat$Date))
  dd[is.na(dd)]=FALSE
  trow[dd]=trow[dd]+1
  
  
  datat=cbind(datat,trow)
  colnames(datat)[4] = "DOY"
  
  datat
}
