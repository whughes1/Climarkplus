#' Convert a data set to standard form
#' 
#' 
#' @param   Data  a dataset
#' @return A standard data set (every year has DOY 61 March 1)
#' @details  This function selects some needed coluns and makes
#' a DOY column (every year has DOY 61 March 1)
#' @export

convert_data=function(Data){
  
  #test will have only needed columns
  #(not DOY we will make a new one)
  
  test=cbind(Data$Station, as.character(Data$Date), Data$Rain)

  
  
  datat=as.data.frame(test)    
  colnames(datat)=c("Station","Date","Rain") 
  datat$Rain=as.numeric(as.character(datat$Rain))
  
  # add day of year, for every year Day 61
  # is march  1, thus add 1 to non leap years
  
  
  trow=yday(datat$Date)
  dd=trow>59 & !leap_year(year(datat$Date))
  dd[is.na(dd)]=FALSE
  trow[dd]=trow[dd]+1
  
  
  datat=cbind(datat,trow)
  colnames(datat)[4] = "DOY"
  
  datat
}
