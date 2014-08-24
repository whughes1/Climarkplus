#' Add columns with the Day, Month and "Year" to a standard
#' data set
#' 
#' 
#' @param dataset  a standard data set (may have extra columns, e.g. markov info)
#' @param year_change_in_july  usefull if growing season crosses years.
#' July 1970 will have "Year" 1971
#' @return A standard data set with added columns for day, month and mod_year
#' @export

add_dmy=function(dataset,year_change_in_july=FALSE){
  num_rows=nrow(dataset)
  year_vec=vector("numeric",num_rows)
  month_vec=vector("numeric",num_rows)
  day_vec=vector("numeric",num_rows)
  
  date_vec=as.character(dataset$Date)
  for(index in 1:num_rows){
    
#     if(isTRUE(dataset$DOY[index-1]==59)){
#       if(!isTRUE(v_leap_year(dataset$Date[index-1]))) next
#     }
    
  
#    year_vec[index] = year(dataset$Date[index]) 
    
    #this is much faster, but depends on Date being in
    #a specific format
    tempdate=strsplit(date_vec[index],"-")[[1]]
    year_vec[index] = as.numeric(tempdate[1])
    month_vec[index] = as.numeric(tempdate[2])
    day_vec[index] = as.numeric(tempdate[3])
    
    # mod_year should never be NA
    #(the usual reason for this is that 
    # there is no valid data, e.g. day of year 60 in
    # a non leap year.  Use the previous or next year)
    
    if (is.na(year_vec[index])){
      if(!(index==1)){
        year_vec[index]=year_vec[index-1]
      }else{
        tempdate=strsplit(date_vec[index+1],"-")[[1]]
        year_vec[index] = as.numeric(tempdate[1])
      }    
    }   
    #if the growing season spans dec-jan then
    #it is more convenient to have the year start in july
    if (year_change_in_july  && isTRUE(dataset$DOY[index] > 182) ) year_vec[index] = year_vec[index] + 1
  }
  dataset$mod_year =year_vec
  dataset$month =month_vec
  dataset$day =day_vec
  
  
  dataset
  
}