
#' Given a year of data, determine the first 
#' Day of the growing season
#' 
#' 
#' @param year_data  standard data for a single year  (must have wet_or_dry column)
#' @param n_total total amount of rainfall (see details)
#' @param over  over how many days(see details) 
#' @param span  length in which there is no dry spell(see details)
#' @param dry_length length of maximum dry spell(see details)
#' @param type 1 or 2 (see details)
#' @return The day or year of the start of the growing season.
#' @details  Two definitons are used, \code{type} 1 and 2:
#' 
#'   1: the first day of a rainy period in which there is \code{n_total}
#' millimetres of rain over \code{over} days
#' 
#'  2: the first day of a period in which there is \code{n_total}
#' millimetres of rain over \code{over} days and there is 
#' no dry spell of length {dry_spell} in the next \code{span} days
#
#' @note  The function is robust against \code{NA}'s, and
#' partial or mixed data but may give bad results
#' @export

fdgs=function(year_data,n_total=25,over=2,span=30, dry_length=6,type=1){
  num_rows=nrow(year_data)
  found=FALSE
  last_to_check = num_rows-1
  if(identical(type, 2)) last_to_check = last_to_check-dry_length
  for(index in 1:(num_rows-1)){    
    

    if(isTRUE(all(year_data$wet_or_dry[index:(index+over-1)] == "w"))){
      total = sum(year_data$Rain[index:(index+over-1)])
      if(total > n_total){
         if (identical(type,2)){
           last = index +2 + span-dry_length+1
           found=TRUE
           for(j in index:last){
             if(identical(year_data$wet_or_dry[j],"d") && 
                  year_data$spell_length[j]>=dry_length){
               found=FALSE
               break
             }
           }
         }
         else found = TRUE     
      }
    }
    if (isTRUE(found)) break
  }
  if(found==TRUE){
    fdgs_doy=year_data$DOY[index]
  }else{
    fdgs_doy=NA
  }

  fdgs_doy
}