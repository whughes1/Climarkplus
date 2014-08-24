#' Add spell info to dataset
#' 
#' 
#' @param dataset  A standard dataset with \code{DOY} and \code{wet_or_dry} solums
#' @return A standard data set with columns \code{spell_length} and \code{First DOY}
#' @details A spell consists of consecutive days that are wet,dry or unkonwn.
#' Spells can cross year boundaries.  For each day, the length of the current
#' spell is in \code{spell_length}and the day it started is in \code{First DOY}.
#' @note  Leap years are handled correctly
#' @export

add_spell_info=function(dataset){
  #just go through the dataset, keeping track
  #of where the spell started (current_fist_index)
  #the doy of the first day of the spell (current_first DOY)
  #the spell length (current_spell_length)
  #and the spell type (current_spell)
  
  num_rows=nrow(dataset)
  first_doy=vector("numeric",num_rows)
  spell_length=vector("numeric",num_rows)
  current_first_doy=dataset$DOY[1]
  current_first_index=1
  current_spell=dataset$wet_or_dry[1]
  current_spell_length=1
  
  for(index in 2:num_rows){
    
    #skip DOY 60 in non_leap_years
    if(isTRUE(dataset$DOY[index-1]==59)){
      if(!isTRUE(v_leap_year(dataset$Date[index-1]))) next
    }
    if(!is.na(current_spell) && identical(current_spell,dataset$wet_or_dry[index])){
      current_spell_length=current_spell_length+1
    }else{
      first_doy[current_first_index:(index-1)]=current_first_doy
      spell_length[current_first_index:(index-1)]=current_spell_length
      current_first_doy=dataset$DOY[index]
      current_spell_length=1
      current_first_index=index
      current_spell=dataset$wet_or_dry[index]
    }
  }
  
  first_doy[current_first_index:num_rows]=current_first_doy
  spell_length[current_first_index:num_rows]=current_spell_length
  
  dataset$first_DOY=first_doy
  dataset$spell_length = spell_length
  
  dataset
  
}