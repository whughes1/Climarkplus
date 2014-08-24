#' make a file giving the probabilities of a rainy day
#' for all lags up to max_rainy_day_order
#' and the mean and standard deviation of the rainfall
#' given that the day is rainy, both unconditional and
#' conditional on some rain pattern up to max_mean_rain_oder
#' 
#' 
#' @param  data   the raw probability data
#' @param max_rainy_day_order  All lags up to this order are calculated
#' to determine if the day is rainy
#' @param max_mean_rain_order  All lags up to this order are calculated to
#' determine the mean and standard deviation of the amount of rain or rainy days
#' @return A file with 366 rows and a number of columns giving
#' the probability of rain for a given lag, for each lag the number of days
#' on which the probability is based,
#' the mean and standard deviation of rainfall given that the day had rain,
#' again for a number of lags and for each lag 
#' number of rainy days
#' @details for the probability of a rainy day, take all lags up, to
#' length max_rainy_day_order, for the mean and std  of rain take all
#' lags up to max_mean_rain_order
#' @export

make_all_probs= function(data,max_rainy_day_order=2,max_mean_rain_order=1) {
  
  
  #probs  contains the unconditonal prbability
  #of rain and the unconditional mean and stanadard
  #deviation of rainy days.  We also need the number
  #of rainy days for weighting
  
  
  probs=as.data.frame(matrix(nrow=366,ncol=3))
  weights=as.data.frame(matrix(nrow=366,ncol=2))
                      
  
  
  names(probs)=c("P(w)", "<rain>", "sd(rain)")
  names(weights)=c("# days","# wet days")
  

  
  
  #days_with_rain[[i]] is a vector of the Rain on rainy days with 
  #DOY i, a bit messy as we need one variable (Rain) depending on another
  #variable (wet_or_dry) split by a factor, with no days missing.
                       
   
  
  data_split=split(data,data$DOY)
  days_with_rain=sapply(data_split,function(x) x$Rain[!is.na(x$wet_or_dry)
                                                      & x$wet_or_dry=="w"])
  #the overall probability of rain on day i is
  #the number of rainy days divided by the total
  #number of days
  
  all_wet_or_dry=split(data$wet_or_dry,data$DOY)
  num_wet_or_dry=sapply(all_wet_or_dry, function(x) sum(!is.na(x)))
  num_wet=sapply(all_wet_or_dry,function(x) sum( !is.na(x) & x=="w"))
  probs["P(w)"]= num_wet/num_wet_or_dry
  weights["# days"]=num_wet_or_dry
  
  
  
  weights["# wet days"]=num_wet
  probs["<rain>"]=sapply(days_with_rain,mean,na.rm=TRUE)
  probs["sd(rain)"]=sapply(days_with_rain,sd,na.rm=TRUE)

  tempout = cbind(probs,weights)
  
  #add the markov probs for wet or dry
  
  for(k in 1:max_rainy_day_order){
    tempcols=make_some_probs(data,order=k)
    tempout=cbind(tempout,tempcols)
    
  }
  
  #add markov ammounts a stadard dev of rain
  
  for(k in 1:max_mean_rain_order){
    tempcols=make_some_probs_rain(data,order=k)
    tempout=cbind(tempout,tempcols)
    
  }
  
  tempout
  
}

