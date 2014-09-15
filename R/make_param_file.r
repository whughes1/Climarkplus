#'Given a data set help determine a suitable parameter file
#'for fitting the model.  In particular for each lag we see
#'if the (possibly shifted) curve from a previous lag can
#'be used and for each lag used we deternine the number of Fourier
#'harmonics needed to adaquately model it
#' 
#' @param data_set  (must have Markov lags)
#' @param max_order  Check all lags up to max_order for fitting
#'                   probability of rain
#' @param max_rain_order  As above but fitting the mean rain
#' @param filename   the parameter file to output to
#' @param all_pbs    This is calculated by the program but
#'                   it is not fast.   When dealing multiple
#'                   times with one data set it is worth
#'                   computing this once and keepint it.
#'@return A parameter list, the same list that would be
#'          obtained by reading the output parameter file.
#'@details For each lag we help determine                             
#'if the (possibly shifted) curve from a previous lag can
#'be used and for each lag used we deternine the number of Fourier
#'harmonics needed to adaquately model it.  The quantities are written
#'to a parameter file.
#'@export

make_param_file=function(data_set,max_order=3,max_rain_order=2,
                         filename=NULL,all_pbs=NULL){
  
  
  data_set_doy=convert_data(data_set)
  data_wm=add_markov(data_set_doy,max_order)

  if(is.null(all_pbs)){
  all_pbs=make_all_probs(data_wm,max_order,max_rain_order)
  }
  
  params=NULL
  
  order= max_order
  rain_order=max_rain_order

  
  levels=levs(order)
  
  
  #determine if any lags can be replaced by
  #shorter lags, perhaps with an offset
  
  for(lev in levels){
    
    lag_chose=compare_lags(all_pbs,search_lag=lev)
    params[lev]=lag_chose[[1]]
    params[paste(lev,"_offset",sep="")]=lag_chose[[2]]
  }
  
  
  #for each lag find the order of Fourier fit
  #needed
  done = NULL
  lev_orders=NULL
  for(lev in levels){
    if(params[lev] %in% done){
      Fourier_order=lev_orders[params[lev]]
      params[paste(lev,"_fit_order",sep="")]=Fourier_order
    } else {
      
      Fourier_order = get_fourier(all_pbs,1,params[lev])
      done = c(done,params[lev])
      lev_orders[params[lev]]=Fourier_order
      params[paste(lev,"_fit_order",sep="")]=Fourier_order
      
    }
  }
  
  
  params["order"]=order
  
  #fit the rain stuff
  
  order=rain_order
  
  levels = levs(order)
  for(lev in levels){
    
    lag_chose=compare_lags(all_pbs,search_lag=lev,is_rain=TRUE)
    params[paste("r",lev,sep="")]=lag_chose[[1]]
    params[paste("r",lev,"_offset",sep="")]=lag_chose[[2]]
  }
  
  
  #fit fourier coefs to the rain curves
  
  done = NULL
  lev_orders=NULL
  for(lev in levels){
    rlev=paste("r",lev,sep="")
    if(params[rlev]==""){
      plev="none"
    }else{
      plev=params[rlev]
    }
    if(plev %in% done){
      Fourier_order=lev_orders[plev]
      params[paste("r",lev,"_fit_order",sep="")]=Fourier_order
    } else {
      
      Fourier_order = get_fourier(all_pbs,1,plev,is_rain=TRUE,method="Standard")
      done = c(done,plev)
      lev_orders[plev]=Fourier_order
      params[paste("r",lev,"_fit_order",sep="")]=Fourier_order
      
    }
  }
  params[params==""]="0"
  
  params["rain_order"]=order
  
       
  

  
  
  if(!is.null(filename)){
    write_pl(params,filename)
  }else{
    params
  }
  
  
}   