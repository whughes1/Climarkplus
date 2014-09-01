make_param_file=function(data_set,max_order=3,max_rain_order=2,filename=NUll,all_pbs=NULL){
  
  
  data_set_doy=convert_data(data_set)
  data_wm=add_markov(data_set_doy,max_order)

  if(is.null(all_pbs)){
  all_pbs=make_all_probs(data_set,max_order,max_rain_order)
  }
  
  params=NULL
  
  #order=choose_order(dataset) 
  #rain_order=choose_rain_order(dataset)
  order=2
  
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
  params
}   