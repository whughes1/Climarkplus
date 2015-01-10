
#' Make a model from fit objects
#' 
#' 
#' @param fit_object_rainy  a fit object that gives the probability
#'        that a day is rainy
#' @param fit_object_amount  A fit object that give the amount of rain
#'        on a rainy day
#' @param rainy_other_ds  An optional data set containing
#'                        a column for all other variables.
#'                        Each row should be a consecutive day
#' @param rainy_other_first_jul  The julian day corresponding
#'                                to the first row of rainy_other_ds
#' @param amounts_other_ds  An optional data set containing
#'                        a column for all other variables.
#'                        Each row should be a consecutive day
#' @param amounts_other_first_jul  The julian day corresponding
#'                                to the first row of rainy_other_ds

#' @return a model suitable as a parameter to synth_data_set_from_model
#' @details  The underlying functions use some r string maniplation
#' to produce strings which this function evaluates as R code
#' to compute the output of the fits.  Stuff dependent on DOY uses a 
#' vector 1:366.  Stuff dependent of julian day (others) can use the 
#' data, or optional data sets (which are assumed to cover the time
#' interval of interest.)
#' 
#' 
#' @export

make_model_from_fit_objects=function(fit_object_rainy,fit_object_amount,
                                     rainy_other_ds=NULL,
                                     rainy_other_first_jul=NULL,
                                     amounts_other_ds=NULL,
                                     amounts_other_first_jul=NULL){
  fit1=fit_object_rainy[[2]]
  fit2=fit_object_amount[[2]]
  info1=fit_object_rainy[[1]]
  info2=fit_object_amount[[1]]
  params1=info1[[1]]
  params2=info2[[1]]
  
  is_rainy_mask=info1[[5]]
  amt_rain_mask=info2[[5]]
  
  #make the model  (we add a temporary first column)
  temp=rep("",366)
  mod=data.frame(temp)
  
  #info will be a vector of character strings
  info=NULL
  
  
  #add the chance of rain cols
  
  order=params1["order"]
  
  info[1]=paste("<order> =",order)
  
  levels=levs(as.numeric(order))
  
  
  cs =coef(fit1)
  csn=names(cs)
  cs_other=cs[csn %in% info1[[4]]]
  cs_ulag=cs[!(csn %in% info1[[4]])]
  
  
  for(lag in levels) {
    
    colname=paste("P(w|",lag,")",sep="")
    eval_string = make_fourier_string(cs_ulag,params1[lag])
    
    #<KLUDGE ALERT
    # the proper way to do this would be to use NA or NAN
    #as a mask signal.  This would require changing and
    #perhaps slowing the simulator
    #Use the fact we know inverse logit of a large
    #negative number is very close to 0
    
    DOY=1:366
    col_cont=(eval(parse(text=eval_string)))
    
    col_cont[is_rainy_mask == 0] <- -1e50
    
    mod[colname]=col_cont
  }
  
  
  #now take care of the others for wet or dry
  #<KLUDGE ALERT> others are not masked
  #it is assumed the contribution will be negligable compared
  #to -1e50
  
  if(length(cs_other) == 0){
    other_wd_offset=NULL
  }
  else{
    eval_string=make_eval_other_string(cs_other)
    if(is.null(rainy_other_ds)){
      other_wd_offset=eval(parse(text=eval_string),envir=info1[[3]])
      info[4]==paste("<first_julian for rainy)> = ",
                     julian(as.Date(as.character(info1[[3]]$Date[1]))))
    }else{
      other_wd_offset=eval(parse(text=eval_string),envir=rainy_other_ds)
      info[4]==paste("<first_julian for rainy)> = ",
                     rainy_other_first_jul)
    }
  }
  
  
  # add the amount of rain columns
  
  order=params2["rain_order"]
  info[2]=paste("<rain_order> =",order) 
  shape= gamma.shape(fit2)
  info[3]=paste("<shape> =",shape[[1]])
  
  cs =coef(fit2)
  csn=names(cs)
  cs_other=cs[csn %in% info1[[4]]]
  cs_ulag=cs[!(csn %in% info1[[4]])]
  
  
  if(as.numeric(order) > 0){
    
    levels=levs(as.numeric(order))
    
    for(lag in levels) {
      
      colname=paste("<r|",lag,">",sep="")
      eval_string = make_fourier_string(cs_ulag,params2[paste("r",lag,sep="")])
      
      
      #<KLUDGE ALERT
      # the proper way to do this would be to use NA or NAN
      #as a mask signal.  This would require changing and
      #perhaps slowing the simulator
      #Use the fact we know inverse of a large
      #number is very close to 0
      
      DOY=1:366
      col_cont=(eval(parse(text=eval_string)))
      
      if(!is.null(amt_rain_mask)) col_cont[amt_rain_mask == 0] <- 1e50
      
      mod[colname]=col_cont
    }
  }else{
    colname="<rain>"
    eval_string = make_fourier_string(cs_ulag,NULL,is_rain=TRUE)
    DOY=1:366
    col_cont=(eval(parse(text=eval_string)))

    #<KLUDGE ALERT
    # the proper way to do this would be to use NA or NAN
    #as a mask signal.  This would require changing and
    #perhaps slowing the simulator
    #Use the fact we know inverse of a large
    #number is very close to 0
    
    if(!is.null(amt_rain_mask)) col_cont[is_rainy_mask == 0] <- 1e50
    
    mod[colname]=col_cont
    
    
  }
  
  #lose the first column of mod
  
  mod = mod[,2:ncol(mod)]
  
  #now take care of the others for amount of rain
  #<KLUDGE ALERT> others are not masked
  #it is assumed the contribution will be negligable compared
  #to 1e50
  
  if(length(cs_other)==0){
    other_rain_offset=NULL
  }
  else{
    eval_string=make_eval_other_string(cs_other)
    
    if(is.null(rainy_other_ds)){
      other_wd_offset=eval(parse(text=eval_string),envir=info2[[3]])
      info[5]==paste("<first_julian_for_amounts)> = ",
                     julian(as.Date(as.character(info2[[3]]$Date[1]))))
    }else{
      other_wd_offset=eval(parse(text=eval_string),envir=amounts_other_ds)
      info[5]==paste("<first_julian_for_amounts)> = ",
                     rainy_other_first_jul)
    }
    
    
  }
  

  
  mod_object=list(info,mod,other_wd_offset,other_rain_offset,
                  is_rainy_mask, amt_rain_mask)
  
  mod_object
}
