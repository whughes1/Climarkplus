#' Make a string to pass to fitting code
#' 
#'
#' @param others The name of other files in the data that are 
#' included as simple (no interaction) parameters
#' @param other_model_string  You can put anything you want here
#' and it will be passed to the fit function.  Note, if this is not
#' NULL then you will
#' get a fit that cannot be used to make a model for the simulator 
#' 
#' @details  <TBD>
#' @export

make_fit_string_temp=function(others=NULL,other_model_string=NULL)  
{
  
  
  
  if(is.null(filename)){
    markov_string=NULL
    offsets=NULL
  } else {
    params=read_pl(filename)
    if(is_rain){
    order = as.numeric(params["rain_order"])
    }else 
    {
      order = as.numeric(params["order"]) 
    }
    numlags=2^order
    
    # set up names of 2^order levels
    
    if(order>0){
      levels=c("w","d")
      if(order>1){
        for(i in 1:(order-1)){
          levels=add_level(levels)
        }
      }
    }else{
      levels=NULL
    }
    
    
    
    markov_string=make_markov_string(params,levels,is_rain=is_rain)
    
    
    offsets=NULL
    
    for (lev in levels){
      arg= paste(lev,"_offset",sep="")
      if(is_rain) arg=paste("r",arg,sep="")
      if(params[arg]=="YES"){
        k=nchar(lev)
        temp=paste("Off",lev,sep="")
        offsets=paste(offsets,"+",temp)
      }
    }
  }
  
  other_string=paste(others,collapse=" + ")
  
  
  if(is_rain){
    fit_string=paste("Rain ~",markov_string,"-1")
  } else{
    fit_string=paste("w_or_d ~",markov_string,"-1")
  }
  
    
    
  if(!is.null(offset)) fit_string=paste(fit_string,offsets)
  if(!(other_string == "")) fit_string=paste(fit_string," +",other_string)
  if(!is.null(other_model_string)) fit_string=paste(fit_string," +",other_model_string)
  
  fit_string
}


  
  
  

  
