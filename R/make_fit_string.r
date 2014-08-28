#' Make a string to pass to fitting code
#' Use a parameter \code{.pl} file to guide the fitting
#' process
#' 
#' @param filename The name of the parameter file
#' @param others The name of other files in the data that are 
#' included as simple (no interaction) parameters
#' @param other_model_string  You can put anything you want here
#' and it will be passed to the fit function.  Note, if this is not
#' NULL then you will
#' get a fit that cannot be used to make a model for the simulator 
#' 
#' @details  <TBD>
#' @export

make_fit_string=function(filename=NULL,others=NULL,other_model_string=NULL)
  
{
  params=read_pl(filename)
  order = as.numeric(params["order"])
  numlags=2^order
  
  # set up names of 2^order levels
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
    
  markov_string=make_markov_string()
  
  offsets=NULL
  
  for (lev in levels){
    arg= paste(lev,"_offset",sep="")
    if(params[arg]=="YES"){
      k=nchar(lev)
      temp=paste("lag_",k,"=='",lev,"'",sep="")
      offsets=paste(offsets,"+",temp)
    }
  }
  
  other_string=paste(others,collapse=" + ")
  
  fit_string=paste("w_or_d ~",markov_string)
  if(!is.null(offsets)) fit_string=paste(markov_string,offsets)
  if(!(other_string == "")) fit_string=paste(fit_string,"+",other_string)
  if(!is.null(other_model_string)) fit_string=paste(fit_string,"+",other_model_string)
  
  fit_string
}


  
  
  

  
