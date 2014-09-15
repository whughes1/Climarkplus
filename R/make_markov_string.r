
#' A simple utility to make the fourier part
#' of the fit string
#'
#' @param params  a list of parameters
#' @param levels  the levels to include in the fit
#' @param is_rain names slightly different if called
#' for amount of rain fit
#' @return a part of the fit string
#'
make_markov_string = function(params,levels,is_rain=FALSE){
  markov_string=NULL
  done=NULL
  if(is.null(levels)) levels="0"  
  for(lev in levels){
    if(is_rain) lev=paste("r",lev,sep="")
    if(!params[lev] %in% done){
      if(lev=="0"  || lev=="r0" || params[lev]=="0"){
        uname=NULL
      }else{
        uname=paste("(ULAGS=='",params[lev],"')",sep="")
      }
      
      if(lev=="0"){
        fstring=f_string(as.numeric(params["r0_fit_order"]))
      }else{
        fstring=f_string(as.numeric(params[paste(lev,"_fit_order",sep="")]))
      }
      
    
      if(is.null(uname)){
        new_part=fstring
      }else{
        new_part=paste("I(",uname,"*",fstring,")",sep="")
      }
      markov_string=c(markov_string,new_part)
      
      if(!(lev=="0"))done=c(done,params[lev])
    }
  }
  
  
  markov_string=paste(markov_string,collapse=" + ")
  markov_string
  
  
}