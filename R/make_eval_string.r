make_eval_string=function(cs,lag,is_rain=FALSE){
  
  if(is_rain && is.null(lag)){
    eval_string=cdot(cs,names(cs))
  } else{
    
    csn=names(cs)
    eval_string=NULL
    
    
    while (!grepl("ULAGS",csn[1])){
      other_coef= cs[1]
      other_name= csn[1]
      cs=tail(cs,-1)
      csn=tail(csn,-1)
      eval_string = paste(eval_string,"+",cdot(other_coef,other_name))
      
    }
    eval_string=paste(eval_string,"+",make_fourier_string(cs,lag))
  }
  eval_string
}