#' Determine the number of harmonics needed to adaquately
#' model the probabilities associated with lag
#' 
#' @param probs  the markov wet/dry probabilities and the rain mean
#' @param first_fit_order  the first fit order used
#' @param lag  the lag to be fit
#' @param is_rain  TRUE if the fit is to mean rain
#' @param the fitting method to use  
#' @return the order used to fit lag
#' @details  The function detemines the name of the column
#'    of data corresponding to the lag or rain lag.   This column
#'    is fitted by finite Fourier functions in the interactive
#'    function \code{fit_probs_inter}.
#' @export

get_fourier=function(probs, first_fit_order=1,lag="d",is_rain=FALSE
                     ,method="bernoulli",mask=NULL){
  
  
  if(is_rain){
    if(lag=="none"){
      colname="<rain>"
    }else{
      colname=paste("<(r|",lag,")>",sep="")    
    }
  }else{
    colname=paste("P(w|",lag,")",sep="")
  }
  
  
  #each of the lines is a fit of the raw probabilities
  #some question about what is the best smoothing parameter
  #for the raw data.  N=4 seems to be a good compromise
  #a better user inteface would allow this to change
  
  
  if(lag=="none"){
    ws=probs[,"# wet days"]
  } else{
    if(is_rain){
      ws=probs[,paste("#r",lag,sep="")]
    } else {
      ws=probs[,paste("#",lag,sep="")] 
    }
  }
  fourier_order=fit_probs_inter(probs[,colname],
                          ws=ws,N=4,
                          start_order=first_fit_order,
                          y_label=colname,method=method,
                          mask=mask)
  fourier_order
  
  }