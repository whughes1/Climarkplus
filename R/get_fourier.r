#' Determine the number of harmonics needed to adaquately
#' model the probabilities associated with lag
#' 
#' @param probs  the markov wet/dry probabilities and the rain mean
#' @param first_fit_order  the first fit order used
#' @param lag  the lag to be fit
#' @return the order used to fit lag
#' @details  TBD
#' @export

get_fourier=function(probs, first_fit_order=1,lag="d"){
  
  colname=paste("P(w|",lag,")",sep="")
  
  
  #each of the lines is a fit of the raw probabilities
  
  fourier_order=fit_probs_inter(probs[,colname],
                          ws=probs[,paste("#",lag,sep="")],N=1,
                          start_order=first_fit_order,
                          y_label=colname)
  fourier_order
  
  }