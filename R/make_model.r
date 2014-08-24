#' Make a model by fitting probabilites
#' 
#' @param probs  the markov wet/dry probabilities and the rain mean/std
#' @inheritParams construct_model
#' @param fit_order  the degree of the fourier fit if non interactive, if
#' interactive the first fit order used
#' @return A markov model of order \code{order}, that can be used to synthesize
#' a data set
#' @details  The probabilities will normally be produced by taking the estimates
#' from the raw data.  A fourier fit is done, either with a fixed degree of fit,
#' or if \code{inter} is \code{TRUE} then the user can choose the degree of fit
#' for each fitted probability.  Fitting is robust in presence of \code{NA}'s,
#' however, the fitted probabities will not contain \code{NA}'s.
#' @export

make_model=function(probs,order=2,fit_order=4,inter=FALSE){
  
  
  ncols=1+2**(order)+2
  m_model=matrix(nrow=366,ncol=ncols)
  
  #the order of the model is placed
  #at the top of the info column
  m_model[1,1]=order
  
  for(i in 1:2**(order)){
    #each of the lines is a fit of the raw probabilities
    if(!inter){
      tempcol=fit_probs(probs[,i],ws=probs[,2^(order)+i],order=fit_order)
    }else{
      tempcol=fit_probs_inter(probs[,i],ws=probs[,2^(order)+i],N=1,
                              start_order=fit_order,
                              y_label=names(probs)[i])
    }
    m_model[,(i+1)]=tempcol
  }
  colnames(m_model)=c("info",names(probs[1:2**(order)]),"rain_mean","rain_sd")
  
  #fit the mean and standard deviation of rainfall on rainy days
  if(!inter){
    tempcol=fit_probs(probs[,"mean_rain_days"],ws=probs[,"num_rain_days"],
                      order=fit_order,method="std")
  }else{
    tempcol=fit_probs_inter(probs[,"mean_rain_days"],ws=probs[,"num_rain_days"],
                            N=1,start_order=fit_order,method="std") 
  }
  m_model[,"rain_mean"]=tempcol
  if (!inter){
    tempcol=fit_probs(probs[,"sd_rain_days"],ws=probs[,"num_rain_days"],
                      order=fit_order,method="std")
  }else{
    tempcol=fit_probs_inter(probs[,"sd_rain_days"],ws=probs[,"num_rain_days"],
                            N=1,start_order=fit_order,method="std")
  }
  m_model[,"rain_sd"]=tempcol
  
  m_model   
}