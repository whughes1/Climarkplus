#' Fitting probabilites to a fourier series of some order
#' determined by the drop in the deviance being above a threshold
#' 
#' 
#' @param probs probability of success 
#' @param ws total number of successes and failures
#' @param thresh  The deviance threshold
#' @param method  the fitting method used, bernoulli or std
#' 
#' @return A list consisting of the fitted probabilities,
#' (without NA's, the oder of the solution, and the solution)
#' 
#' @details  The fit order will be increases by one and the decrease
#' in deviance checked.  This procedure will continue until the decrease
#' in devience is less than the threshold.  This seems to work well
#' with the bernoulli method, but can be problematic with standard method.
#' @export


choose_order=function(probs,ws,thresh=5.0,method="bernoulli")
{
  old_devi=0
  old_sol=0
  new_sol=0
  devi=1e10
  order=0
  old_no_na=NULL
  no_na=NULL
  
  repeat{
    
    order=order+1
    old_devi=devi
    old_sol=new_sol
    old_no_na=no_na
    
    temp=fit_probs(probs,ws,order=order,method=method)
    no_na=temp[[1]]
    new_sol=temp[[2]]
    devi=deviance(new_sol)
    if( (old_devi-devi)<thresh )
    {
      break
    }
  }
   
  retval=list(old_no_na,order-1,old_sol)
  
  retval
}