#' fit the points given by pts, by a fourier seriers
#' 
#' 
#' @param  pts  The points to be fitted
#' @param ws  optonal weights
#' @param order  The order of the fourier fit or "choose" for
#' a simple automatic choise
#' @param thresh  The amount the deviance of the fit must
#' decrease if the order is to be used
#' @param method  The method to be used in the fit.  Currently
#' if this is set to "bernoulli" bernoulli fitting is done,
#' otherwise least squares with the given weights  (bernoulli assumes
#' pts is a vector of probability of success and ws is the number
#' of attempts)
#' @return A list of two items, the fitted curve (without NA's)
#' and the R fit object
#' @details fit a fourer series of order \code{order} to pts.  If
#' \code{order=="choose"} then keep trying higher orders until the
#' drop in deviance is less than thresh
#' (note this was originally fit_probs)
#' @export
fit_fourier= function(pts,ws=NULL,order=2,thresh=5,method="standard")
    
{
  if(order ==  "choose")
  {
    choose_order(pts,ws,thresh=thresh,method=method)
  }
  else{
    
    order=as.numeric(order)
    if(is.null(ws)) ws=rep(1,366) 
    to_fit=matrix(nrow=366,ncol=2*order)
    
    #set up sin and cos for fourier fit
    for(j in 1:order){
      for(i in 1:366){
        to_fit[i,2*j-1]=cos(2*j*pi*i/366)
        to_fit[i,2*j]=sin(2*j*pi*i/366)
      }
    }
    
    
    #set up formula in form depending
    #on the order of the fit
    mod=NULL
    
    for(j in 1:order){
      mod[2*j-1]=paste("to_fit[,", as.character(2*j-1),"]")
      mod[2*j]=paste("to_fit[,", as.character(2*j),"]")
    }
    form=paste(mod,collapse=" + ")
    
    if( !(method == "bernoulli")){
      form=as.formula(paste("pts ~ ",form))
      sol=lm(form,weights=ws,na.action=na.exclude)
      #no_na does not contain NA values, fitted(sol) or predicted(sol) does
      no_na=cbind(rep(1,366),to_fit) %*% as.matrix(coefficients(sol))
    }
    else{
      b_mat=make_bernoulli_vector(pts,ws)
      form=as.formula(paste("b_mat ~ ",form))
      sol=glm(form,family=binomial,na.action=na.exclude)
      #no_na does not contain NA values, fitted(sol) or predicted(sol) does
      no_na=cbind(rep(1,366),to_fit)%*% as.matrix(coefficients(sol))
      no_na = 1/(1+exp(-no_na))   #need probabilities, not logit(probabilities)
    }

    retval=list(no_na,sol)
    retval
  }
  

 
  

  
}