#' @export
fit_probs = function(probs,ws=NULL,order=2,thresh=5,method="standard")
    
{
  if(order ==  "choose")
  {
    choose_order(probs,ws,thresh=thresh,method=method)
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
      form=as.formula(paste("probs ~ ",form))
      sol=lm(form,weights=ws,na.action=na.exclude)
      #no_na does not contain NA values, fitted(sol) or predicted(sol) does
      no_na=cbind(rep(1,366),to_fit) %*% as.matrix(coefficients(sol))
    }
    else{
      b_mat=make_bernoulli_vector(probs,ws)
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