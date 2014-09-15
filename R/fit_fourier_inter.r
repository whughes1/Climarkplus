#' fit the points given by pts, by a fourier seriers
#' Allow the user to see the fit given by
#' different orders and choose the order
#' 
#' 
#' @param  pts  The points to be fitted
#' @param ws  optonal 
#' @param start_order  The first order to be displayed
#' @param N  pts is smoothed by a rectangular window of length
#' N to aid in visual smoothing (the fit does not use the smoothed data)
#' @param  y_label  A label to put on the y axis
#' @param method  The method to be used in the fit.  Currently
#' if this is set to "bernoulli" bernoulli fitting is done,
#' otherwise least squares with the given weights   (bernoulli assumes
#' pts is a vector of probaility of success and ws is the number
#' of attempts)
#' @return A list of three items, the orderof the fit, the fitted curve
#'(without NA's) and the R fit object
#' @details fit a fourer series of order \code{start_order} to pts.
#' Display the fit with the raw data visually weighted (area of circles)
#' by ws.   Allow the user to choose higher or lower fit orders
#' or to accept the fit order given.  
#' @export
fit_probs_inter= function(pts,ws=NULL,start_order=1,N=8,y_label="",method="bernoulli")
{
  if(is.null(ws)) ws=rep(1,366) 
  
  order=start_order
  last_order=order
  input="e"
  devi=0
  
  while(input!="a") { 
    to_fit=matrix(nrow=366,ncol=2*order)
    for(j in 1:order){
      for(i in 1:366){
        to_fit[i,2*j-1]=cos(2*j*pi*i/366)
        to_fit[i,2*j]=sin(2*j*pi*i/366)
      }
    }
    mod=NULL
    
    for(j in 1:order){
      mod[2*j-1]=paste("to_fit[,", as.character(2*j-1),"]")
      mod[2*j]=paste("to_fit[,", as.character(2*j),"]")
    }
    form=paste(mod,collapse=" + ")
    if( !(method == "bernoulli")){
      form=as.formula(paste("pts ~ ",form))
      sol=lm(form,weights=ws,na.action=na.exclude)
      old_devi=devi
      devi=deviance(sol)
      #no_na does not contain NA values, fitted(sol) or predicted(sol) does
      no_na=cbind(rep(1,366),to_fit) %*% as.matrix(coefficients(sol))
    }
    else{
      b_mat=make_bernoulli_vector(pts,ws)
      form=as.formula(paste("b_mat ~ ",form))
      sol=glm(form,family=binomial,na.action=na.exclude)
      #no_na does not contain NA values, fitted(sol) or predicted(sol) does
      old_devi=devi
      devi=deviance(sol)
      no_na= cbind(rep(1,366),to_fit) %*% as.matrix(coefficients(sol))
      no_na = 1/(1+exp(-no_na))   #need x not logit(x)
    }
    
    #the points with be scaled by cirle radius
    #so this makes visual area compaison correct
    visual_weight=sqrt(ws)
  
    
    #only get rid of NA,s if we are filtering
    #the raw data (i.e. N > 1)
    #plot the raw data a circles, radius proportional
    #to the square root of the weight 
    
    main_title=paste(y_label,"fitted by Fourier series: order=",
          as.character(order))
    sub_title = paste("Delta Deviance ",as.character(round((devi-old_devi),1)),
                      " going from order ",
                       as.character(last_order),"to",as.character(order))
    if(!N==1){
      new_visual_weight=visual_weight
      new_visual_weight[is.na(pts)]=NA
      symbols(filter(clean_probs(pts),rep(1/N,N),circular=TRUE),
              circles=new_visual_weight,main=main_title,sub=sub_title,bg="red",
              ylab=y_label,xlab="Day of Year",inches=.06)
              
      
    }else{
      new_visual_weight=visual_weight
      new_visual_weight[is.na(probs)]=NA
      
      symbols(probs,circles=new_visual_weight,
              main=main_title,sub=sub_title,
              bg="red",ylab=y_label,xlab="Day of Year",inches=.06)
    }
    
    #plot the fitted curve as a line on top
    lines(no_na,col="blue")
    
    legend("bottomright", c("Raw Points","Fit"),
           lty = c(0, 1),pch=c(19,NA),col = c("red",
                                               "blue"),cex=.8)
    
    
    # output query
    cat("\n\nenter\n\na:  use this fit\np:  select previous order\nf:  add one to order\nb:  take one from order\nk:  set order to k")
    
    #get info
    input=readLines(n=1)
    
    if(identical(input,"p")){
      order=last_order
    }
    if(identical(input,"f")){
      last_order=order
      order=order+1
    }
    if(identical(input,"b")){
      last_order=order
      order=order-1
    }
    
    if(suppressWarnings(!is.na(as.numeric(input)))){
      last_order=order
      order = trunc(as.numeric(input))
    }
    if(order<1) order=1
    if(order>100) order=100
    
  }
  
  retval=list(order,no_na,sol)
  retval
  
}