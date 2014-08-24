#' Compare  curves to see
#' when you need a higher order model
#' 
#' 
#' @param probs all raw probabilities
#' @param first_lag  The first case to try
#' 
#' @return none, this produces plots
#' 
#' @details  You are asked for a lag (e.g d)
#' The function makes new lags by appending d and w
#' (eg. dd dw).  The three curves are plotted
#' 
#' @export
compare_lags=function(all_pbs,first_lag="d"){
  input="a"
  while(input!="") {
    # output query
    cat("\n\nenter\n\nlag      lag to use\nnothing   quit\n\n")
    
    #get info
    input=readLines(n=1)
    
    if(input == "")break
    
    #plot the fitted curve for
    #the given lag and the curves if the previous
    #day was wet or dry
    
    lag = input
    lag_d=paste(lag,"d",sep="")
    lag_w=paste(lag,"w",sep="")
    
    
    #check if the needed probabities are
    #available
    
    
    c_lag=paste("P(w|",lag,")",sep="")
    c_lag_d=paste("P(w|",lag_d,")",sep="")
    c_lag_w=paste("P(w|",lag_w,")",sep="")
    
    if(  (!c_lag %in% names(all_pbs))
        |(!c_lag_d %in% names(all_pbs))
        |(!c_lag_w %in% names(all_pbs))
    )
    {
      cat("\n\n*****\nlag not available in probability set\n*****")
      next
    }
    
    lagline=fit_probs(all_pbs[,c_lag],
                      ws=all_pbs[,paste("#",lag,sep="")],
                      order=4)[[1]]
    main_title=paste("Compare",c_lag,"to",c_lag_d, "and", c_lag_w)
    plot(lagline,type="l",col="blue",ylim=c(0,.8),
          main=main_title,xlab="day",ylab="probability")
    
    lagline_w=fit_probs(all_pbs[,c_lag_d],
                      ws=all_pbs[,paste("#",lag_d,sep="")],
                      order=4)[[1]]
    lines(lagline_w,col="red")
    
    lagline_d=fit_probs(all_pbs[,c_lag_w],
                        ws=all_pbs[,paste("#",lag_w,sep="")],
                        order=4)[[1]]
    lines(lagline_d,col="green")
    
    legend("topleft", c(c_lag,c_lag_d,c_lag_w),lty=c(1,1,1),
           col = c("blue","red","green"),cex=.8)
    
    
  }
    
    
}

