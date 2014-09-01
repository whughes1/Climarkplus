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
compare_lags=function(all_pbs,search_lag="dd"){
  input="a"
  first_round=TRUE
  while(input!="") {
    
    if(first_round){
      first_round=FALSE
      index=1
    }else{
      # output query
      cat("\n\nenter\n\nn      no\nb     go back one\ny     yes\no    yes with offset\n")
      
      #get info
      input=readLines(n=1)
      
      if(input == "q" || input == "y" || input == "o" )break
      
      if(input=="b"){  
        index=index-1
      } else {
        index = index + 1
        if(index > nchar(search_lag)) index = nchar(search_lag)
      }
      
      
      
    }
    
    lag=substring(search_lag,1,index)
    
    
    #check if the needed probabities are
    #available
    
    
    c_lag=paste("P(w|",lag,")",sep="")
    c_lag_search=paste("P(w|",search_lag,")",sep="")
    
    if(  (!c_lag %in% names(all_pbs)) )
    {
      cat("\n\n*****\nlag not available in probability set\n*****")
      next
    }
    
    lagline=fit_probs(all_pbs[,c_lag],
                      ws=all_pbs[,paste("#",lag,sep="")],
                      order=4)[[1]]
    main_title = paste ("Will ",lag," do for ",search_lag,"?",sep="")
    sub_title=paste("Compare",c_lag,"to",c_lag_search)
    plot(lagline,type="l",col="blue",ylim=c(0,.8),
          main=main_title,sub=sub_title,xlab="day",ylab="probability")
    
    lagline_search=fit_probs(all_pbs[,c_lag_search],
                      ws=all_pbs[,paste("#",search_lag,sep="")],
                      order=4)[[1]]
    lines(lagline_search,col="red")
    
    
    legend("topleft", c(c_lag,c_lag_search),lty=c(1,1),
           col = c("blue","red"),cex=.8)
    
    
  }
  
  if (input == "o"){
    ret_list = list(lag,"YES")
  }else{
    ret_list = list(lag,"NO")
  }
    
  ret_list  
}

