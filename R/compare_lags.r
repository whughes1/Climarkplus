#' Compare  curves to see
#' when you need a higher order model
#' 
#' 
#' @param probs all raw probabilities
#' @param search lag  Try all the prefixes of search
#'                    lag to see if any can be used (perhaps
#'                    with an offset)
#' @param is_rain     True if we are looking at mean rainfall
#' @param mask  a standard mask see \code{\link{mask_util}}
#' @return The lag to use and the offset
#' 
#' @details  For every prefix of the search lag (e.g search_lag = "ddw"
#' prefixes = "d". "dd", "ddw") fit the prefix and the search lag
#' to the data and compare the curves.  You can decide wether
#' or not to accept the lag, perhaps with an offset.
#' 
#' @export
compare_lags=function(all_pbs,search_lag="dd",is_rain=FALSE,
                      mask=NULL){
  input="a"
  first_round=TRUE
  while(input!="Q") {
    
    if(first_round){
      first_round=FALSE
      index=1
      if(is_rain) {index=0}
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
    
    
    #check if the needed probabilties are
    #available
    
    
    
    if(lag==""){
      c_lag="<rain>"
    }else{
      if(is_rain){
        c_lag=paste("<(r|",lag,")>",sep="") 
      } else{
        c_lag=paste("P(w|",lag,")",sep="")
      }
    }
    
    if(is_rain){
      c_lag_search=paste("<(r|",search_lag,")>",sep="") 
    } else{
      c_lag_search=paste("P(w|",search_lag,")",sep="")
    }
    
    if(  (!c_lag %in% names(all_pbs)) )
    {
      cat("\n\n*****\nlag not available in probability set\n*****")
      next
    }
    
    if(is_rain){
      if(lag==""){
           ws=all_pbs[,"# wet days"]
        }else{
           ws=all_pbs[,paste("#r",search_lag,sep="")]
        }
    }else{
      ws=all_pbs[,paste("#",search_lag,sep="")]
    }
    
    lagline=fit_probs(all_pbs[,c_lag],
                      ws=ws,
                      order=4,
                      mask)[[1]]
    
    if(lag==""){
      main_title = paste ("Will no lag do for ",search_lag,"?",sep="")      
    }else{
      main_title = paste ("Will ",lag," do for ",search_lag,"?",sep="")
    }
    sub_title=paste("Compare",c_lag,"to",c_lag_search)
    if(is_rain){
      ylim=c(5,15)
    }else{
      ylim=c(0,0.8)
    }
    plot(lagline,type="l",col="blue",ylim=ylim,
          main=main_title,sub=sub_title,xlab="day",ylab="probability")
    if(is_rain){
      ws=all_pbs[,paste("#r",search_lag,sep="")]
    }else{
      ws=all_pbs[,paste("#",search_lag,sep="")]
    }
    lagline_search=fit_probs(all_pbs[,c_lag_search],
                      ws=ws,
                      order=4,
                      mask=mask)[[1]]
    lines(lagline_search,col="red")
    
    
    legend("topleft", c(c_lag,c_lag_search),lty=c(1,1),
           col = c("blue","red"),cex=.8)
    
    
    if(!is.null(mask)) mask_plot(mask)  
  }
  
  if (input == "o"){
    ret_list = list(lag,"YES")
  }else{
    ret_list = list(lag,"NO")
  }
    
  ret_list  
}

