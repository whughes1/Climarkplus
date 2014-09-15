#' Make a string of fourier functions
#
#' @param n  order of the string
#
#' @return A string of fourier functions of length n
#' @note  a bit specialized to the Climarkplus
#  package, but can be make more gerneral
#'




f_string= function(n){
  
    fstring = "cos(DOY*0*2*pi/366)"  # and odd way of writing 1
    
    if(n>0) {
      for(i in 1:n){
        temp_c=paste("cos(DOY*",i,"*2*pi/366)")
        temp_s=paste("sin(DOY*",i,"*2*pi/366)")
        fstring=c(fstring,temp_c,temp_s)
      }
    }
    
    fstring
    }


  